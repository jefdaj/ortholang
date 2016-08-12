{-# LANGUAGE GADTs #-}

-- Once text has been parsed into an abstract syntax tree (Parse.hs), and that
-- tree has been checked for errors (Check.hs), this module "compiles" it by
-- translating it into a set of Shake build rules. To actually run the rules,
-- use `eval` in the Interpret module.

-- Note that there are some `error` calls in this module, but as far as I know
-- they can't ever lead to runtime errors. They're just here to satisfy GHC
-- until it can figure out how to pattern match on GADTs properly.
-- See: https://ghc.haskell.org/trac/ghc/wiki/PatternMatchCheck
-- I added more descriptive errors than `undefined` though, just in case.
-- TODO see if this is still a problem in GHC 8

-- TODO add more descriptive runtime error for canonicalizePath failing b/c no file
-- TODO see if you can avoid making more than one absolute symlink per input file
-- TODO make systematically sure there's only one rule for each file
-- TODO pass tmpDir as a config option somehow, and verbosity

-- TODO just insert IO debugging messages in the actions, duh!
-- TODO bring namedTmp back from Types.hs? It's not a type!

module ShortCut.Interpret.Compile where

import Development.Shake
import ShortCut.Types

import Crypto.Hash                (hash, Digest, MD5)
import Data.ByteString.Char8      (pack)
import Data.Maybe                 (fromJust)
import Data.Scientific            (Scientific)
import Data.Set                   (Set, union, difference, intersection
                                  ,fromList, toList)
import Data.String.Utils          (strip)
import Development.Shake.FilePath ((<.>), (</>))
import System.Directory           (canonicalizePath)
import System.FilePath            (makeRelative)

-------------------
-- name tmpfiles --
-------------------

-- tmpDir :: FilePath
-- tmpDir = "_shortcut"

cacheDir :: FilePath
cacheDir = tmpDir </> "cache"

exprDir :: FilePath
exprDir = cacheDir </> "shortcut"

-- Note that MD5 is no longer considered secure!
-- But for our purposes (checking for updated files) it doesn't matter.
-- See https://en.wikipedia.org/wiki/MD5
digest :: (Show a) => a -> String
digest val = take 10 $ show (hash asBytes :: Digest MD5)
  where
    asBytes = (pack . show) val

hashedTmp :: Returns a -> Typed a -> [FilePath] -> FilePath
hashedTmp retn expr paths = exprDir </> uniq <.> ext retn
  where
    uniq = digest $ unlines $ (show expr):paths

-- TODO unify this with hashedTmp, or at least name it better
hashedTmp' :: String -> Typed a -> [FilePath] -> FilePath
hashedTmp' extn expr paths = exprDir </> uniq <.> extn
  where
    uniq = digest $ unlines $ (show expr):paths

---------------------
-- dispatch on AST --
---------------------

cExpr :: TypedExpr -> Rules FilePath
cExpr (TypedExpr _ e@(Reference   _ _)) = cRef e
cExpr (TypedExpr r e@(File          _)) = cLit r e
cExpr (TypedExpr r e@(Number        _)) = cLit r e
cExpr (TypedExpr r e@(LoadFNA       _)) = cLoad r e
cExpr (TypedExpr r e@(LoadFAA       _)) = cLoad r e
cExpr (TypedExpr r e@(LoadGenes     _)) = cLoadGenes r e
cExpr (TypedExpr r e@(LoadGenomes   _)) = cLoad r e
cExpr (TypedExpr r e@(Add           _)) = cMath (+) "add"      r e
cExpr (TypedExpr r e@(Subtract      _)) = cMath (-) "subtract" r e
cExpr (TypedExpr r e@(Multiply      _)) = cMath (*) "multiply" r e
cExpr (TypedExpr r e@(Divide        _)) = cMath (/) "divide"   r e
cExpr (TypedExpr r e@(Union         _)) = cSet union        "union"      r e
cExpr (TypedExpr r e@(Difference    _)) = cSet difference   "difference" r e
cExpr (TypedExpr r e@(Intersect     _)) = cSet intersection "intersect"  r e
cExpr (TypedExpr _ e@(FilterGenes   _)) = cFilterGenes   e
cExpr (TypedExpr _ e@(FilterGenomes _)) = cFilterGenomes e
cExpr (TypedExpr _ e@(WorstBest     _)) = cWorstBest     e
-- cExpr (TypedExpr r e@(Complement      _ _)) = cSet ??? r e -- TODO remove?

-- same as above, but works on the unwrapped return type + expression
cExpr' :: Returns a -> Typed a -> Rules FilePath
cExpr' r e = cExpr $ TypedExpr r e

cAssign :: TypedAssign -> Rules (TypedVar, FilePath)
cAssign (var, expr@(TypedExpr rtn _)) = do
  path  <- cExpr expr
  path' <- cVar var rtn path
  return (var, path')

-- TODO how to fail if the var doesn't exist??
cScript :: TypedVar -> TypedScript -> Rules FilePath
cScript v as = do
  rpaths <- mapM cAssign as
  return $ fromJust $ lookup v rpaths

----------------------
-- compile literals --
----------------------

-- write a literal value from ShortCut source code to file
cLit :: Returns a -> Typed a -> Rules FilePath
cLit rtn expr = do
  let path = hashedTmp rtn expr []
  path %> \out -> do
    putQuiet $ unwords ["write", out]
    writeFileChanged out $ paths expr ++ "\n"
  return path
  where
    paths :: Typed a -> String
    paths (File   s) = s
    paths (Number n) = show n
    paths _ = error "oh no, programmer error in cLit!"

----------------------------------------
-- compile everything symlink-related --
----------------------------------------

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
cRef :: Typed a -> Rules FilePath
cRef (Reference rtn var) = return $ namedTmp rtn $ TypedVar var
cRef _ = error "oh no, programmer error in cRef!"

-- creates a symlink from expression file to input file
-- these should be the only absolute ones,
-- and the only ones that point outside the temp dir
cLoad :: Returns a -> Typed a -> Rules FilePath
cLoad retn path = do
  path' <- paths path
  let link = hashedTmp retn path []
  link %> \out -> do
    str    <- fmap strip $ readFile' path'
    path'' <- liftIO $ canonicalizePath str
    putQuiet $ unwords ["link", str, out]
    quietly $ cmd "ln -fs" [path'', out]
  return link
  where
    paths :: Typed a -> Rules FilePath
    paths (LoadFNA     s) = cExpr' RFile s
    paths (LoadFAA     s) = cExpr' RFile s
    paths (LoadGenomes s) = cExpr' RFile s
    paths _ = error "oh no, programmer error in cLoad!"

-- TODO should what you've been calling load_genes actually be load_fna/faa?
-- TODO adapt to work with multiple files?
cLoadGenes :: Returns [Gene] -> Typed [Gene] -> Rules FilePath
cLoadGenes retn e@(LoadGenes s) = do
  path <- cExpr' RFile s
  let fstmp = cacheDir </> "loadgenes" -- not actually used
      genes = hashedTmp retn e []
  genes %> \out -> do
    need [path]
    path' <- readFile' path
    cmd "extract-seq-ids.py" fstmp out path'
  return genes
cLoadGenes _ _ = error "oh no, programmer error in cLoadGenes!"

-- Creates a symlink from varname to expression file.
cVar :: TypedVar -> Returns a -> FilePath -> Rules FilePath
cVar var rtn dest = do
  let link  = namedTmp rtn var
      dest' = makeRelative tmpDir dest
  link %> \out -> do
    alwaysRerun
    need [dest]
    putQuiet $ unwords ["link", tmpDir </> dest', out]
    quietly $ cmd "ln -fs" [dest', out]
  return link

------------------------------
-- compile binary operators --
------------------------------

-- apply a math operation to two numbers
cMath :: (Scientific -> Scientific -> Scientific) -> String
      -> Returns Scientific -> Typed Scientific
      -> Rules FilePath
cMath fn fnName _ expr = do
  (p1, p2, p3) <- paths RNumber expr
  p3 %> \out -> do
    num1 <- fmap strip $ readFile' p1
    num2 <- fmap strip $ readFile' p2
    putQuiet $ unwords [fnName, p1, p2, p3]
    let num3 = fn (read num1 :: Scientific) (read num2 :: Scientific)
    writeFileChanged out $ show num3 ++ "\n"
  return p3
  where
    paths :: Returns a -> Typed a -> Rules (FilePath, FilePath, FilePath)
    paths r e@(Add      (n1,n2)) = cBop r e (n1,n2)
    paths r e@(Subtract (n1,n2)) = cBop r e (n1,n2)
    paths r e@(Multiply (n1,n2)) = cBop r e (n1,n2)
    paths r e@(Divide   (n1,n2)) = cBop r e (n1,n2)
    paths _ _ = error "oh no, programmer error in cMath!"

-- apply a set operation to two sets (implemented as lists so far)
cSet :: (Set String -> Set String -> Set String) -> String
     -> Returns [a] -> Typed [a]
     -> Rules FilePath
cSet fn fnName rtn expr = do
  (p1, p2, p3) <- paths rtn expr
  p3 %> \out -> do
    lines1 <- readFileLines p1
    lines2 <- readFileLines p2
    putQuiet $ unwords [fnName, p1, p2, p3]
    let lines3 = fn (fromList lines1) (fromList lines2)
    writeFileLines out $ toList lines3
  return p3
  where
    paths :: Returns [a] -> Typed [a] -> Rules (FilePath, FilePath, FilePath)
    paths r e@(Union      (s1,s2)) = cBop r e (s1,s2)
    paths r e@(Difference (s1,s2)) = cBop r e (s1,s2)
    paths r e@(Intersect  (s1,s2)) = cBop r e (s1,s2)
    paths _ _ = error "oh no, programmer error in cSet!"

-- handles the actual rule generation for all binary operators
-- basically the `paths` functions with pattern matching factored out
cBop :: Returns a -> Typed a -> (Typed a, Typed a)
     -> Rules (FilePath, FilePath, FilePath)
cBop r e (s1,s2) = do
  p1 <- cExpr' r s1
  p2 <- cExpr' r s2
  return (p1, p2, hashedTmp r e [p1, p2])

---------------------
-- compile scripts --
---------------------

-- TODO does this need to distinguish FNA from FAA?
extractSeqs :: CmdResult b => FilePath -> FilePath -> Action b
extractSeqs genes out = do
  let estmp = cacheDir </> "extractseqs"
  need [genes]
  cmd "extract-seqs-by-id.py" estmp out genes

bblast :: CmdResult b => FilePath -> FilePath -> FilePath -> Action b
bblast genes genomes out = do
  let bbtmp = cacheDir </> "bblast"
  need [genes, genomes]
  -- TODO fix bblast so order doesn't matter here
  -- TODO take a verbosity flag and pass the value on to bblast
  cmd "bblast" "-o" out "-d" genomes "-f" genes "-c" "tblastn" "-t" bbtmp

-- TODO factor out bblast!
cFilterGenes :: Typed [Gene] -> Rules FilePath
cFilterGenes e@(FilterGenes (gens,goms,sci)) = do
  genes   <- cExpr' RGenes   gens
  genomes <- cExpr' RGenomes goms
  evalue  <- cExpr' RNumber  sci
  let hits   = hashedTmp' "csv" e [genes, genomes]
      faa    = hashedTmp' "faa" e [genes, "extractseqs"]
      genes' = hashedTmp RGenes e [hits, evalue]
      fgtmp  = cacheDir </> "fgtmp" -- TODO remove? not actually used
  -- TODO extract-seqs-by-id first, and pass that to filter_genes.R
  faa  %> extractSeqs genes
  hits %> bblast faa genomes
  genes' %> \out -> do
    need [genomes, hits, evalue]
    cmd "filter_genes.R" [fgtmp, out, genomes, hits, evalue]
  return genes'
cFilterGenes _ = error "oh no, programmer error in cFilterGenes!"

-- TODO factor out bblast!
cFilterGenomes :: Typed [Genome] -> Rules FilePath
cFilterGenomes e@(FilterGenomes (goms,gens,sci)) = do
  genomes <- cExpr' RGenomes goms
  genes   <- cExpr' RGenes   gens
  evalue  <- cExpr' RNumber  sci
  let hits     = hashedTmp' "csv" e [genomes, genes]
      faa      = hashedTmp' "faa" e [genes, "extractseqs"]
      genomes' = hashedTmp RGenomes e [hits, evalue]
      fgtmp = cacheDir </> "fgtmp" -- TODO remove? not actually used
  faa  %> extractSeqs genes
  hits %> bblast faa genomes
  genomes' %> \out -> do
    need [genes, hits, evalue]
    cmd "filter_genomes.R" [fgtmp, out, genes, hits, evalue]
  return genomes'
cFilterGenomes _ = error "oh no, programmer error in cFilterGenomes!"

cWorstBest :: Typed Scientific -> Rules FilePath
cWorstBest e@(WorstBest (gens,goms)) = do
  genes   <- cExpr' RGenes   gens
  genomes <- cExpr' RGenomes goms
  let faa    = hashedTmp' "faa"  e [genes, "extractseqs"]
      hits   = hashedTmp' "csv"  e [genomes, genes]
      evalue = hashedTmp RNumber e [genes, genomes]
      wbtmp  = cacheDir </> "wbtmp" -- TODO remove? not actually used
  faa  %> extractSeqs genes
  hits %> bblast faa genomes
  evalue %> \out -> do
    need [hits, genes]
    cmd "worst_best_evalue.R" [wbtmp, out, hits, genes]
  return evalue
cWorstBest _ = error "oh no, programmer error in cWorstBest!"
