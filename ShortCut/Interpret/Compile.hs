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
import ShortCut.Interpret.Parse (uExpr)

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

hashedTmp2 :: String -> ParsedExpr -> [FilePath] -> FilePath
hashedTmp2 extn expr paths = exprDir </> uniq <.> extn
  where
    uniq = digest $ unlines $ (show expr):paths

---------------------
-- dispatch on AST --
---------------------

cExpr :: TypedExpr -> Rules FilePath
cExpr e@(TypedExpr r (Reference _ _)) = cRef2 (ext r) (uExpr e)
cExpr e@(TypedExpr r (File        _)) = cLit2 (ext r) (uExpr e)
cExpr e@(TypedExpr r (Number      _)) = cLit2 (ext r) (uExpr e)
cExpr e@(TypedExpr r (LoadFNA     _)) = cLoad2 (ext r) (uExpr e)
cExpr e@(TypedExpr r (LoadFAA     _)) = cLoad2 (ext r) (uExpr e)
cExpr e@(TypedExpr r (LoadGenomes _)) = cLoad2 (ext r) (uExpr e)
cExpr e@(TypedExpr r (LoadGenes   _)) = cLoadGenes2 (ext r) (uExpr e)
cExpr e@(TypedExpr _ (Add      ts)) = cMath2 (+) "add"      (uExpr e) ts
cExpr e@(TypedExpr _ (Subtract ts)) = cMath2 (-) "subtract" (uExpr e) ts
cExpr e@(TypedExpr _ (Multiply ts)) = cMath2 (*) "multiply" (uExpr e) ts
cExpr e@(TypedExpr _ (Divide   ts)) = cMath2 (/) "divide"   (uExpr e) ts
cExpr e@(TypedExpr r (Union      ts)) = cSet2 union        "union"      (uExpr e) r ts
cExpr e@(TypedExpr r (Difference ts)) = cSet2 difference   "difference" (uExpr e) r ts
cExpr e@(TypedExpr r (Intersect  ts)) = cSet2 intersection "intersect"  (uExpr e) r ts
cExpr e@(TypedExpr _ t@(FilterGenes   _)) = cFilterGenes2   (uExpr e) t
cExpr e@(TypedExpr _ t@(FilterGenomes _)) = cFilterGenomes2 (uExpr e) t
cExpr e@(TypedExpr _ t@(WorstBest     _)) = cWorstBest2     (uExpr e) t

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
cLit2 :: String -> ParsedExpr -> Rules FilePath
cLit2 extn expr = do
  let path = hashedTmp2 extn expr []
  path %> \out -> do
    putQuiet $ unwords ["write", out]
    writeFileChanged out $ paths expr ++ "\n"
  return path
  where
    paths :: ParsedExpr -> String
    paths (Fil s) = s
    paths (Num n) = show n

----------------------------------------
-- compile everything symlink-related --
----------------------------------------

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
cRef2 :: String -> ParsedExpr -> Rules FilePath
cRef2 ext (Ref (VarName var)) = return $ namedTmp2 ext var

-- creates a symlink from expression file to input file
-- these should be the only absolute ones,
-- and the only ones that point outside the temp dir
cLoad2 :: String -> ParsedExpr -> Rules FilePath
cLoad2 extn expr = do
  path <- paths expr
  let link = hashedTmp2 extn expr []
  link %> \out -> do
    str    <- fmap strip $ readFile' path
    path'' <- liftIO $ canonicalizePath str
    putQuiet $ unwords ["link", str, out]
    quietly $ cmd "ln -fs" [path'', out]
  return link
  where
    paths :: ParsedExpr -> Rules FilePath
    paths (Cmd _ [Fil s]) = cExpr' RFile $ File s

-- TODO should what you've been calling load_genes actually be load_fna/faa?
-- TODO adapt to work with multiple files?
cLoadGenes2 :: String -> ParsedExpr -> Rules FilePath
cLoadGenes2 extn expr@(Cmd _ [Fil s]) = do
  path <- cExpr' RFile $ File s
  let fstmp = cacheDir </> "loadgenes" -- not actually used
      genes = hashedTmp2 extn expr []
  genes %> \out -> do
    need [path]
    path' <- readFile' path
    cmd "extract-seq-ids.py" fstmp out path'
  return genes

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
cMath2 :: (Scientific -> Scientific -> Scientific) -> String
      -> ParsedExpr -> (Typed Scientific, Typed Scientific) -> Rules FilePath
cMath2 fn fnName expr ts = do
  (p1, p2, p3) <- paths expr
  p3 %> \out -> do
    num1 <- fmap strip $ readFile' p1
    num2 <- fmap strip $ readFile' p2
    putQuiet $ unwords [fnName, p1, p2, p3]
    let num3 = fn (read num1 :: Scientific) (read num2 :: Scientific)
    writeFileChanged out $ show num3 ++ "\n"
  return p3
  where
    paths :: ParsedExpr -> Rules (FilePath, FilePath, FilePath)
    paths (Cmd _ [n1, n2]) = cBop2 expr (n1, n2) RNumber ts

-- apply a set operation to two sets (implemented as lists so far)
cSet2 :: (Set String -> Set String -> Set String) -> String
     -> ParsedExpr
     -> Returns [a] -> (Typed [a], Typed [a]) -- TODO remove
     -> Rules FilePath
cSet2 fn fnName pexpr retn texpr = do
  (p1, p2, p3) <- paths pexpr retn texpr
  p3 %> \out -> do
    lines1 <- readFileLines p1
    lines2 <- readFileLines p2
    putQuiet $ unwords [fnName, p1, p2, p3]
    let lines3 = fn (fromList lines1) (fromList lines2)
    writeFileLines out $ toList lines3
  return p3
  where
    paths :: ParsedExpr -> Returns [a] -> (Typed [a], Typed [a])
          -> Rules (FilePath, FilePath, FilePath)
    paths (Bop _ s1 s2) r ts = cBop2 pexpr (s1, s2) r ts

-- handles the actual rule generation for all binary operators
-- basically the `paths` functions with pattern matching factored out
cBop2 :: ParsedExpr -> (ParsedExpr, ParsedExpr)
      -> Returns a -> (Typed a, Typed a) -- TODO remove
      -> Rules (FilePath, FilePath, FilePath)
cBop2 expr (n1, n2) r (t1, t2) = do
  p1 <- cExpr' r t1
  p2 <- cExpr' r t2
  return (p1, p2, hashedTmp2 (ext r) expr [p1, p2])

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
cFilterGenes2 :: ParsedExpr -> Typed [Gene] -> Rules FilePath
cFilterGenes2 pexpr e@(FilterGenes (gens,goms,sci)) = do
  genes   <- cExpr' RGenes   gens
  genomes <- cExpr' RGenomes goms
  evalue  <- cExpr' RNumber  sci
  let hits   = hashedTmp2 "csv" pexpr [genes, genomes]
      faa    = hashedTmp2 "faa" pexpr [genes, "extractseqs"]
      genes' = hashedTmp RGenes e [hits, evalue]
      fgtmp  = cacheDir </> "fgtmp" -- TODO remove? not actually used
  -- TODO extract-seqs-by-id first, and pass that to filter_genes.R
  faa  %> extractSeqs genes
  hits %> bblast faa genomes
  genes' %> \out -> do
    need [genomes, hits, evalue]
    cmd "filter_genes.R" [fgtmp, out, genomes, hits, evalue]
  return genes'

-- TODO factor out bblast!
cFilterGenomes2 :: ParsedExpr -> Typed [Genome] -> Rules FilePath
cFilterGenomes2 pexpr e@(FilterGenomes (goms,gens,sci)) = do
  genomes <- cExpr' RGenomes goms
  genes   <- cExpr' RGenes   gens
  evalue  <- cExpr' RNumber  sci
  let hits     = hashedTmp2 "csv" pexpr [genomes, genes]
      faa      = hashedTmp2 "faa" pexpr [genes, "extractseqs"]
      genomes' = hashedTmp RGenomes e [hits, evalue]
      fgtmp = cacheDir </> "fgtmp" -- TODO remove? not actually used
  faa  %> extractSeqs genes
  hits %> bblast faa genomes
  genomes' %> \out -> do
    need [genes, hits, evalue]
    cmd "filter_genomes.R" [fgtmp, out, genes, hits, evalue]
  return genomes'

cWorstBest2 :: ParsedExpr -> Typed Scientific -> Rules FilePath
cWorstBest2 pexpr e@(WorstBest (gens,goms)) = do
  genes   <- cExpr' RGenes   gens
  genomes <- cExpr' RGenomes goms
  let faa    = hashedTmp2 "faa"  pexpr [genes, "extractseqs"]
      hits   = hashedTmp2 "csv"  pexpr [genomes, genes]
      evalue = hashedTmp RNumber e [genes, genomes]
      wbtmp  = cacheDir </> "wbtmp" -- TODO remove? not actually used
  faa  %> extractSeqs genes
  hits %> bblast faa genomes
  evalue %> \out -> do
    need [hits, genes]
    cmd "worst_best_evalue.R" [wbtmp, out, hits, genes]
  return evalue
