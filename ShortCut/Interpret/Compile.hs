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
import ShortCut.Interpret.Parse (uExpr, uAssign, uScript)

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

hashedTmp :: String -> ParsedExpr -> [FilePath] -> FilePath
hashedTmp extn expr paths = exprDir </> uniq <.> extn
  where
    uniq = digest $ unlines $ (show expr):paths

---------------------
-- dispatch on AST --
---------------------

cExpr :: ParsedExpr -> Rules FilePath
cExpr e@(Ref _) = cRef e
cExpr e@(Fil _) = cLit "txt" e
cExpr e@(Num _) = cLit "num" e
cExpr e@(Bop '+' _ _) = cMath (+) "add"      e
cExpr e@(Bop '-' _ _) = cMath (-) "subtract" e
cExpr e@(Bop '*' _ _) = cMath (*) "multiply" e
cExpr e@(Bop '/' _ _) = cMath (/) "divide"   e
cExpr e@(Bop '|' _ _) = cSet union        "union"      e
cExpr e@(Bop '~' _ _) = cSet difference   "difference" e
cExpr e@(Bop '&' _ _) = cSet intersection "intersect"  e
cExpr e@(Cmd "load_fasta_na" _) = cLoad      "fna"     e
cExpr e@(Cmd "load_fasta_aa" _) = cLoad      "faa"     e
cExpr e@(Cmd "load_genes"    _) = cLoadGenes "genes"   e
cExpr e@(Cmd "load_genomes"  _) = cLoad      "genomes" e
cExpr e@(Cmd "filter_genes"      _) = cFilterGenes   e
cExpr e@(Cmd "filter_genomes"    _) = cFilterGenomes e
cExpr e@(Cmd "worst_best_evalue" _) = cWorstBest     e

cAssign :: ParsedAssign -> Rules (ParsedVar, FilePath)
cAssign (v@(VarName var), expr) = do
  -- liftIO $ putStrLn "entering cExpr"
  path  <- cExpr expr
  -- liftIO $ putStrLn "got past cExpr!"
  path' <- cVar var path
  return (v, path')

-- TODO how to fail if the var doesn't exist??
cScript' :: ParsedVar -> ParsedScript -> Rules FilePath
cScript' v as = do
  -- liftIO $ putStrLn "entering cScript"
  rpaths <- mapM cAssign as
  return $ fromJust $ lookup v rpaths

-- pretends to the rest of ShortCut that cScript' still works with GADTs
cScript :: TypedVar -> TypedScript -> Rules FilePath
cScript (TypedVar v) s = cScript' (VarName v) (uScript s)

----------------------
-- compile literals --
----------------------

-- write a literal value from ShortCut source code to file
cLit :: String -> ParsedExpr -> Rules FilePath
cLit extn expr = do
  -- liftIO $ putStrLn "entering cLit"
  let path = hashedTmp extn expr []
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
cRef :: ParsedExpr -> Rules FilePath
cRef (Ref (VarName var)) = do
  -- liftIO $ putStrLn "entering cRef"
  return $ namedTmp2 "ref" var

-- creates a symlink from expression file to input file
-- these should be the only absolute ones,
-- and the only ones that point outside the temp dir
cLoad :: String -> ParsedExpr -> Rules FilePath
cLoad extn e@(Cmd _ [f]) = do
  -- liftIO $ putStrLn "entering cLoad"
  path <- cExpr f
  let link = hashedTmp extn e [path]
  link %> \out -> do
    str    <- fmap strip $ readFile' path
    path'' <- liftIO $ canonicalizePath str
    putQuiet $ unwords ["link", str, out]
    quietly $ cmd "ln -fs" [path'', out]
  return link

-- TODO should what you've been calling load_genes actually be load_fna/faa?
-- TODO adapt to work with multiple files?
cLoadGenes :: String -> ParsedExpr -> Rules FilePath
cLoadGenes extn expr@(Cmd _ [f]) = do
  -- liftIO $ putStrLn "entering cLoadGenes"
  path <- cExpr f
  let fstmp = cacheDir </> "loadgenes" -- not actually used
      genes = hashedTmp extn expr []
  genes %> \out -> do
    need [path]
    path' <- readFile' path
    cmd "extract-seq-ids.py" fstmp out path'
  return genes

-- Creates a symlink from varname to expression file.
cVar :: String -> FilePath -> Rules FilePath
cVar var dest = do
  -- liftIO $ putStrLn "entering cVar"
  let link  = namedTmp2 "var" var -- wonder if the freeze bug occurs here?
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
      -> ParsedExpr -> Rules FilePath
cMath fn fnName e@(Bop _ n1 n2) = do
  -- liftIO $ putStrLn "entering cMath"
  (p1, p2, p3) <- cBop "num" e (n1, n2)
  p3 %> \out -> do
    num1 <- fmap strip $ readFile' p1
    num2 <- fmap strip $ readFile' p2
    putQuiet $ unwords [fnName, p1, p2, p3]
    let num3 = fn (read num1 :: Scientific) (read num2 :: Scientific)
    writeFileChanged out $ show num3 ++ "\n"
  return p3

-- apply a set operation to two sets (implemented as lists so far)
cSet :: (Set String -> Set String -> Set String) -> String
     -> ParsedExpr -> Rules FilePath
cSet fn fnName e@(Bop _ s1 s2) = do
  -- liftIO $ putStrLn "entering cSet"
  (p1, p2, p3) <- cBop "set" e (s1, s2)
  p3 %> \out -> do
    lines1 <- readFileLines p1
    lines2 <- readFileLines p2
    putQuiet $ unwords [fnName, p1, p2, p3]
    let lines3 = fn (fromList lines1) (fromList lines2)
    writeFileLines out $ toList lines3
  return p3

-- handles the actual rule generation for all binary operators
-- basically the `paths` functions with pattern matching factored out
cBop :: String -> ParsedExpr -> (ParsedExpr, ParsedExpr)
      -> Rules (FilePath, FilePath, FilePath)
cBop extn expr (n1, n2) = do
  -- liftIO $ putStrLn "entering cBop"
  p1 <- cExpr n1
  p2 <- cExpr n2
  return (p1, p2, hashedTmp extn expr [p1, p2])

---------------------
-- compile scripts --
---------------------

-- TODO does this need to distinguish FNA from FAA?
extractSeqs :: CmdResult b => FilePath -> FilePath -> Action b
extractSeqs genes out = do
  -- liftIO $ putStrLn "entering extractseqs"
  let estmp = cacheDir </> "extractseqs"
  need [genes]
  cmd "extract-seqs-by-id.py" estmp out genes

bblast :: CmdResult b => FilePath -> FilePath -> FilePath -> Action b
bblast genes genomes out = do
  -- liftIO $ putStrLn "entering bblast"
  let bbtmp = cacheDir </> "bblast"
  need [genes, genomes]
  -- TODO fix bblast so order doesn't matter here
  -- TODO take a verbosity flag and pass the value on to bblast
  cmd "bblast" "-o" out "-d" genomes "-f" genes "-c" "tblastn" "-t" bbtmp

-- TODO factor out bblast!
cFilterGenes :: ParsedExpr -> Rules FilePath
cFilterGenes e@(Cmd _ [gens, goms, sci]) = do
  -- liftIO $ putStrLn "entering cFilterGenes"
  genes   <- cExpr gens
  genomes <- cExpr goms
  evalue  <- cExpr sci
  let hits   = hashedTmp "csv" e [genes, genomes]
      faa    = hashedTmp "faa" e [genes, "extractseqs"]
      genes' = hashedTmp (ext RGenes) e [hits, evalue]
      fgtmp  = cacheDir </> "fgtmp" -- TODO remove? not actually used
  -- TODO extract-seqs-by-id first, and pass that to filter_genes.R
  faa  %> extractSeqs genes
  hits %> bblast faa genomes
  genes' %> \out -> do
    need [genomes, hits, evalue]
    cmd "filter_genes.R" [fgtmp, out, genomes, hits, evalue]
  return genes'

-- TODO factor out bblast!
cFilterGenomes :: ParsedExpr -> Rules FilePath
cFilterGenomes e@(Cmd _ [goms, gens, sci]) = do
  -- liftIO $ putStrLn "entering cFilterGenomes"
  genomes <- cExpr goms
  genes   <- cExpr gens
  evalue  <- cExpr sci
  let hits     = hashedTmp "csv" e [genomes, genes]
      faa      = hashedTmp "faa" e [genes, "extractseqs"]
      genomes' = hashedTmp (ext RGenomes) e [hits, evalue]
      fgtmp = cacheDir </> "fgtmp" -- TODO remove? not actually used
  faa  %> extractSeqs genes
  hits %> bblast faa genomes
  genomes' %> \out -> do
    need [genes, hits, evalue]
    cmd "filter_genomes.R" [fgtmp, out, genes, hits, evalue]
  return genomes'

cWorstBest :: ParsedExpr -> Rules FilePath
cWorstBest e@(Cmd _ [gens, goms]) = do
  -- liftIO $ putStrLn "entering cWorstBest"
  genes   <- cExpr gens
  genomes <- cExpr goms
  let faa    = hashedTmp "faa"  e [genes, "extractseqs"]
      hits   = hashedTmp "csv"  e [genomes, genes]
      evalue = hashedTmp (ext RNumber) e [genes, genomes]
      wbtmp  = cacheDir </> "wbtmp" -- TODO remove? not actually used
  faa  %> extractSeqs genes
  hits %> bblast faa genomes
  evalue %> \out -> do
    need [hits, genes]
    cmd "worst_best_evalue.R" [wbtmp, out, hits, genes]
  return evalue
