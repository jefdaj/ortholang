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

-- TODO why doesn't turning down the verbosity actually work?

module ShortCut.Core.Compile
  ( compileScript
  )
  where

import Development.Shake
import ShortCut.Core.Types

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

-- TODO remove before release!
-- import Debug.Trace

-------------------
-- name tmpfiles --
-------------------

-- TODO move all this stuff to utils or a new config module or something...

-- TODO deduplicate with the one in Compile.hs
--      (actually, load from config)
-- tmpDir :: CutConfig -> FilePath
-- tmpDir cfg = cfgTmpDir cfg

cacheDir :: CutConfig -> FilePath
cacheDir cfg = cfgTmpDir cfg </> "cache"

-- TODO what was this even for? remove it?
exprDir :: CutConfig -> FilePath
exprDir cfg = cacheDir cfg </> "shortcut"

-- Note that MD5 is no longer considered secure
-- But for our purposes (checking for updated files) it doesn't matter.
-- See https://en.wikipedia.org/wiki/MD5
digest :: (Show a) => a -> String
digest val = take 10 $ show (hash asBytes :: Digest MD5)
  where
    asBytes = (pack . show) val

-- TODO flip arguments for consistency with everything else
-- There's a kludge here for the special case of "result", which is like the
-- "main" function of a ShortCut script, and always goes to <tmpdir>/result.
namedTmp :: CutConfig -> CutVar -> CutExpr -> FilePath
namedTmp cfg (CutVar var) expr = cfgTmpDir cfg </> base
  where
    base  = if var == "result" then var else var <.> ext
    (CutType ext _) = typeOf expr

-- TODO extn can be found inside expr now; remove it
hashedTmp :: CutConfig -> CutExpr -> [FilePath] -> FilePath
hashedTmp cfg expr paths = exprDir cfg </> uniq <.> e
  where
    (CutType e _) = typeOf expr
    uniq = digest $ unlines $ (show expr):paths

-- overrides the expression's "natural" extension
-- TODO figure out how to remove!
hashedTmp' :: CutConfig -> CutType -> CutExpr -> [FilePath] -> FilePath
hashedTmp' cfg (CutType extn _) expr paths = exprDir cfg </> uniq <.> extn
  where
    uniq = digest $ unlines $ (show expr):paths
hashedTmp' _ _ _ _ = error "bad arguments to hashedTmp'"

---------------------
-- dispatch on AST --
---------------------

cExpr :: CutConfig -> CutExpr -> Rules FilePath
cExpr c e@(CutLit _ _) = cLit c e
cExpr c e@(CutRef _ _) = cRef c e
cExpr c e@(CutBop _ "+" _ _) = cMath c (+) "add"      e
cExpr c e@(CutBop _ "-" _ _) = cMath c (-) "subtract" e
cExpr c e@(CutBop _ "*" _ _) = cMath c (*) "multiply" e
cExpr c e@(CutBop _ "/" _ _) = cMath c (/) "divide"   e
cExpr c e@(CutBop _ "|" _ _) = cSet c union        "union"      e
cExpr c e@(CutBop _ "~" _ _) = cSet c difference   "difference" e
cExpr c e@(CutBop _ "&" _ _) = cSet c intersection "intersect"  e
cExpr c e@(CutFun _ "load_fasta_na" _) = cLoad      c e
cExpr c e@(CutFun _ "load_fasta_aa" _) = cLoad      c e
cExpr c e@(CutFun _ "load_genes"    _) = cLoadGenes c e
cExpr c e@(CutFun _ "load_genomes"  _) = cLoad      c e
cExpr c e@(CutFun _ "filter_genes"      _) = cFilterGenes   c e
cExpr c e@(CutFun _ "filter_genomes"    _) = cFilterGenomes c e
cExpr c e@(CutFun _ "worst_best_evalue" _) = cWorstBest     c e
cExpr _ _ = error "bad argument to cExpr"

cAssign :: CutConfig -> CutAssign -> Rules (CutVar, FilePath)
cAssign cfg (var, expr) = do
  -- liftIO $ putStrLn "entering cExpr"
  path  <- cExpr cfg expr
  -- liftIO $ putStrLn "got past cExpr!"
  path' <- cVar cfg var expr path
  return (var, path')

-- TODO how to fail if the var doesn't exist??
compileScript :: CutConfig -> CutScript -> Rules FilePath
compileScript cfg as = do
  -- liftIO $ putStrLn "entering compileScript"
  rpaths <- mapM (cAssign cfg) as
  return $ fromJust $ lookup (CutVar "result") rpaths

----------------------
-- compile literals --
----------------------

-- write a literal value from ShortCut source code to file
cLit :: CutConfig -> CutExpr -> Rules FilePath
cLit cfg expr = do
  -- liftIO $ putStrLn "entering cLit"
  let path = hashedTmp cfg expr []
  path %> \out -> do
    -- putQuiet $ unwords ["write", out]
    writeFileChanged out $ paths expr ++ "\n"
  return path
  where
    paths :: CutExpr -> String
    paths (CutLit _ s) = s
    paths _ = error "bad argument to paths"

----------------------------------------
-- compile everything symlink-related --
----------------------------------------

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
cRef :: CutConfig -> CutExpr -> Rules FilePath
cRef cfg expr@(CutRef _ var) = do
  -- liftIO $ putStrLn "entering cRef"
  return $ namedTmp cfg var expr
cRef _ _ = error "bad argument to cRef"

-- creates a symlink from expression file to input file
-- these should be the only absolute ones,
-- and the only ones that point outside the temp dir
cLoad :: CutConfig -> CutExpr -> Rules FilePath
cLoad cfg e@(CutFun _ _ [f]) = do
  -- liftIO $ putStrLn "entering cLoad"
  path <- cExpr cfg f
  let link = hashedTmp cfg e [path]
  link %> \out -> do
    str'   <- fmap strip $ readFile' path
    path'' <- liftIO $ canonicalizePath str'
    -- putQuiet $ unwords ["link", str', out]
    quietly $ cmd "ln -fs" [path'', out]
  return link
cLoad _ _ = error "bad argument to cLoad"

-- TODO should what you've been calling load_genes actually be load_fna/faa?
-- TODO adapt to work with multiple files?
cLoadGenes :: CutConfig -> CutExpr -> Rules FilePath
cLoadGenes cfg expr@(CutFun _ _ [f]) = do
  -- liftIO $ putStrLn "entering cLoadGenes"
  path <- cExpr cfg f
  let fstmp = cacheDir cfg </> "loadgenes" -- not actually used
      genes = hashedTmp cfg expr []
  genes %> \out -> do
    need [path]
    path' <- readFile' path
    quietly $ cmd "extract-seq-ids.py" fstmp out path'
  return genes
cLoadGenes _ _ = error "bad argument to cLoadGenes"

-- Creates a symlink from varname to expression file.
-- TODO how should this handle file extensions? just not have them?
-- TODO or pick up the extension of the destination?
cVar :: CutConfig -> CutVar -> CutExpr -> FilePath -> Rules FilePath
cVar cfg var expr dest = do
  -- liftIO $ putStrLn "entering cVar"
  let link  = namedTmp cfg var expr
      dest' = makeRelative (cfgTmpDir cfg) dest
  link %> \out -> do
    alwaysRerun
    need [dest]
    -- putQuiet $ unwords ["link", (cfgTmpDir cfg) </> dest', out]
    quietly $ cmd "ln -fs" [dest', out]
  return link

------------------------------
-- compile binary operators --
------------------------------

-- apply a math operation to two numbers
cMath :: CutConfig -> (Scientific -> Scientific -> Scientific) -> String
      -> CutExpr -> Rules FilePath
cMath cfg fn _ e@(CutBop extn _ n1 n2) = do
  -- liftIO $ putStrLn "entering cMath"
  (p1, p2, p3) <- cBop cfg extn e (n1, n2)
  p3 %> \out -> do
    num1 <- fmap strip $ readFile' p1
    num2 <- fmap strip $ readFile' p2
    -- putQuiet $ unwords [fnName, p1, p2, p3]
    let num3 = fn (read num1 :: Scientific) (read num2 :: Scientific)
    writeFileChanged out $ show num3 ++ "\n"
  return p3
cMath _ _ _ _ = error "bad argument to cMath"

-- apply a set operation to two sets (implemented as lists so far)
cSet :: CutConfig -> (Set String -> Set String -> Set String) -> String
     -> CutExpr -> Rules FilePath
cSet cfg fn _ e@(CutBop extn _ s1 s2) = do
  -- liftIO $ putStrLn "entering cSet"
  (p1, p2, p3) <- cBop cfg extn e (s1, s2)
  p3 %> \out -> do
    lines1 <- readFileLines p1
    lines2 <- readFileLines p2
    -- putQuiet $ unwords [fnName, p1, p2, p3]
    let lines3 = fn (fromList lines1) (fromList lines2)
    writeFileLines out $ toList lines3
  return p3
cSet _ _ _ _ = error "bad argument to cSet"

-- handles the actual rule generation for all binary operators
-- basically the `paths` functions with pattern matching factored out
cBop :: CutConfig -> CutType -> CutExpr -> (CutExpr, CutExpr)
      -> Rules (FilePath, FilePath, FilePath)
cBop cfg t expr (n1, n2) = do
  -- liftIO $ putStrLn "entering cBop"
  p1 <- cExpr cfg n1
  p2 <- cExpr cfg n2
  return (p1, p2, hashedTmp' cfg t expr [p1, p2])

---------------------
-- compile scripts --
---------------------

-- TODO does this need to distinguish FNA from FAA?
extractSeqs :: CmdResult b => CutConfig -> FilePath -> FilePath -> Action b
extractSeqs cfg genes out = do
  -- liftIO $ putStrLn "entering extractseqs"
  let estmp = cacheDir cfg </> "extractseqs"
  need [genes]
  quietly $ cmd "extract-seqs-by-id.py" estmp out genes

bblast :: CmdResult b => CutConfig -> FilePath -> FilePath -> FilePath -> Action b
bblast cfg genes genomes out = do
  -- liftIO $ putStrLn "entering bblast"
  let bbtmp = cacheDir cfg </> "bblast"
  need [genes, genomes]
  -- TODO fix bblast so order doesn't matter here
  -- TODO take a verbosity flag and pass the value on to bblast
  quietly $ cmd "bblast" "-o" out "-d" genomes "-f" genes "-c" "tblastn" "-t" bbtmp

-- TODO factor out bblast!
cFilterGenes :: CutConfig -> CutExpr -> Rules FilePath
cFilterGenes cfg e@(CutFun _ _ [gens, goms, sci]) = do
  -- liftIO $ putStrLn "entering cFilterGenes"
  genes   <- cExpr cfg gens
  genomes <- cExpr cfg goms
  evalue  <- cExpr cfg sci
  let hits   = hashedTmp' cfg csv e [genes, genomes]
      faa'   = hashedTmp' cfg faa e [genes, "extractseqs"]
      genes' = hashedTmp  cfg e [hits, evalue]
      fgtmp  = cacheDir cfg </> "fgtmp" -- TODO remove? not actually used
  -- TODO extract-seqs-by-id first, and pass that to filter_genes.R
  faa' %> extractSeqs cfg genes
  hits %> bblast cfg faa' genomes
  genes' %> \out -> do
    need [genomes, hits, evalue]
    quietly $ cmd "filter_genes.R" [fgtmp, out, genomes, hits, evalue]
  return genes'
cFilterGenes _ _ = error "bad argument to cFilterGenes"

-- TODO factor out bblast!
cFilterGenomes :: CutConfig -> CutExpr -> Rules FilePath
cFilterGenomes cfg e@(CutFun _ _ [goms, gens, sci]) = do
  -- liftIO $ putStrLn "entering cFilterGenomes"
  genomes <- cExpr cfg goms
  genes   <- cExpr cfg gens
  evalue  <- cExpr cfg sci
  let faa'     = hashedTmp' cfg faa e [genes, "extractseqs"]
      hits     = hashedTmp' cfg csv e [genomes, genes]
      genomes' = hashedTmp  cfg e [hits, evalue]
      fgtmp = cacheDir cfg </> "fgtmp" -- TODO remove? not actually used
  faa' %> extractSeqs cfg genes
  hits %> bblast cfg faa' genomes
  genomes' %> \out -> do
    need [genes, hits, evalue]
    quietly $ cmd "filter_genomes.R" [fgtmp, out, genes, hits, evalue]
  return genomes'
cFilterGenomes _ _ = error "bad argument to cFilterGenomes"

cWorstBest :: CutConfig -> CutExpr -> Rules FilePath
cWorstBest cfg e@(CutFun _ _ [gens, goms]) = do
  -- liftIO $ putStrLn "entering cWorstBest"
  genes   <- cExpr cfg gens
  genomes <- cExpr cfg goms
  let faa'   = hashedTmp' cfg faa e [genes, "extractseqs"]
      hits   = hashedTmp' cfg csv e [genomes, genes]
      evalue = hashedTmp  cfg e [genes, genomes]
      wbtmp  = cacheDir cfg </> "wbtmp" -- TODO remove? not actually used
  faa' %> extractSeqs cfg genes
  hits %> bblast cfg faa' genomes
  evalue %> \out -> do
    need [hits, genes]
    quietly $ cmd "worst_best_evalue.R" [wbtmp, out, hits, genes]
  return evalue
cWorstBest _ _ = error "bad argument to cWorstBest"
