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

-- TODO move all this stuff to utils or a new config module or something...

-- TODO deduplicate with the one in Compile.hs
--      (actually, load from config)
tmpDir :: FilePath
tmpDir = "_shortcut"

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

-- TODO move back to Compile.hs or somewhere else? It's not a type!
-- TODO flip arguments for consistency with everything else
-- There's a kludge here for the special case of "result", which is like the
-- "main" function of a ShortCut script, and always goes to <tmpdir>/result.
namedTmp :: CutVar -> CutExpr -> FilePath
namedTmp (CutVar var) expr = tmpDir </> base
  where
    base  = if var == "result" then var else var <.> e
    (CutType e _) = typeOf expr

-- TODO extn can be found inside expr now; remove it
hashedTmp :: CutExpr -> [FilePath] -> FilePath
hashedTmp expr paths = exprDir </> uniq <.> e
  where
    (CutType e _) = typeOf expr
    uniq = digest $ unlines $ (show expr):paths

-- overrides the expression's "natural" extension
-- TODO figure out how to remove!
hashedTmp' :: CutType -> CutExpr -> [FilePath] -> FilePath
hashedTmp' (CutType extn _) expr paths = exprDir </> uniq <.> extn
  where
    uniq = digest $ unlines $ (show expr):paths
hashedTmp' _ _ _ = error "bad arguments to hashedTmp'"

---------------------
-- dispatch on AST --
---------------------

cExpr :: CutExpr -> Rules FilePath
cExpr e@(TStr _) = cLit e
cExpr e@(TNum _) = cLit e
cExpr e@(TRef _ _) = cRef e
cExpr e@(TBop _ "+" _ _) = cMath (+) "add"      e
cExpr e@(TBop _ "-" _ _) = cMath (-) "subtract" e
cExpr e@(TBop _ "*" _ _) = cMath (*) "multiply" e
cExpr e@(TBop _ "/" _ _) = cMath (/) "divide"   e
cExpr e@(TBop _ "|" _ _) = cSet union        "union"      e
cExpr e@(TBop _ "~" _ _) = cSet difference   "difference" e
cExpr e@(TBop _ "&" _ _) = cSet intersection "intersect"  e
cExpr e@(TCmd _ "load_fasta_na" _) = cLoad      e
cExpr e@(TCmd _ "load_fasta_aa" _) = cLoad      e
cExpr e@(TCmd _ "load_genes"    _) = cLoadGenes e
cExpr e@(TCmd _ "load_genomes"  _) = cLoad      e
cExpr e@(TCmd _ "filter_genes"      _) = cFilterGenes   e
cExpr e@(TCmd _ "filter_genomes"    _) = cFilterGenomes e
cExpr e@(TCmd _ "worst_best_evalue" _) = cWorstBest     e
cExpr _ = error "bad argument to cExpr"

cAssign :: CutAssign -> Rules (CutVar, FilePath)
cAssign (var, expr) = do
  -- liftIO $ putStrLn "entering cExpr"
  path  <- cExpr expr
  -- liftIO $ putStrLn "got past cExpr!"
  path' <- cVar var expr path
  return (var, path')

-- TODO how to fail if the var doesn't exist??
cScript' :: CutVar -> CutScript -> Rules FilePath
cScript' v as = do
  -- liftIO $ putStrLn "entering cScript"
  rpaths <- mapM cAssign as
  return $ fromJust $ lookup v rpaths

-- pretends to the rest of ShortCut that cScript' still works with GADTs
cScript :: CutVar -> CutScript -> Rules FilePath
cScript (CutVar v) s = cScript' (CutVar v) s

----------------------
-- compile literals --
----------------------

-- write a literal value from ShortCut source code to file
cLit :: CutExpr -> Rules FilePath
cLit expr = do
  -- liftIO $ putStrLn "entering cLit"
  let path = hashedTmp expr []
  path %> \out -> do
    -- putQuiet $ unwords ["write", out]
    writeFileChanged out $ paths expr ++ "\n"
  return path
  where
    paths :: CutExpr -> String
    paths (TStr s) = s
    paths (TNum n) = show n
    paths _ = error "bad argument to paths"

----------------------------------------
-- compile everything symlink-related --
----------------------------------------

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
cRef :: CutExpr -> Rules FilePath
cRef expr@(TRef _ var) = do
  -- liftIO $ putStrLn "entering cRef"
  return $ namedTmp var expr
cRef _ = error "bad argument to cRef"

-- creates a symlink from expression file to input file
-- these should be the only absolute ones,
-- and the only ones that point outside the temp dir
cLoad :: CutExpr -> Rules FilePath
cLoad e@(TCmd _ _ [f]) = do
  -- liftIO $ putStrLn "entering cLoad"
  path <- cExpr f
  let link = hashedTmp e [path]
  link %> \out -> do
    str'   <- fmap strip $ readFile' path
    path'' <- liftIO $ canonicalizePath str'
    -- putQuiet $ unwords ["link", str', out]
    quietly $ cmd "ln -fs" [path'', out]
  return link
cLoad _ = error "bad argument to cLoad"

-- TODO should what you've been calling load_genes actually be load_fna/faa?
-- TODO adapt to work with multiple files?
cLoadGenes :: CutExpr -> Rules FilePath
cLoadGenes expr@(TCmd _ _ [f]) = do
  -- liftIO $ putStrLn "entering cLoadGenes"
  path <- cExpr f
  let fstmp = cacheDir </> "loadgenes" -- not actually used
      genes = hashedTmp expr []
  genes %> \out -> do
    need [path]
    path' <- readFile' path
    quietly $ cmd "extract-seq-ids.py" fstmp out path'
  return genes
cLoadGenes _ = error "bad argument to cLoadGenes"

-- Creates a symlink from varname to expression file.
-- TODO how should this handle file extensions? just not have them?
-- TODO or pick up the extension of the destination?
cVar :: CutVar -> CutExpr -> FilePath -> Rules FilePath
cVar var expr dest = do
  -- liftIO $ putStrLn "entering cVar"
  let link  = namedTmp var expr
      dest' = makeRelative tmpDir dest
  link %> \out -> do
    alwaysRerun
    need [dest]
    -- putQuiet $ unwords ["link", tmpDir </> dest', out]
    quietly $ cmd "ln -fs" [dest', out]
  return link

------------------------------
-- compile binary operators --
------------------------------

-- apply a math operation to two numbers
cMath :: (Scientific -> Scientific -> Scientific) -> String
      -> CutExpr -> Rules FilePath
cMath fn _ e@(TBop extn _ n1 n2) = do
  -- liftIO $ putStrLn "entering cMath"
  (p1, p2, p3) <- cBop extn e (n1, n2)
  p3 %> \out -> do
    num1 <- fmap strip $ readFile' p1
    num2 <- fmap strip $ readFile' p2
    -- putQuiet $ unwords [fnName, p1, p2, p3]
    let num3 = fn (read num1 :: Scientific) (read num2 :: Scientific)
    writeFileChanged out $ show num3 ++ "\n"
  return p3
cMath _ _ _ = error "bad argument to cMath"

-- apply a set operation to two sets (implemented as lists so far)
cSet :: (Set String -> Set String -> Set String) -> String
     -> CutExpr -> Rules FilePath
cSet fn _ e@(TBop extn _ s1 s2) = do
  -- liftIO $ putStrLn "entering cSet"
  (p1, p2, p3) <- cBop extn e (s1, s2)
  p3 %> \out -> do
    lines1 <- readFileLines p1
    lines2 <- readFileLines p2
    -- putQuiet $ unwords [fnName, p1, p2, p3]
    let lines3 = fn (fromList lines1) (fromList lines2)
    writeFileLines out $ toList lines3
  return p3
cSet _ _ _ = error "bad argument to cSet"

-- handles the actual rule generation for all binary operators
-- basically the `paths` functions with pattern matching factored out
cBop :: CutType -> CutExpr -> (CutExpr, CutExpr)
      -> Rules (FilePath, FilePath, FilePath)
cBop t expr (n1, n2) = do
  -- liftIO $ putStrLn "entering cBop"
  p1 <- cExpr n1
  p2 <- cExpr n2
  return (p1, p2, hashedTmp' t expr [p1, p2])

---------------------
-- compile scripts --
---------------------

-- TODO does this need to distinguish FNA from FAA?
extractSeqs :: CmdResult b => FilePath -> FilePath -> Action b
extractSeqs genes out = do
  -- liftIO $ putStrLn "entering extractseqs"
  let estmp = cacheDir </> "extractseqs"
  need [genes]
  quietly $ cmd "extract-seqs-by-id.py" estmp out genes

bblast :: CmdResult b => FilePath -> FilePath -> FilePath -> Action b
bblast genes genomes out = do
  -- liftIO $ putStrLn "entering bblast"
  let bbtmp = cacheDir </> "bblast"
  need [genes, genomes]
  -- TODO fix bblast so order doesn't matter here
  -- TODO take a verbosity flag and pass the value on to bblast
  quietly $ cmd "bblast" "-o" out "-d" genomes "-f" genes "-c" "tblastn" "-t" bbtmp

-- TODO factor out bblast!
cFilterGenes :: CutExpr -> Rules FilePath
cFilterGenes e@(TCmd _ _ [gens, goms, sci]) = do
  -- liftIO $ putStrLn "entering cFilterGenes"
  genes   <- cExpr gens
  genomes <- cExpr goms
  evalue  <- cExpr sci
  let hits   = hashedTmp' csv e [genes, genomes]
      faa'   = hashedTmp' faa e [genes, "extractseqs"]
      genes' = hashedTmp e [hits, evalue]
      fgtmp  = cacheDir </> "fgtmp" -- TODO remove? not actually used
  -- TODO extract-seqs-by-id first, and pass that to filter_genes.R
  faa' %> extractSeqs genes
  hits %> bblast faa' genomes
  genes' %> \out -> do
    need [genomes, hits, evalue]
    quietly $ cmd "filter_genes.R" [fgtmp, out, genomes, hits, evalue]
  return genes'
cFilterGenes _ = error "bad argument to cFilterGenes"

-- TODO factor out bblast!
cFilterGenomes :: CutExpr -> Rules FilePath
cFilterGenomes e@(TCmd _ _ [goms, gens, sci]) = do
  -- liftIO $ putStrLn "entering cFilterGenomes"
  genomes <- cExpr goms
  genes   <- cExpr gens
  evalue  <- cExpr sci
  let faa'     = hashedTmp' faa e [genes, "extractseqs"]
      hits     = hashedTmp' csv e [genomes, genes]
      genomes' = hashedTmp e [hits, evalue]
      fgtmp = cacheDir </> "fgtmp" -- TODO remove? not actually used
  faa' %> extractSeqs genes
  hits %> bblast faa' genomes
  genomes' %> \out -> do
    need [genes, hits, evalue]
    quietly $ cmd "filter_genomes.R" [fgtmp, out, genes, hits, evalue]
  return genomes'
cFilterGenomes _ = error "bad argument to cFilterGenomes"

cWorstBest :: CutExpr -> Rules FilePath
cWorstBest e@(TCmd _ _ [gens, goms]) = do
  -- liftIO $ putStrLn "entering cWorstBest"
  genes   <- cExpr gens
  genomes <- cExpr goms
  let faa'   = hashedTmp' faa e [genes, "extractseqs"]
      hits   = hashedTmp' csv e [genomes, genes]
      evalue = hashedTmp e [genes, genomes]
      wbtmp  = cacheDir </> "wbtmp" -- TODO remove? not actually used
  faa' %> extractSeqs genes
  hits %> bblast faa' genomes
  evalue %> \out -> do
    need [hits, genes]
    quietly $ cmd "worst_best_evalue.R" [wbtmp, out, hits, genes]
  return evalue
cWorstBest _ = error "bad argument to cWorstBest"
