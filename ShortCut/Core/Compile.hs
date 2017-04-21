-- TODO once the modules are done, will this mostly be gone?
--      all except the basic refs, symlinks, etc. i guess

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
  , cBop
  , cLoad
  , hashedTmp
  , hashedTmp'
  , cExpr
  , cacheDir
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
import Development.Shake.FilePath ((<.>), (</>))
import System.Directory           (canonicalizePath)
import System.FilePath            (makeRelative)
import Data.String.Utils          (strip)

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

-- TODO remove or put in Types
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

-- TODO flip arguments for consistency with everything else There's a special
-- case for "result", which is like the "main" function of a ShortCut script,
-- and always goes to <tmpdir>/result.
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
    paths' = map (makeRelative $ cfgTmpDir cfg) paths
    uniq = digest $ unlines $ (show expr):paths'

-- overrides the expression's "natural" extension
-- TODO figure out how to remove!
hashedTmp' :: CutConfig -> CutType -> CutExpr -> [FilePath] -> FilePath
hashedTmp' cfg (CutType extn _) expr paths = exprDir cfg </> uniq <.> extn
  where
    paths' = map (makeRelative $ cfgTmpDir cfg) paths
    uniq = digest $ unlines $ (show expr):paths'
hashedTmp' _ _ _ _ = error "bad arguments to hashedTmp'"

---------------------
-- dispatch on AST --
---------------------

-- TODO after moving compile fns into modules, this kind of goes away right?
--      (it's trivial to call each fns own compiler)
-- TODO but you might want to keep all bops in this module?
--
-- Nah, just invert it: each function has one of these as the fCompiler,
-- then cExpr2 just calls those. Oh man, just need one case for CutFun, CutBop?
cExpr :: CutConfig -> CutExpr -> Rules FilePath
cExpr c e@(CutLit _ _) = cLit c e
cExpr c e@(CutRef _ _) = cRef c e
-- TODO these go in math
-- cExpr c e@(CutBop _ "+" _ _) = cMath c (+) "add"      e
-- cExpr c e@(CutBop _ "-" _ _) = cMath c (-) "subtract" e
-- cExpr c e@(CutBop _ "*" _ _) = cMath c (*) "multiply" e
-- cExpr c e@(CutBop _ "/" _ _) = cMath c (/) "divide"   e
-- TODO these go in sets
-- cExpr c e@(CutBop _ "|" _ _) = cSet c union        "union"      e
-- cExpr c e@(CutBop _ "~" _ _) = cSet c difference   "difference" e
-- cExpr c e@(CutBop _ "&" _ _) = cSet c intersection "intersect"  e
-- TODO these go in blast (for now)
-- cExpr c e@(CutFun _ "load_fasta_na" _) = cLoad      c e
-- cExpr c e@(CutFun _ "load_fasta_aa" _) = cLoad      c e
-- cExpr c e@(CutFun _ "load_genes"    _) = cLoadGenes c e
-- cExpr c e@(CutFun _ "load_genomes"  _) = cLoad      c e
-- cExpr c e@(CutFun _ "filter_genes"      _) = cFilterGenes   c e
-- cExpr c e@(CutFun _ "filter_genomes"    _) = cFilterGenomes c e
-- cExpr c e@(CutFun _ "worst_best_evalue" _) = cWorstBest     c e
-- TODO this goes away because invalid fns don't parse in the first place
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

-- TODO this should probably be exported for use in modules?
-- TODO typecheck here? expr has to be of type str (rtn type set by caller)
--      but wait, does Compile need to do any typechecking at all?
--      maybe we can assume the expr is a string here?
-- Pass a return type to make a function like the old cLoad
-- TODO wait a minute, is a return type even needed by compile time?
-- mkLoader :: CutType -> (CutConfig -> CutExpr -> Rules FilePath)
-- mkLoader rtn cfg rtn expr = do
--   -- TODO assert s is a str expression here, for extra safety if nothing else
--   path <- cExpr cfg expr
--   let link = hashedTmp cfg expr [path]
--   link %> \out -> do
--     str'   <- fmap strip $ readFile' path
--     path'' <- liftIO $ canonicalizePath str'
--     -- putQuiet $ unwords ["link", str', out]
--     quietly $ cmd "ln -fs" [path'', out]
--   return link

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

-- handles the actual rule generation for all binary operators
-- basically the `paths` functions with pattern matching factored out
cBop :: CutConfig -> CutType -> CutExpr -> (CutExpr, CutExpr)
      -> Rules (FilePath, FilePath, FilePath)
cBop cfg t expr (n1, n2) = do
  -- liftIO $ putStrLn "entering cBop"
  p1 <- cExpr cfg n1
  p2 <- cExpr cfg n2
  return (p1, p2, hashedTmp' cfg t expr [p1, p2])

-- TODO export cBop for use in modules. it's already ready??
--      looks good, like it has the type i came up with for cLoad2
