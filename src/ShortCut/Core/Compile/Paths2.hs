-- TODO there are only a couple cases:
--
-- link from somewhere in tmpdir to a file
-- link from somewhere in the workdir (or absolute) to a file
-- link from var to another var (same dir)
--
-- all destinations are in the tmpdir
-- start with absolute expr paths (expr fn)
-- make them relative when linking or writing to a file (helper fn)
-- make them absolute again when reading (helper fn)
-- do it the haskell way this time: start repetitive, test, then DRY out

module ShortCut.Core.Compile.Paths2
  ( cacheDir
  , exprHash
  , pathHash
  , exprPath
  , exprPathExplicit
  , toTmpPath
  )
  where

-- import ShortCut.Core.Types2
import Path hiding ((</>)) -- (Path(..), Abs, Rel, File, Dir, parseAbsFile, parseAbsDir)
import ShortCut.Core.Types
-- import ShortCut.Core.Compile.Paths hiding (exprPrefix, exprPath, exprPathExplicit, cacheDir)

-- import Data.List                  (isInfixOf)
import ShortCut.Core.Debug (debugPath)
-- import System.FilePath (isPathSeparator, makeRelative)
-- import Text.PrettyPrint.HughesPJClass
import Data.List                  (intersperse)
import Data.Maybe                 (fromJust)
import Development.Shake.FilePath ((<.>), (</>), makeRelative)
import ShortCut.Core.Debug        (debugHash, debug)
import ShortCut.Core.Util         (digest, lookupVar)

-----------------------
-- new Paths-based paths --
-----------------------

-- TODO add phantom type to var + expr
-- TODO version that doesn't assume it exists?
-- lookupVar :: CutVar -> CutScript -> CutExpr
-- lookupVar var scr = fromJust $ lookup var scr

-- TODO replace with a variant on argHashes? maybe don't include in Paths3
exprHash :: CutState -> CutExpr -> String
exprHash s@(scr,_) (CutRef _ _ _ v) -- important not to include varnames themselves
  = exprHash s $ lookupVar v scr
exprHash s@(_, cfg) expr = res'
  where
    main = digest $ [pref, salt] ++ name -- most need one "main" hash
    res  = case expr of
             (CutLit _ _ _) -> concat subs -- single hash is enough
             _ -> concat $ intersperse "_" (main:subs)
    subs = argHashes s expr
    res' = debugHash cfg "exprHash" expr res
    pref = prefixOf expr
    salt = show $ saltOf expr
    name = case expr of -- TODO roll this into prefix?
             (CutBop _ _ _ n  _ _) -> [n]
             (CutFun _ _ _ n _   ) -> [n]
             _ -> []

-- TODO rename hPath?
-- TODO act differently when given a lit path?
pathHash :: CutConfig -> FilePath -> String
pathHash cfg = digest . makeRelative (cfgTmpDir cfg)

-- TODO use Paths here rather than hashes to make it map-compatible!
-- TODO and make them relative to the tmpdir for determinism
argHashes :: CutState -> CutExpr -> [String]
argHashes s@(_, cfg) expr = debug cfg ("argHashes for '" ++ show expr ++ "': " ++ show res) res
  where
    res = argHashes' s expr

-- TODO rename hSomething?
argHashes' :: CutState -> CutExpr -> [String]
argHashes' s@(as,_) (CutRef _ _ _ v) = argHashes s $ lookupVar v as
argHashes' _ (CutLit  _ _     v ) = [digest v]
argHashes' s@(_,cfg) (CutFun  _ _ _ _ es) = map (pathHash cfg . fromAbsFile . exprPath s) es
argHashes' s@(_,cfg) (CutBop  _ _ _ _ e1 e2) = map (pathHash cfg . fromAbsFile . exprPath s) [e1, e2]
argHashes' s@(_,cfg) (CutList _ _ _   es) = [digest $ concat $ map (pathHash cfg . fromAbsFile . exprPath s) es]

-- TODO rename... back to exprPath for now? and rewrite exprPathExplicit to match?
exprPath :: CutState -> CutExpr -> Path Abs File
exprPath s@(scr, _) (CutRef _ _ _ v) = exprPath s $ lookupVar v scr
exprPath s@(_, cfg) expr = debugPath cfg "exprPath" expr res
  where
    prefix = prefixOf expr
    rtype  = typeOf expr
    salt   = saltOf expr
    hashes = argHashes s expr
    res    = exprPathExplicit s prefix rtype salt hashes

-- TODO now we need the prefix to be unique, so "cut_bop" isn't good enough!
--      cut_bop -> union, difference, etc.
--      cut_lit should be OK, but maybe separate into num, str anyway?
exprPathExplicit :: CutState -> String -> CutType -> Int -> [String] -> Path Abs File
exprPathExplicit (_, cfg) prefix rtype salt hashes = fp
  where
    suf  = if salt == 0 then "" else "_" ++ show salt
    base = (concat $ intersperse "_" hashes) ++ suf
    path = cfgTmpDir cfg </> "exprs" </> prefix </> base <.> extOf rtype
    fp   = fromJust $ parseAbsFile path -- TODO remove and use Path everywhere

-- make an absolute expression path relative to the tmpdir
-- TODO can it work on the cache dir/files?
toTmpPath :: CutConfig -> Path Abs File -> Path Rel File
toTmpPath cfg path = fromJust $ parseRelFile relPath
  where
    relPath = makeRelative tmpDir absPath
    tmpDir  = cfgTmpDir cfg
    absPath = fromAbsFile path

-- TODO is this too complicated?
-- TODO rename: linkToExpr :: ... -> Action?
-- TODO switch src and dst?
-- exprToExpr :: CutState -> CutExpr -> CutExpr -> Path Expr Expr
-- exprToExpr s src dst = Path $ backToTmp </> fromTmp
--   where
--     (Path fromTmp) = exprPath s dst
--     (Path src')    = exprPath s src
--     nDirsBack      = length $ filter isPathSeparator src'
--     backToTmp      = foldr1 (</>) (take nDirsBack $ repeat "..")

-- varToExpr :: CutState -> CutExpr -> Path Var Expr
-- varToExpr s expr = Path $ ".." </> tmpPath
--  where
--     (Path tmpPath) = exprPath s expr

-- varToVar :: CutExpr -> String -> Path Var Var
-- varToVar expr name = Path $ name <.> extOf (typeOf expr)

-- resToVar :: CutExpr -> String -> Path Res Var
-- resToVar expr name = Path v2v
--   where
--     (Path v2v) = varToVar expr name

-- Makes the path relative to working dir if possible, and absolute otherwise.
-- exprToInput :: CutConfig -> CutExpr -> FilePath -> Path Expr Input
-- exprToInput cfg _ input = Path $ makeRelative (cfgWorkDir cfg) input

-- TODO change to tmpToCache? rootToCache?
-- TODO any better idea than fromJust here?
cacheDir :: CutConfig -> String -> Path Abs Dir
cacheDir cfg modName = fromJust $ parseAbsDir res
  where
    res :: FilePath
    res = cfgTmpDir cfg </> "cache" </> modName

-- Creates a unique hashed directory inside the main module cache dir.
-- Needed when scripts name their tmpfiles the same each time they're run
-- (I'm looking at you, crb-blast...)
-- cacheDirUniq2 :: CutState -> String -> CutExpr -> MyPath TmpDir CacheDir
-- cacheDirUniq2 s@(_, cfg) modName expr = MyPath $ mainCache </> hash
--   where
--     (MyPath mainCache) = cacheDir cfg modName
--     hash = exprHash s expr
