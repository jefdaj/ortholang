{-# LANGUAGE DeriveDataTypeable #-}

-- TODO wait it's much simpler:
-- 1. always deal in full paths when shake is involved
-- 2. always write paths relative to tmpdir and workdir
-- 3. convert back and forth with a couple functions
--    (these actually are mostly captured by the paths package?)
--    (no need for your own types; just use smart constructors/editors)

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

-- DO ONLY THIS FIRST IN ONE BIG PUSH (NO CHANGING PATHS/TESTS)
-- THEN NEXT PUSH IS TO GET THE PATHS INTO A NICE FORMAT (INDIV HASHES)
-- THEN MASSAGE RMAP* TO BE NICER (NO BREAKING IF POSSIBLE!)
-- THEN REASSESS/DOCUMENT + DEBUG BLAST, AFTER A BREAK DAY

{- This is a transitional module for the new phantom-typed paths;
 - once everything uses them I'll remove the other and rename this to Paths.
 -}

module ShortCut.Core.Compile.Paths2
  ( tmpToExpr
  , exprToExpr
  , varToExpr
  , varToVar
  , resToVar
  , exprToInput
  , cacheDir2
  , cacheDirUniq2
  , Path(..) -- TODO don't export constructor for safety?
  , TmpDir, Var, Res, Expr -- TODO Root?
  -- TODO don't export:
  -- , argHashes
  , exprHash
  )
  where

import ShortCut.Core.Types
-- import ShortCut.Core.Types2
import ShortCut.Core.Compile.Paths

-- import Data.List                  (isInfixOf)
import ShortCut.Core.Debug        (debugPath, debugHash)
import Data.Data                  (Data)
import Data.List                  (intersperse)
import Data.Maybe                 (fromJust)
import Development.Shake.FilePath ((<.>), (</>))
import ShortCut.Core.Util         (digest)
import System.FilePath (isPathSeparator, makeRelative)
-- import ShortCut.Core.Debug (debug)
-- import Text.PrettyPrint.HughesPJClass

-----------------------------
-- new phantom-typed paths --
-----------------------------

-- This doesn't guarantee much of anything on its own; needs smart constructors!
-- TODO hide it from being exported
-- (rtn put off until I have some time to add it)
-- newtype Path src dst rtn = Path FilePath deriving (Show, Data)
newtype Path src dst = Path FilePath deriving (Show, Data)

-- Possible source types:
-- TODO also hide from being exported, or no?
-- TODO Root?
data TmpDir   -- <tmpdir>
data Res      -- <tmpdir>/vars/result (TODO: salt dirs)
data Var      -- <tmpdir>/vars/<name>.<ext>
data Expr     -- <tmpdir>/exprs/<prefix>/<hash>.<ext>

-- Possible destination types are Res, Var, Expr, or:
data Input     -- any/path/the/user/feels.like
-- data CacheDir2 -- <tmpdir>/cache/<modulename>

-- Then the return type is either a phantom CutType, or:
-- (these are put off until I have some time to add them)
-- data Dir -- a dir rather than file (only used for cache dir so far)

{- This code:
 - let e = (CutExpr2 $ CutLit str 0 "stuff") :: CutExpr2 CutStr
 -     p = tmpToExpr [] e
 -
 - ... would create a "path from the tmpdir to an expression of type str":
 - p :: Path TmpDir Expr CutStr
 - p = Path "exprs/cut_lit/6f2d5f011a.str"
 -}

-----------------------------
-- hashes for use in paths --
-----------------------------

-- TODO add phantom type to var + expr
-- TODO version that doesn't assume it exists?
lookupVar :: CutVar -> CutScript -> CutExpr
lookupVar var scr = fromJust $ lookup var scr

-- TODO hash phantom type? so far we just ignore it. will determine ext anyway
-- TODO debug fn specifically for this?
exprHash :: CutState -> CutExpr -> String
exprHash s@(scr,_) (CutRef _ _ _ v) -- important not to include varnames themselves
  = exprHash s $ lookupVar v scr
exprHash (scr, cfg) expr = res'
  where
    main = digest $ [pref, salt] ++ name -- "main" hash
    res  = concat $ intersperse "_" (main:subs)
    subs = argHashes scr expr
    res' = debugHash cfg "exprHash" expr res
    pref = exprPrefix expr
    salt = show $ saltOf expr
    -- deps = map (\(Path p) -> p)
         -- $ map (tmpToExpr s)
         -- $ map (\v -> lookupVar v s)
         -- $ depsOf expr
    name = case expr of
             (CutBop _ _ _ n  _ _) -> [n]
             (CutFun _ _ _ n _   ) -> [n]
             _ -> []
    -- TODO use the subs' *paths* rather than exprs (call tmpToExpr)
    --      (what about when it's a ref... look up like above first?)
    -- TODO and factor out into a fn that can be reused when mapping
--     subs = map (\(Path p) -> p)
--          $ map (tmpToExpr s)
--          $ case expr of
--              (CutBop _ _ _  _ e1 e2) -> [e1, e2]
--              (CutFun _ _ _ _  es   ) -> es
--              (CutList _ _ _   es   ) -> es
--              _ -> []

-- TODO replace exprHash above with this
argHashes :: CutScript -> CutExpr -> [String]
argHashes s (CutRef  _ _ _   v ) = argHashes s $ lookupVar v s
argHashes s (CutFun  _ _ _ _ es) = concat $ map (argHashes s) es
argHashes _ _ = []

------------------------------------------
-- smart constructors for the new paths --
------------------------------------------

tmpToExpr :: CutState -> CutExpr -> Path TmpDir Expr
tmpToExpr s@(_, cfg) expr = Path res'
  where
    prefix = exprPrefix expr
    hash   = exprHash s expr
    ext    = extOf $ typeOf expr
    res    = "exprs" </> prefix </> hash <.> ext
    res'   = debugPath cfg "tmpToExpr" expr res

-- TODO is this too complicated?
-- TODO rename: linkToExpr :: ... -> Action?
-- TODO switch src and dst?
exprToExpr :: CutState -> CutExpr -> CutExpr -> Path Expr Expr
exprToExpr s src dst = Path $ backToTmp </> fromTmp
  where
    (Path fromTmp) = tmpToExpr s dst
    (Path src')    = tmpToExpr s src
    nDirsBack      = length $ filter isPathSeparator src'
    backToTmp      = foldr1 (</>) (take nDirsBack $ repeat "..")

varToExpr :: CutState -> CutExpr -> Path Var Expr
varToExpr s expr = Path $ ".." </> tmpPath
 where
    (Path tmpPath) = tmpToExpr s expr

varToVar :: CutExpr -> String -> Path Var Var
varToVar expr name = Path $ name <.> extOf (typeOf expr)

resToVar :: CutExpr -> String -> Path Res Var
resToVar expr name = Path v2v
  where
    (Path v2v) = varToVar expr name

-- Makes the path relative to working dir if possible, and absolute otherwise.
exprToInput :: CutConfig -> CutExpr -> FilePath -> Path Expr Input
exprToInput cfg _ input = Path $ makeRelative (cfgWorkDir cfg) input

-- TODO change to tmpToCache? rootToCache?
cacheDir2 :: CutConfig -> String -> Path TmpDir CacheDir
cacheDir2 cfg modName = Path $ cfgTmpDir cfg </> "cache" </> modName

-- Creates a unique hashed directory inside the main module cache dir.
-- Needed when scripts name their tmpfiles the same each time they're run
-- (I'm looking at you, crb-blast...)
cacheDirUniq2 :: CutState -> String -> CutExpr -> Path TmpDir CacheDir
cacheDirUniq2 s@(_, cfg) modName expr = Path $ mainCache </> hash
  where
    (Path mainCache) = cacheDir2 cfg modName
    hash = exprHash s expr
