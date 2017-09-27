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
-- THEN REASSESS/DOCUMENT + DEBUG BLAST, AFTER A BREAK DAY (OR DO TYPED EXPRESSIONS IF FEEL LIKE)

{- This is a transitional module for the new phantom-typed paths;
 - once everything uses them I'll remove the other and rename this to Paths.
 -}

module ShortCut.Core.Compile.Paths2
  -- currently used in the codebase and need updating:
  ( MyPath(..) -- TODO don't export constructor for safety?
  , Path, Abs, Rel, File, Dir
  , cacheDir
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
-- import ShortCut.Core.Debug (debug)
-- import System.FilePath (isPathSeparator, makeRelative)
-- import Text.PrettyPrint.HughesPJClass
import Data.Data                  (Data)
import Data.List                  (intersperse)
import Data.Maybe                 (fromJust)
import Development.Shake.FilePath ((<.>), (</>), makeRelative)
import ShortCut.Core.Debug        (debugHash, debug)
import ShortCut.Core.Util         (digest)

-----------------------
-- new Paths-based paths --
-----------------------

---------------------------------
-- aborted phantom-typed paths --
---------------------------------

-- TODO remove all this gunk and replace using Paths pkg

-- This doesn't guarantee much of anything on its own; needs smart constructors!
-- TODO hide it from being exported
-- (rtn put off until I have some time to add it)
-- newtype Path src dst rtn = Path FilePath deriving (Show, Data)
newtype MyPath src dst = MyPath FilePath deriving (Show, Data)

-- Possible source types:
-- TODO also hide from being exported, or no?
-- TODO Root?
-- data TmpDir   -- <tmpdir>
-- data Res      -- <tmpdir>/vars/result (TODO: salt dirs)
-- data Var      -- <tmpdir>/vars/<name>.<ext>
-- data Expr     -- <tmpdir>/exprs/<prefix>/<hash>.<ext>

-- Possible destination types are Res, Var, Expr, or:
-- data Input     -- any/path/the/user/feels.like
-- data CacheDir2 -- <tmpdir>/cache/<modulename>

-- Then the return type is either a phantom CutType, or:
-- (these are put off until I have some time to add them)
-- data Dir -- a dir rather than file (only used for cache dir so far)

{- This code:
 - let e = (CutExpr2 $ CutLit str 0 "stuff") :: CutExpr2 CutStr
 -     p = exprPath [] e
 -
 - ... would create a "path from the tmpdir to an expression of type str":
 - p :: MyPath TmpDir Expr CutStr
 - p = MyPath "exprs/cut_lit/6f2d5f011a.str"
 -}

-- TODO add phantom type to var + expr
-- TODO version that doesn't assume it exists?
lookupVar :: CutVar -> CutScript -> CutExpr
lookupVar var scr = fromJust $ lookup var scr

-- TODO hash phantom type? so far we just ignore it. will determine ext anyway
-- TODO debug fn specifically for this?
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
    pref = exprPrefix expr
    salt = show $ saltOf expr
    name = case expr of -- TODO roll this into prefix?
             (CutBop _ _ _ n  _ _) -> [n]
             (CutFun _ _ _ n _   ) -> [n]
             _ -> []

-- TODO use Paths here rather than hashes to make it map-compatible!
-- TODO and make them relative to the tmpdir for determinism
argHashes :: CutState -> CutExpr -> [String]
argHashes s@(_, cfg) expr = debug cfg ("argHashes for '" ++ show expr ++ "': " ++ show res) res
  where
    res = argHashes' s expr

-- TODO rename hPath?
-- TODO act differently when given a lit path?
pathHash :: CutConfig -> FilePath -> String
pathHash cfg = digest . makeRelative (cfgTmpDir cfg)

-- TODO rename hSomething?
argHashes' :: CutState -> CutExpr -> [String]
argHashes' s@(as,_) (CutRef _ _ _ v) = argHashes s $ lookupVar v as
argHashes' _ (CutLit  _ _     v ) = [digest v]
argHashes' s@(_,cfg) (CutFun  _ _ _ _ es) = map (pathHash cfg . fromAbsFile . exprPath s) es
argHashes' s@(_,cfg) (CutBop  _ _ _ _ e1 e2) = map (pathHash cfg . fromAbsFile . exprPath s) [e1, e2]
argHashes' s@(_,cfg) (CutList _ _ _   es) = [digest $ concat $ map (pathHash cfg . fromAbsFile . exprPath s) es]

-- TODO add names to the CutBops themselves... or associate with prefix versions?
exprPrefix :: CutExpr -> String
exprPrefix (CutLit rtn _ _     ) = extOf rtn
exprPrefix (CutFun _ _ _ name _) = name
exprPrefix (CutList _ _ _ _    ) = "list"
exprPrefix (CutRef _ _ _ _     ) = error  "CutRefs don't need a prefix"
exprPrefix (CutBop _ _ _ n _ _ ) = case n of
                                     "+" -> "add"
                                     "-" -> "subtract"
                                     "*" -> "multiply"
                                     "/" -> "divide"
                                     "~" -> "difference"
                                     "&" -> "intersection"
                                     "|" -> "union"
                                     _   -> error "unknown CutBop"

-- TODO rename... back to exprPath for now? and rewrite exprPathExplicit to match?
exprPath :: CutState -> CutExpr -> Path Abs File
exprPath s@(scr, _) (CutRef _ _ _ v) = exprPath s $ lookupVar v scr
exprPath s expr = exprPathExplicit s prefix rtype salt hashes
  where
    prefix = exprPrefix expr
    rtype  = typeOf expr
    salt   = saltOf expr
    hashes = argHashes s expr

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
