module ShortCut.Modules.Load
  -- ( cutModule
  -- , mkLoad
  -- )
  where

-- TODO load string lists, which are needed for almost anything else!

import Debug.Trace

import Development.Shake
import ShortCut.Core.Types
import Data.String.Utils     (strip)
import ShortCut.Core.Compile (cExpr, hashedTmp, hashedTmp', toShortCutList)
import ShortCut.Core.Parse   (defaultTypeCheck)
import System.Directory      (canonicalizePath)
import System.FilePath            (makeRelative)

cutModule :: CutModule
cutModule = CutModule
  { mName = "load"
  , mFunctions = []
  }

------------------------
-- load a single file --
------------------------

{- Takes a string with the filepath to load. Creates a trivial expression file
 - that's just a symlink to the given path. These should be the only absolute
 - links, and the only ones that point outside the temp dir.
 -}
mkLoad :: String -> CutType -> CutFunction
mkLoad name rtn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str] rtn
  , fFixity    = Prefix
  , fCompiler  = cLoadOne rtn
  }

-- The paths here are a little confusing: expr is a str of the path we want to
-- link to. So after compiling it we get a path to *that str*, and have to read
-- the file to access it. Then we want to `ln` to the file it points to.
cLink :: CutState -> CutExpr -> CutType -> Rules FilePath
cLink s@(_,cfg) expr rtype = do
  strPath <- cExpr s (traceShow expr expr)
  -- TODO damn, need to be more systematic about these unique paths!
  let outPath = hashedTmp' cfg rtype expr ["outPath"] -- TODo remove outPath part?
  outPath %> \out -> do
    str <- fmap strip $ readFile' strPath
    src <- liftIO $ canonicalizePath str
    need [src]
    -- TODO these have to be absolute, so golden tests need to adjust them:
    quietly $ cmd "ln -fs" [src, out]
  return outPath

cLoadOne :: CutType -> CutState -> CutExpr -> Rules FilePath
cLoadOne t s (CutFun _ _ _ [p]) = cLink (trace "cLoadOne" s) p t
cLoadOne _ _ _ = error "bad argument to cLoadOne"

--------------------------
-- load a list of files --
--------------------------

{- Like cLoad, except it operates on a list of strings. Note that you can also
 - load lists using cLoad, but it's not recommended because then you have to
 - write the list in a file, whereas this can handle literal lists in the
 - source code.
 -}
mkLoadList :: String -> CutType -> CutFunction
mkLoadList name rtn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [(ListOf str)] (ListOf rtn)
  , fFixity    = Prefix
  , fCompiler  = cLoadList rtn
  }

cLoadList :: CutType -> CutState -> CutExpr -> Rules FilePath
cLoadList rtype s@(_,cfg) e@(CutFun (ListOf t) _ _ [CutList _ _ ps]) = do
  liftIO $ putStrLn "entering cLoadList"
  paths <- mapM (\p -> cLink s p rtype) ps -- TODO is cLink OK with no paths?
  let links = hashedTmp cfg e paths
  links %> \out -> need paths >> writeFileLines out paths
  return links
cLoadList _ _ _ = error "bad arguments to cLoadList"

-------------------------------------------
-- load a list of strings as one big one --
-------------------------------------------

-- TODO finish writing this

-- loadLines :: CutFunction
-- loadLines = CutFunction
--   { fName      = "list_lines"
--   , fTypeCheck = defaultTypeCheck [str] (ListOf str)
--   , fFixity    = Prefix
--   , fCompiler  = cListLines
--   }

-- toShortCutList :: CutState -> CutType -> FilePath -> FilePath -> Action ()
-- TODO requires multiline strings? should be pretty easy, just """
-- cListLines :: CutState -> CutExpr -> Rules FilePath
-- cListLines s@(_,cfg) e@(CutFun _ _ _ [lines]) = do
--   strPath <- cExpr s (traceShow ("expr: " ++ show lines) lines)
--   let outPath = hashedTmp cfg e []
--   -- outPath %> \out -> do
--     -- strs <- fmap (map strip . lines . strip) (readFile' strPath)
--     -- undefined
--   -- return outPath
--   (traceShow ("outPath: " ++ outPath) outPath) %> toShortCutList s str strPath -- TODO this doesn't work :(
--   return outPath
-- cListLines _ _ = error "bad argument to cListLines"
