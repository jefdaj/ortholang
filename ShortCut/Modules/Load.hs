module ShortCut.Modules.Load
  -- ( cutModule
  -- , mkLoad
  -- )
  where

-- TODO load string lists, which are needed for almost anything else!

import Development.Shake
import ShortCut.Core.Types
import Data.String.Utils     (strip)
import ShortCut.Core.Compile (cExpr, hashedTmp)
import ShortCut.Core.Parse   (defaultTypeCheck)
import System.Directory      (canonicalizePath)
import System.FilePath            (makeRelative)

---------------
-- interface --
---------------

cutModule :: CutModule
cutModule = CutModule
  { mName = "load"
  , mFunctions = []
  }

{- Takes a string with the filepath to load. Creates a trivial expression file
 - that's just a symlink to the given path. These should be the only absolute
 - links, and the only ones that point outside the temp dir.
 -}
mkLoad :: String -> CutType -> CutFunction
mkLoad name rtn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str] rtn
  , fFixity    = Prefix
  , fCompiler  = cLoadOne
  }

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
  , fCompiler  = cLoadList2
  }

--------------------
-- implementation --
--------------------

-- TODO replace with cLink2
-- cLink :: CutConfig -> FilePath -> FilePath -> Action ()
-- cLink cfg strPath dstPath = do
--   str <- fmap strip $ readFile' strPath
--   src <- liftIO $ canonicalizePath str
--   -- putQuiet $ unwords ["link", str', out]
--   quietly $ cmd "ln -fs" [src, dstPath]

-- The paths here are a little confusing: expr is a str of the path we want to
-- link to. So after compiling it we get a path to *that str*, and have to read
-- the file to access it. Then we want to `ln` to the file it points to.
cLink2 :: CutState -> CutExpr -> Rules FilePath
cLink2 s@(_,cfg) expr  = do
  strPath <- cExpr s expr
  -- TODO damn, need to be more systematic about these unique paths!
  let outPath = hashedTmp cfg expr ["outPath"] -- a symlink to the path in the strPath
  outPath %> \out -> do
    -- alwaysRerun
    liftIO $ putStrLn "running cLink2"
    str <- fmap strip $ readFile' strPath
    src <- liftIO $ canonicalizePath str -- TODO need relative symlink to pass tests
    -- let src = makeRelative (cfgTmpDir cfg) str
    liftIO . putStrLn $ unwords ["ln -fs", src, out]
    cmd "ln -fs" [src, out]
  return outPath

-- TODO this is linking to the path as a string rather than its actual path!
-- TODO need to include e in the list of paths given to cLink2?
cLoadOne :: CutState -> CutExpr -> Rules FilePath
cLoadOne s (CutFun _ _ _ [p]) = cLink2 s p
cLoadOne _ _ = error "bad argument to cLoadOne"

-- cLoadList :: CutState -> CutExpr -> Rules FilePath
-- cLoadList s@(_,cfg) e@(CutFun _ _ _ [CutList (ListOf t) ds ps]) = do
--   liftIO $ putStrLn "entering cLoadList"
--   strPaths <- mapM (cExpr s) ps
--   let loaded = hashedTmp cfg e strPaths
--       -- links  = map () ps
--   loaded %> \out -> do
--     -- pathsToStrs <- readFileLines strs
--     -- need pathsToStrs
--     userStrs <- liftIO $ mapM (\p -> fmap strip $ readFile p) strPaths
--     extPaths <- liftIO $ mapM canonicalizePath userStrs
--     need extPaths
--     writeFileLines out extPaths
--   return loaded
-- cLoadList _ _ = error "bad argument to cLoadList"

cLoadList2 :: CutState -> CutExpr -> Rules FilePath
cLoadList2 s@(_,cfg) e@(CutFun (ListOf t) _ _ [CutList _ _ ps]) = do
  liftIO $ putStrLn "entering cLoadList2"
  -- TODO any way to avoid depending on all ds each time?
  -- let fnCalls = map (\p -> CutFun t ds n [p]) ps
      -- e' = CutList (ListOf t) ds fnCalls
  -- lst <- cExpr s e'
  paths <- mapM (cLink2 s) ps -- TODO is cLink2 OK with no paths?
  let links = hashedTmp cfg e paths
  links %> \out -> need paths >> writeFileLines out paths
  return links
cLoadList2 _ _ = error "bad arguments to cLoadList2"
