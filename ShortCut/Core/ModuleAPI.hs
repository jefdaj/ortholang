module ShortCut.Core.ModuleAPI
  ( typeError
  , defaultTypeCheck
  , mkLoad
  , mkLoadList
  , cOneArgScript
  , cOneArgListScript
  , aTsvColumn
  , rSimpleTmp
  , rMapLastTmp
  , rMapLastTmps
  )
  where

import Development.Shake
import ShortCut.Core.Types

import Data.Set                   (fromList, toList)
import Data.String.Utils          (strip)
import Development.Shake.FilePath ((</>), (<.>))
import ShortCut.Core.Paths        (cacheDir, cacheDirUniq, cacheFile, exprPath, exprPathTyped)
import ShortCut.Core.Compile      (cExpr, toShortCutList)
import ShortCut.Core.Debug        (debugReadLines)
import System.Directory           (canonicalizePath, createDirectoryIfMissing)
import System.FilePath            (makeRelative)

------------------------------
-- [t]ypechecking functions --
------------------------------

typeError :: [CutType] -> [CutType] -> String
typeError expected actual =
  "Type error:\nexpected " ++ show expected
           ++ "\nbut got " ++ show actual

-- TODO this should fail for type errors like multiplying a list by a num!
defaultTypeCheck :: [CutType] -> CutType
                 -> [CutType] -> Either String CutType
defaultTypeCheck expected returned actual =
  if actual == expected
    then Right returned
    else Left $ typeError expected actual

------------------------------------------
-- functions to make whole CutFunctions --
------------------------------------------

cOneArgScript :: FilePath -> FilePath -> CutState -> CutExpr -> Rules ExprPath
cOneArgScript tmpName script s@(_,cfg) expr@(CutFun _ _ _ _ [arg]) = do
  (ExprPath argPath) <- cExpr s arg
  -- let tmpDir = cacheDir cfg </> tmpName
  -- TODO get tmpDir from a Paths funcion
  let tmpDir = cfgTmpDir cfg </> "cache" </> tmpName
      (ExprPath oPath) = exprPath cfg expr []
  oPath %> \_ -> do
    need [argPath]
    quietly $ unit $ cmd script tmpDir oPath argPath
    trackWrite [oPath]
  return (ExprPath oPath)
cOneArgScript _ _ _ _ = error "bad argument to cOneArgScript"

cOneArgListScript :: FilePath -> FilePath -> CutState -> CutExpr -> Rules ExprPath
cOneArgListScript tmpName script s@(_,cfg) expr@(CutFun _ _ _ _ [fa]) = do
  (ExprPath faPath) <- cExpr s fa
  let tmpDir = cfgTmpDir cfg </> "cache" </> tmpName
      tmpOut = cacheFile cfg tmpDir expr "txt"
      (ExprPath actOut) = exprPath cfg expr []
  tmpOut %> \out -> do
    need [faPath]
    quietly $ cmd script tmpDir out faPath
    -- trackWrite [out]
  actOut %> \_ -> toShortCutList cfg str (ExprPath tmpOut) (ExprPath actOut)
  return (ExprPath actOut)
cOneArgListScript _ _ _ _ = error "bad argument to cOneArgListScript"

-- load a single file --

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
cLink :: CutState -> CutExpr -> CutType -> Rules ExprPath
cLink s@(_,cfg) expr rtype = do
  (ExprPath strPath) <- cExpr s expr
  -- TODO damn, need to be more systematic about these unique paths!
  let (ExprPath outPath) = exprPathTyped cfg rtype expr [] -- ok without ["outPath"]?
  outPath %> \out -> do
    str <- fmap strip $ readFile' strPath
    src <- liftIO $ canonicalizePath str
    need [src]
    -- TODO these have to be absolute, so golden tests need to adjust them:
    quietly $ cmd "ln -fs" [src, out]
  return (ExprPath outPath)

cLoadOne :: CutType -> CutState -> CutExpr -> Rules ExprPath
cLoadOne t s (CutFun _ _ _ _ [p]) = cLink s p t
cLoadOne _ _ _ = error "bad argument to cLoadOne"

-- load a list of files --

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
  , fCompiler  = cLoadList
  }

cLoadList :: CutState -> CutExpr -> Rules ExprPath
cLoadList s@(_,cfg) e@(CutFun (ListOf t) _ _ _ [CutList _ _ _ ps]) = do
  paths <- mapM (\p -> cLink s p t) ps -- is cLink OK with no paths?
  let paths' = map (\(ExprPath p) -> p) paths
      (ExprPath links) = exprPathTyped cfg t e []
  links %> \out -> need paths' >> writeFileLines out paths'
  return (ExprPath links)
cLoadList _ _ = error "bad arguments to cLoadList"

-----------------------------------------------------------
-- [a]ction functions (just describe how to build files) --
-----------------------------------------------------------

-- based on https://stackoverflow.com/a/18627837
-- uniqLines :: Ord a => [a] -> [a]
uniqLines = unlines . toList . fromList . lines

aTsvColumn :: Int -> CutConfig -> CacheDir -> [ExprPath] -> Action ()
aTsvColumn n cfg _ as@[(ExprPath outPath), (ExprPath tsvPath)] = do
  let awkCmd = "awk '{print $" ++ show n ++ "}'"
      -- TODO write toShortCutList' that takes strings
      tmpOut = cacheFile cfg (cfgTmpDir cfg </> "exprs") ["aTsvColumn", show n, tsvPath] (extOf str)
  Stdout strs <- quietly $ cmd Shell awkCmd tsvPath
  writeFile' tmpOut $ uniqLines strs
  toShortCutList cfg str (ExprPath tmpOut) (ExprPath outPath)
aTsvColumn _ _ _ as = error "bad arguments to aTsvColumn"

-------------------------------------------------------------------------------
-- [r]ules functions (just describe which files to build with which actions) --
-------------------------------------------------------------------------------

-- takes an action fn with any number of args and calls it with a tmpdir.
rSimpleTmp :: (CutConfig -> CacheDir -> [ExprPath] -> Action ()) -> String -> CutType
           -> (CutState -> CutExpr -> Rules ExprPath)
rSimpleTmp actFn tmpPrefix rtnType s@(scr,cfg) e@(CutFun _ _ _ _ exprs) = do
  argPaths <- mapM (cExpr s) exprs
  let (ExprPath outPath) = exprPath cfg e []
      (CacheDir tmpDir ) = cacheDir cfg tmpPrefix
  outPath %> \_ -> do
    need $ map (\(ExprPath p) -> p) argPaths
    liftIO $ createDirectoryIfMissing True tmpDir
    actFn cfg (CacheDir tmpDir) ([ExprPath outPath] ++ argPaths)
    trackWrite [outPath]
  return (ExprPath outPath)
rSimpleTmp _ _ _ _ _ = error "bad argument to rSimpleTmp"

rMapLastTmp :: (CutConfig -> CacheDir -> [ExprPath] -> Action ()) -> String -> CutType
            -> (CutState -> CutExpr -> Rules ExprPath)
rMapLastTmp actFn tmpPrefix t@(ListOf elemType) s@(scr,cfg) e@(CutFun _ _ _ _ exprs) = do
  exprPaths <- mapM (cExpr s) exprs
  let (ExprPath outPath) = exprPath cfg e []
  outPath %> \_ -> do
    lastPaths <- debugReadLines cfg $ (\(ExprPath p) -> p) $ last exprPaths
    let inits  = init exprPaths
        lasts  = map (cfgTmpDir cfg </>) lastPaths
        -- TODO replace with a Paths function
        tmpDir = cfgTmpDir cfg </> "cache" </> tmpPrefix
        outs   = map (\p -> cacheFile cfg (cfgTmpDir cfg </> "cache" </> "shortcut") p (extOf elemType)) lastPaths
        outs'  = map (makeRelative $ cfgTmpDir cfg) outs
    (flip mapM)
      (zip outs lasts)
      (\(out, last) -> do
        need (last:map (\(ExprPath p) -> p) inits)
        liftIO $ createDirectoryIfMissing True tmpDir
        actFn cfg (CacheDir tmpDir) (ExprPath out:ExprPath last:inits)
        trackWrite [out]
      )
    need outs
    writeFileLines outPath outs'
  return (ExprPath outPath)
rMapLastTmp _ _ _ _ _ = error "bad argument to cMapLastTmp"

-- takes an action fn and vectorizes the last arg (calls the fn with each of a
-- list of last args). returns a list of results. uses a new tmpDir each call.
rMapLastTmps :: (CutConfig -> CacheDir -> [ExprPath] -> Action ()) -> String -> CutType
             -> (CutState -> CutExpr -> Rules ExprPath)
rMapLastTmps actFn tmpPrefix rtnType s@(_,cfg) e@(CutFun _ _ _ _ exprs) = do
  initPaths <- mapM (cExpr s) (init exprs)
  (ExprPath lastsPath) <- cExpr s (last exprs)
  let (ExprPath outPath) = exprPathTyped cfg (ListOf rtnType) e []
      -- tmpPrefix' = cfgTmpDir cfg </> "cache" </> tmpPrefix
      tmpDir = cacheDir cfg tmpPrefix
  outPath %> \_ -> do
    lastPaths <- readFileLines lastsPath
    let inits = map (\(ExprPath p) -> p) initPaths
        lasts = map (\p -> ExprPath $ cfgTmpDir cfg </> p) lastPaths
        dirs  = map (\(ExprPath p) -> cacheDirUniq cfg tmpPrefix [show e, show p]) lasts
        outs  = map (\(CacheDir d) -> ExprPath (d </> "out" <.> extOf rtnType)) dirs
        rels  = map (\(ExprPath p) -> makeRelative (cfgTmpDir cfg) p) outs
    -- TODO oh shit, does this only work sequentially? parallelize!
    (flip mapM)
      (zip3 lasts dirs outs)
      (\(l@(ExprPath last), (CacheDir dir), o@(ExprPath out)) -> do
        need $ inits ++ [last]
        liftIO $ createDirectoryIfMissing True dir
        actFn cfg (CacheDir dir) ([o] ++ initPaths ++ [l])
        trackWrite [out]
      )
    need $ map (\(ExprPath p) -> p) outs
    writeFileLines outPath rels
  return (ExprPath outPath)
rMapLastTmps _ _ _ _ _ = error "bad argument to rMapLastTmps"
