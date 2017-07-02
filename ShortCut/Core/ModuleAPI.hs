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

import Data.String.Utils          (strip)
import Development.Shake.FilePath ((</>), (<.>))
import ShortCut.Core.Compile      (cExpr, scriptTmpDir, scriptTmpFile, cacheDir,
                                   hashedTmp, hashedTmp', exprDir, toShortCutList)
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

defaultTypeCheck :: [CutType] -> CutType
                 -> [CutType] -> Either String CutType
defaultTypeCheck expected returned actual =
  if actual == expected
    then Right returned
    else Left $ typeError expected actual

------------------------------------------
-- functions to make whole CutFunctions --
------------------------------------------

cOneArgScript :: FilePath -> FilePath -> CutState -> CutExpr -> Rules FilePath
cOneArgScript tmpName script s@(_,cfg) expr@(CutFun _ _ _ _ [arg]) = do
  argPath <- cExpr s arg
  let tmpDir = cacheDir cfg </> tmpName
      oPath  = hashedTmp cfg expr []
  oPath %> \_ -> do
    need [argPath]
    quietly $ unit $ cmd script tmpDir oPath argPath
    trackWrite [oPath]
  return oPath
cOneArgScript _ _ _ _ = error "bad argument to cOneArgScript"

cOneArgListScript :: FilePath -> FilePath -> CutState -> CutExpr -> Rules FilePath
cOneArgListScript tmpName script s@(_,cfg) expr@(CutFun _ _ _ _ [fa]) = do
  faPath <- cExpr s fa
  let tmpDir = cacheDir cfg </> tmpName
      tmpOut = scriptTmpFile cfg tmpDir expr "txt"
      actOut = hashedTmp cfg expr []
  tmpOut %> \out -> do
    need [faPath]
    quietly $ cmd script tmpDir out faPath
    -- trackWrite [out]
  actOut %> \_ -> toShortCutList cfg str tmpOut actOut
  return actOut
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
cLink :: CutState -> CutExpr -> CutType -> Rules FilePath
cLink s@(_,cfg) expr rtype = do
  strPath <- cExpr s expr
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
  , fCompiler  = cLoadList rtn
  }

cLoadList :: CutType -> CutState -> CutExpr -> Rules FilePath
cLoadList rtype s@(_,cfg) e@(CutFun (ListOf t) _ _ _ [CutList _ _ _ ps]) = do
  liftIO $ putStrLn "entering cLoadList"
  paths <- mapM (\p -> cLink s p rtype) ps -- TODO is cLink OK with no paths?
  let links = hashedTmp cfg e paths
  links %> \out -> need paths >> writeFileLines out paths
  return links
cLoadList _ _ _ = error "bad arguments to cLoadList"

-----------------------------------------------------------
-- [a]ction functions (just describe how to build files) --
-----------------------------------------------------------

aTsvColumn :: Int -> CutConfig -> [FilePath] -> Action ()
aTsvColumn n cfg as@[_, outPath, tsvPath] = do
  let awkCmd = "awk '{print $" ++ show n ++ "}'"
      tmpOut = scriptTmpFile cfg (exprDir cfg) ["aTsvColumn", show n, tsvPath] (extOf str)
  Stdout strs <- quietly $ cmd Shell awkCmd tsvPath
  writeFile' tmpOut strs
  toShortCutList cfg str tmpOut outPath
aTsvColumn _ _ as = error "bad arguments to aTsvColumn"

-------------------------------------------------------------------------------
-- [r]ules functions (just describe which files to build with which actions) --
-------------------------------------------------------------------------------

-- takes an action fn with any number of args and calls it with a tmpdir.
rSimpleTmp :: (CutConfig -> [FilePath] -> Action ()) -> String -> CutType
           -> (CutState -> CutExpr -> Rules FilePath)
rSimpleTmp actFn tmpPrefix rtnType s@(scr,cfg) e@(CutFun _ _ _ _ exprs) = do
  argPaths <- mapM (cExpr s) exprs
  let outPath = hashedTmp' cfg rtnType e []
      tmpDir  = scriptTmpDir cfg (cfgTmpDir cfg </> "cache" </> tmpPrefix) e
  outPath %> \_ -> do
    need argPaths
    liftIO $ createDirectoryIfMissing True tmpDir
    actFn cfg $ [tmpDir, outPath] ++ argPaths
    trackWrite [outPath]
  return outPath
rSimpleTmp _ _ _ _ _ = error "bad argument to rSimpleTmp"

rMapLastTmp :: (CutConfig -> [FilePath] -> Action ()) -> String -> CutType
            -> (CutState -> CutExpr -> Rules FilePath)
rMapLastTmp actFn tmpPrefix t@(ListOf elemType) s@(scr,cfg) e@(CutFun _ _ _ _ exprs) = do
  exprPaths <- mapM (cExpr s) exprs
  let outPath    = hashedTmp' cfg t e []
  outPath %> \_ -> do
    lastPaths <- debugReadLines cfg $ last exprPaths
    let inits  = init exprPaths
        lasts  = map (cfgTmpDir cfg </>) lastPaths
        tmpDir = exprDir cfg </> tmpPrefix -- TODO probably don't need the prefix anymore
        outs   = map (\p -> scriptTmpFile cfg (exprDir cfg) p (extOf elemType)) lastPaths
        outs'  = map (makeRelative $ cfgTmpDir cfg) outs
    (flip mapM)
      (zip outs lasts)
      (\(out, last) -> do
        need (last:inits)
        liftIO $ createDirectoryIfMissing True tmpDir
        actFn cfg (tmpDir:out:last:inits)
        trackWrite [out]
      )
    need outs
    writeFileLines outPath outs'
  return outPath
rMapLastTmp _ _ _ _ _ = error "bad argument to cMapLastTmp"

-- takes an action fn and vectorizes the last arg (calls the fn with each of a
-- list of last args). returns a list of results. uses a new tmpDir each call.
rMapLastTmps :: (CutConfig -> [FilePath] -> Action ()) -> String -> CutType
             -> (CutState -> CutExpr -> Rules FilePath)
rMapLastTmps actFn tmpPrefix rtnType s@(_,cfg) e@(CutFun _ _ _ _ exprs) = do
  initPaths <- mapM (cExpr s) (init exprs)
  lastsPath <- cExpr s (last exprs)
  let outPath    = hashedTmp' cfg (ListOf rtnType) e []
      tmpPrefix' = cfgTmpDir cfg </> "cache" </> tmpPrefix
  outPath %> \_ -> do
    lastPaths <- readFileLines lastsPath
    let lasts = map (cfgTmpDir cfg </>) lastPaths
        dirs  = map (\p -> scriptTmpDir cfg tmpPrefix' [show e, show p]) lasts
        outs  = map (\d -> d </> "out" <.> extOf rtnType) dirs
        rels  = map (makeRelative $ cfgTmpDir cfg) outs -- TODO standardize this stuff
    (flip mapM)
      (zip3 lasts dirs outs)
      (\(last, dir, out) -> do
        need $ initPaths ++ [last]
        liftIO $ createDirectoryIfMissing True dir
        actFn cfg $ [dir, out] ++ initPaths ++ [last]
        trackWrite [out]
      )
    need outs
    writeFileLines outPath rels
  return outPath
rMapLastTmps _ _ _ _ _ = error "bad argument to rMapLastTmps"
