module ShortCut.Core.ModuleAPI
  ( typeError
  , defaultTypeCheck
  , mkLoad
  , mkLoadList
  , cOneArgScript
  , cOneArgListScript
  , aTsvColumn
  , rSimpleTmp
  , rMapLast
  , rMapLastTmp
  , rMapLastTmps
  )
  where

import Development.Shake
import ShortCut.Core.Types

-- import Data.Set                   (fromList, toList)
import Data.List                  (nub, sort)
import Data.String.Utils          (strip)
import Development.Shake.FilePath ((</>), (<.>))
import ShortCut.Core.Paths        (cacheDir, cacheDirUniq, exprPath, exprPathExplicit)
import ShortCut.Core.Compile      (cExpr)
import ShortCut.Core.Debug        (debugTrackWrite, debugWriteLines
                                   , debug, debugWriteFile, debugReadLines)
import System.Directory           (canonicalizePath, createDirectoryIfMissing)
import System.FilePath            (takeBaseName, makeRelative)
import ShortCut.Core.Config       (wrappedCmd)
import ShortCut.Core.Util         (absolutize, resolveSymlinks)

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
    quietly $ unit $ wrappedCmd cfg [] script [tmpDir, oPath, argPath]
    trackWrite [oPath]
  return (ExprPath oPath)
cOneArgScript _ _ _ _ = error "bad argument to cOneArgScript"

-- for scripts that take one arg and return a list of lits,
-- which then needs converting to ShortCut format
-- TODO this should put tmpfiles in cache/<script name>!
-- TODO name something more explicitly about fasta files?
cOneArgListScript :: FilePath -> FilePath -> CutState -> CutExpr -> Rules ExprPath
cOneArgListScript tmpName script s@(_,cfg) expr@(CutFun _ _ _ _ [fa]) = do
  (ExprPath faPath) <- cExpr s fa
  let (CacheDir tmpDir ) = cacheDir cfg tmpName
      (ExprPath outPath) = exprPath cfg expr []
      -- tmpOut            = cacheFile cfg tmpName fa "txt"
  -- tmpOut %> \out -> do
  outPath %> \_ -> do
    need [faPath]
    Stdout out <- wrappedCmd cfg [Cwd tmpDir] script
                    [(debug cfg ("cOneArgList outPath: " ++ outPath) outPath), faPath]
    debugWriteFile cfg outPath out
    -- trackWrite [out]
  -- outPath %> \_ -> toShortCutList cfg str (ExprPath tmpOut) (ExprPath outPath)
  -- outPath %> \_ -> debugWriteLines cfg tmpOut (ExprPath outPath)
  return (ExprPath outPath)
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
-- TODO absolutize these to match the new list stuff
--      oh heeeey, don't have to follow them! just don't make at all lol
cLink :: CutState -> CutExpr -> CutType -> String -> Rules ExprPath
cLink s@(_,cfg) expr rtype prefix = do
  (ExprPath strPath) <- cExpr s expr -- TODO is this the issue?
  -- TODO fix this putting file symlinks in cut_lit dir. they should go in their own

  -- TODO only depend on final expressions
  let (ExprPath outPath) = exprPathExplicit cfg rtype prefix [show expr] -- ok without ["outPath"]?

  outPath %> \_ -> do
    pth <- fmap strip $ readFile' strPath
    -- src <- liftIO $ canonicalizePath pth
    src <- liftIO $ absolutize pth
    liftIO $ putStrLn $ "pth: " ++ pth
    liftIO $ putStrLn $ "src: " ++ src
    liftIO $ putStrLn $ "outPath: " ++ outPath
    need [src]
    -- TODO these have to be absolute, so golden tests need to adjust them:
    unit $ quietly $ wrappedCmd cfg [] "ln" ["-fs", src, outPath]
    debugTrackWrite cfg [outPath]
  return (ExprPath outPath)

cLoadOne :: CutType -> RulesFn
cLoadOne t s (CutFun _ _ _ n [p]) = cLink s p t n
cLoadOne _ _ _ = error "bad argument to cLoadOne"

-- load a list of files --

{- Like cLoad, except it operates on a list of strings. Note that you can also
 - load lists using cLoad, but it's not recommended because then you have to
 - write the list in a file, whereas this can handle literal lists in the
 - source code.
 -}
-- TODO fix it putting both the initial files and lists of them in the same dir!
--      (.faa and .faa.list are together in exprs/load_faa_all,
--       when the former should be in exprs/load_faa)
mkLoadList :: String -> CutType -> CutFunction
mkLoadList name rtn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [(ListOf str)] (ListOf rtn)
  , fFixity    = Prefix
  , fCompiler  = cLoadList
  }

-- TODO why is a list of lists like [[1,2,5], [1,2,3]] going to cLoadOne? fix!
cLoadList :: RulesFn
cLoadList s e@(CutFun (ListOf rtn) _ _ _ [es])
  | typeOf es `elem` [ListOf str, ListOf num] = cLoadListOne rtn s es
  -- | typeOf es `elem` [ListOf str, ListOf num] = cListOne s e -- infinite loop :(
  | otherwise = cLoadListMany s e
cLoadList _ _ = error "bad arguments to cLoadList"

-- special case for lists of str and num
-- TODO is this different from cListOne, except in its return type? unify!
cLoadListOne :: CutType -> RulesFn
cLoadListOne rtn (_,cfg) e@(CutList _ _ _ es) = do
  liftIO $ putStrLn "cLoadListOne"
  -- (ExprPath esPath) <- cExpr s es

  -- TODO only depend on final expressions
  let (ExprPath outPath) = exprPathExplicit cfg (ListOf rtn) "cut_list" [show e]

  outPath %> \_ -> do
    -- es <- debugReadLines cfg esPath
    let lits = map extractLit es
    lits' <- liftIO $ mapM absolutize lits
    liftIO $ putStrLn $ "lits: " ++ show lits
    liftIO $ putStrLn $ "lits': " ++ show lits'
    liftIO $ putStrLn $ "outPath: " ++ outPath
    debugWriteLines cfg outPath lits'
  return (ExprPath outPath)
  where
    extractLit (CutLit _ _ s) = s
    extractLit _ = error "bad argument to extractLit"
cLoadListOne _ _ e = error $ "bad arguments to cLoadListOne: " ++ show e

-- regular case for lists of any other file type
cLoadListMany :: RulesFn
cLoadListMany s@(_,cfg) e@(CutFun _ _ _ _ [es]) = do
  liftIO $ putStrLn "cLoadListMany"
  (ExprPath pathsPath) <- cExpr s es
  -- let (ExprPath outPath) = exprPath cfg e []

  -- TODO only depend on final expressions
  let (ExprPath outPath) = exprPathExplicit cfg (typeOf e) "cut_list" [show e]

  outPath %> \_ -> do
  -- let (ExprPath outPath) = exprPathExplicit cfg (ListOf elemRtnType) e name []
    -- paths <- fmap (map (cfgWorkDir cfg </>)) (debugReadLines cfg pathsPath)
    paths <- fmap (map (cfgTmpDir cfg </>)) (debugReadLines cfg pathsPath)
    paths' <- liftIO $ mapM resolveSymlinks paths
    -- paths <- debugReadLines cfg pathsPath
    -- let paths' = map (cfgWorkDir cfg </>) paths
    -- liftIO $ putStrLn $ "es: " ++ show expr
    liftIO $ putStrLn $ "paths: " ++ show paths
    liftIO $ putStrLn $ "paths': " ++ show paths'
    need paths'
    debugWriteLines cfg outPath paths'
  return (ExprPath outPath)
cLoadListMany _ _ = error "bad arguments to cLoadListMany"

-----------------------------------------------------------
-- [a]ction functions (just describe how to build files) --
-----------------------------------------------------------

-- based on https://stackoverflow.com/a/18627837
-- uniqLines :: Ord a => [a] -> [a]
-- uniqLines = unlines . toList . fromList . lines

-- TODO rewrite this awk -> haskell, and using wrappedCmd
aTsvColumn :: Int -> ActionFn
aTsvColumn n cfg _ [ExprPath outPath, ExprPath tsvPath] = do
  let awkCmd = "awk '{print $" ++ show n ++ "}'"
  Stdout out <- quietly $ cmd Shell awkCmd tsvPath
  let out' = sort $ nub $ lines out
  -- toShortCutListStr cfg str outPath out'
  debugWriteLines cfg outPath out'
aTsvColumn _ _ _ _ = error "bad arguments to aTsvColumn"

-------------------------------------------------------------------------------
-- [r]ules functions (just describe which files to build with which actions) --
-------------------------------------------------------------------------------

-- takes an action fn with any number of args and calls it with a tmpdir.
rSimpleTmp :: ActionFn -> String -> CutType -> RulesFn
rSimpleTmp actFn tmpPrefix _ s@(_,cfg) e@(CutFun _ _ _ _ exprs) = do
  argPaths <- mapM (cExpr s) exprs
  let (ExprPath outPath) = exprPath cfg e []
      (CacheDir tmpDir ) = cacheDir cfg tmpPrefix -- TODO tables bug here?
  outPath %> \_ -> do
    need $ map (\(ExprPath p) -> p) argPaths
    liftIO $ createDirectoryIfMissing True tmpDir
    actFn cfg (CacheDir tmpDir) ([ExprPath outPath] ++ argPaths)
    trackWrite [outPath]
  return (ExprPath outPath)
rSimpleTmp _ _ _ _ _ = error "bad argument to rSimpleTmp"

rMapLastTmp :: ActionFn -> String -> CutType -> RulesFn
rMapLastTmp actFn tmpPrefix t s@(_,cfg) = rMapLast (const tmpDir) actFn tmpPrefix t s
  where
    tmpDir = cacheDir cfg tmpPrefix

-- TODO use a hash for the cached path rather than the name, which changes!

-- takes an action fn and vectorizes the last arg (calls the fn with each of a
-- list of last args). returns a list of results. uses a new tmpDir each call.
rMapLastTmps :: ActionFn -> String -> CutType -> RulesFn
rMapLastTmps fn tmpPrefix t s@(_,cfg) e = rMapLast tmpFn fn tmpPrefix t s e
  where
    -- TODO what if the same last arg is used in different mapping fns? will it be unique?
    tmpFn args = cacheDirUniq cfg tmpPrefix args

-- common code factored out from the two functions above
-- TODO put the .args and final functions in the cachedir of the regular fn?
-- TODO now that the new Shake strategy works, clean it up!
-- TODO sprinkle some need in here?
rMapLast :: ([FilePath] -> CacheDir) -> ActionFn -> String -> CutType -> RulesFn
rMapLast tmpFn actFn _ rtnType s@(_,cfg) e@(CutFun _ _ _ name exprs) = do

  initPaths <- mapM (cExpr s) (init exprs)
  (ExprPath lastsPath) <- cExpr s (last exprs)
  let inits = map (\(ExprPath p) -> p) initPaths
      (ExprPath outPath) = exprPathExplicit cfg (ListOf rtnType) name [show e]
      (CacheDir mapTmp) = cacheDirUniq cfg "map_last" e

  outPath %> \_ -> do
    lastPaths <- readFileLines lastsPath
    -- this writes the .args files for use in the rule above
    (flip mapM_) lastPaths $ \p -> do
      -- TODO write the out path here too so all the args are together?
      let argsPath = mapTmp </> takeBaseName p <.> "args" -- TODO use a hash here?
          argPaths = inits ++ [cfgTmpDir cfg </> p]
      liftIO $ createDirectoryIfMissing True $ mapTmp
      debugWriteLines cfg argsPath argPaths
    -- then we just trigger them and write to the overall outPath
    let outPaths = map (\p -> mapTmp </> takeBaseName p) lastPaths
    need outPaths
    debugWriteLines cfg outPath outPaths

  -- This builds one of the list of out paths based on a .args file
  -- (made in the action above). It's a pretty roundabout way to do it!
  -- TODO ask ndmitchell if there's something much more elegant I'm missing
  (mapTmp </> "*") %> \out -> do
    let argsPath = out <.> ".args" -- TODO clean up
    -- args <- debugReadLines cfg argsPath
    args <- fmap lines $ liftIO $ readFile argsPath
    let args' = map (cfgTmpDir cfg </>) args
        rels  = map (makeRelative $ cfgTmpDir cfg) args
    need args'
    let (CacheDir dir) = tmpFn rels -- relative paths for determinism!
    liftIO $ createDirectoryIfMissing True dir
    actFn cfg (CacheDir dir) (map ExprPath (out:args'))
    trackWrite [out]

  return (ExprPath outPath)
rMapLast _ _ _ _ _ _ = error "bad argument to rMapLastTmps"
