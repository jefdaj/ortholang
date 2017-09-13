module ShortCut.Core.ModuleAPI
  ( typeError
  , defaultTypeCheck
  , mkLoad
  , mkLoadList
  , rOneArgScript
  , rOneArgListScript
  , aTsvColumn
  , rSimpleTmp
  , rMapLast
  , rMapLastTmp
  , rMapLastTmps
  , typeMatches
  , typesMatch
  )
  where

import Development.Shake
import ShortCut.Core.Types

-- import Data.Set                   (fromList, toList)
import Data.List                  (nub, sort)
import Data.String.Utils          (strip)
import Development.Shake.FilePath ((</>), (<.>))
import ShortCut.Core.Paths        (cacheDir, cacheDirUniq, exprPath, exprPathExplicit)
import ShortCut.Core.Rules      (rExpr)
import ShortCut.Core.Debug        (debugTrackWrite, debugWriteLines, debugReadLines)
import System.Directory           (createDirectoryIfMissing)
import System.FilePath            (takeBaseName, makeRelative)
import ShortCut.Core.Config       (wrappedCmd)
import ShortCut.Core.Util         (absolutize, resolveSymlinks)
import Text.PrettyPrint.HughesPJClass

------------------------------
-- [t]ypechecking functions --
------------------------------

typeError :: [CutType] -> [CutType] -> String
typeError expected actual =
  "Type error:\nexpected " ++ show expected
           ++ "\nbut got " ++ show actual

-- this mostly checks equality, but also has to deal with how an empty set can
-- be any kind of set
-- TODO is there any more elegant way? this seems error-prone...
typeMatches :: CutType -> CutType -> Bool
typeMatches EmptySet  (SetOf _) = True
typeMatches (SetOf _) EmptySet  = True
typeMatches a b = a == b

typesMatch :: [CutType] -> [CutType] -> Bool
typesMatch as bs = sameLength && allMatch
  where
    sameLength = length as == length bs
    allMatch   = all (\(a,b) -> a `typeMatches` b) (zip as bs)

-- TODO this should fail for type errors like multiplying a list by a num!
defaultTypeCheck :: [CutType] -> CutType
                 -> [CutType] -> Either String CutType
defaultTypeCheck expected returned actual =
  if actual `typesMatch` expected
    then Right returned
    else Left $ typeError expected actual

------------------------------------------
-- functions to make whole CutFunctions --
------------------------------------------

rOneArgScript :: FilePath -> FilePath -> CutState -> CutExpr -> Rules ExprPath
rOneArgScript tmpName script s@(_,cfg) expr@(CutFun _ _ _ _ [arg]) = do
  (ExprPath argPath) <- rExpr s arg
  -- let tmpDir = cacheDir cfg </> tmpName
  -- TODO get tmpDir from a Paths funcion
  let tmpDir = cfgTmpDir cfg </> "cache" </> tmpName
      (ExprPath oPath) = exprPath cfg True expr []
  oPath %> \_ -> aOneArgScript cfg oPath script tmpDir argPath
  return (ExprPath oPath)
rOneArgScript _ _ _ _ = error "bad argument to rOneArgScript"

aOneArgScript :: CutConfig -> String -> FilePath -> FilePath -> FilePath -> Action ()
aOneArgScript cfg oPath script tmpDir argPath = do
  need [argPath]
  liftIO $ createDirectoryIfMissing True tmpDir
  quietly $ unit $ wrappedCmd cfg [oPath] [] script [tmpDir, oPath, argPath]
  trackWrite [oPath]

-- for scripts that take one arg and return a list of lits
-- TODO this should put tmpfiles in cache/<script name>!
-- TODO name something more explicitly about fasta files?
rOneArgListScript :: FilePath -> FilePath -> CutState -> CutExpr -> Rules ExprPath
rOneArgListScript tmpName script s@(_,cfg) expr@(CutFun _ _ _ _ [fa]) = do
  (ExprPath faPath) <- rExpr s fa
  let (CacheDir tmpDir ) = cacheDir cfg tmpName
      (ExprPath outPath) = exprPath cfg True expr []
  outPath %> \_ -> aOneArgListScript cfg outPath script tmpDir faPath
  return (ExprPath outPath)
rOneArgListScript _ _ _ _ = error "bad argument to rOneArgListScript"

aOneArgListScript :: CutConfig -> FilePath -> String -> FilePath -> FilePath -> Action ()
aOneArgListScript cfg outPath script tmpDir faPath = do
  need [faPath]
  liftIO $ createDirectoryIfMissing True tmpDir
  wrappedCmd cfg [outPath] [Cwd tmpDir] script [outPath, faPath]
  -- debugWriteFile cfg outPath out
  debugTrackWrite cfg [outPath]

-- load a single file --

{- Takes a string with the filepath to load. Creates a trivial expression file
 - that's just a symlink to the given path. These should be the only absolute
 - links, and the only ones that point outside the temp dir.
 - TODO still true?
 -}
mkLoad :: String -> CutType -> CutFunction
mkLoad name rtn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str] rtn
  , fFixity    = Prefix
  , fRules  = rLoadOne rtn
  }

-- The paths here are a little confusing: expr is a str of the path we want to
-- link to. So after compiling it we get a path to *that str*, and have to read
-- the file to access it. Then we want to `ln` to the file it points to.
-- TODO should this go in Compile.hs?
rLink :: CutState -> CutExpr -> CutType -> String -> Rules ExprPath
rLink s@(_,cfg) expr rtype prefix = do
  (ExprPath strPath) <- rExpr s expr -- TODO is this the issue?
  -- TODO only depend on final expressions
  -- ok without ["outPath"]?
  let (ExprPath outPath) = exprPathExplicit cfg True rtype prefix [show expr]
  outPath %> \_ -> aLink cfg outPath strPath
  return (ExprPath outPath)

aLink :: CutConfig -> FilePath -> FilePath -> Action ()
aLink cfg outPath strPath = do
  pth <- fmap strip $ readFile' strPath
  src <- liftIO $ absolutize pth -- TODO also follow symlinks here?
  need [src]
  unit $ quietly $ wrappedCmd cfg [outPath] [] "ln" ["-fs", src, outPath]
  debugTrackWrite cfg [outPath]

rLoadOne :: CutType -> RulesFn
rLoadOne t s (CutFun _ _ _ n [p]) = rLink s p t n
rLoadOne _ _ _ = error "bad argument to rLoadOne"

-- load a list of files --

{- Like cLoad, except it operates on a list of strings. Note that you can also
 - load lists using cLoad, but it's not recommended because then you have to
 - write the list in a file, whereas this can handle literal lists in the
 - source code.
 -}
-- TODO fix it putting both the initial files and lists of them in the same dir!
--      (.faa and .faa.list are together in exprs/load_faa_each,
--       when the former should be in exprs/load_faa)
mkLoadList :: String -> CutType -> CutFunction
mkLoadList name rtn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [(SetOf str)] (SetOf rtn)
  , fFixity    = Prefix
  , fRules  = rLoadList
  }

rLoadList :: RulesFn
rLoadList s e@(CutFun (SetOf rtn) _ _ _ [es])
  | typeOf es `elem` [SetOf str, SetOf num] = rLoadListOne rtn s es
  | otherwise = rLoadListMany s e
rLoadList _ _ = error "bad arguments to rLoadList"

-- special case for lists of str and num
-- TODO remove rtn and use (typeOf expr)?
-- TODO is this different from rSetOne, except in its return type?
-- TODO is it different from rLink? seems like it's just a copy/link operation...
rLoadListOne :: CutType -> RulesFn
rLoadListOne rtn s@(_,cfg) expr = do
  (ExprPath litsPath) <- rExpr s expr
  let relPath = makeRelative (cfgTmpDir cfg) litsPath
      (ExprPath outPath) = exprPathExplicit cfg True (SetOf rtn) "cut_set" [relPath]
  outPath %> \_ -> aLoadListOne cfg outPath litsPath
  return (ExprPath outPath)

aLoadListOne :: CutConfig -> FilePath -> FilePath -> Action ()
aLoadListOne cfg outPath litsPath = do
  lits  <- debugReadLines cfg litsPath -- TODO strip?
  lits' <- liftIO $ mapM absolutize lits -- TODO does this mess up non-paths?
  debugWriteLines cfg outPath lits'

-- regular case for lists of any other file type
rLoadListMany :: RulesFn
rLoadListMany s@(_,cfg) e@(CutFun _ _ _ _ [es]) = do
  (ExprPath pathsPath) <- rExpr s es
  -- TODO is relPath enough to make sure it's unique??
  let relPath = makeRelative (cfgTmpDir cfg) pathsPath
      (ExprPath outPath) = exprPathExplicit cfg True (typeOf e) "cut_set" [relPath]
  outPath %> \_ -> aLoadListMany cfg outPath pathsPath
  return (ExprPath outPath)
rLoadListMany _ _ = error "bad arguments to rLoadListMany"

aLoadListMany :: CutConfig -> FilePath -> FilePath -> Action ()
aLoadListMany cfg outPath pathsPath = do
    paths <- fmap (map (cfgTmpDir cfg </>)) (debugReadLines cfg pathsPath)
    need paths
    paths' <- liftIO $ mapM resolveSymlinks paths
    -- need paths'
    debugWriteLines cfg outPath paths'

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
  -- toShortCutSetStr cfg str outPath out'
  debugWriteLines cfg outPath out'
aTsvColumn _ _ _ _ = error "bad arguments to aTsvColumn"

-------------------------------------------------------------------------------
-- [r]ules functions (just describe which files to build with which actions) --
-------------------------------------------------------------------------------

-- takes an action fn with any number of args and calls it with a tmpdir.
rSimpleTmp :: ActionFn -> String -> CutType -> RulesFn
rSimpleTmp actFn tmpPrefix _ s@(_,cfg) e@(CutFun _ _ _ _ exprs) = do
  argPaths <- mapM (rExpr s) exprs
  let (ExprPath outPath) = exprPath cfg True e []
      (CacheDir tmpDir ) = cacheDir cfg tmpPrefix -- TODO tables bug here?
  outPath %> \_ -> aSimpleTmp cfg outPath actFn tmpDir argPaths
  return (ExprPath outPath)
rSimpleTmp _ _ _ _ _ = error "bad argument to rSimpleTmp"

aSimpleTmp :: CutConfig -> FilePath -> ActionFn -> FilePath -> [ExprPath] -> Action ()
aSimpleTmp cfg outPath actFn tmpDir argPaths = do
  need $ map (\(ExprPath p) -> p) argPaths
  liftIO $ createDirectoryIfMissing True tmpDir
  actFn cfg (CacheDir tmpDir) ([ExprPath outPath] ++ argPaths)
  trackWrite [outPath]

rMapLastTmp :: ActionFn -> String -> CutType -> RulesFn
rMapLastTmp actFn tmpPrefix t s@(_,cfg) = mapFn t s
  where
    tmpDir = cacheDir cfg tmpPrefix
    mapFn  = rMapLast (const tmpDir) actFn tmpPrefix

-- TODO use a hash for the cached path rather than the name, which changes!

-- takes an action fn and vectorizes the last arg (calls the fn with each of a
-- list of last args). returns a list of results. uses a new tmpDir each call.
rMapLastTmps :: ActionFn -> String -> CutType -> RulesFn
rMapLastTmps fn tmpPrefix t s@(_,cfg) e = rMapLast tmpFn fn tmpPrefix t s e
  where
    -- TODO what if the same last arg is used in different mapping fns?
    --      will it be unique?
    tmpFn args = cacheDirUniq cfg tmpPrefix args

-- common code factored out from the two functions above
-- TODO put the .args and final functions in the cachedir of the regular fn?
-- TODO now that the new Shake strategy works, clean it up!
-- TODO sprinkle some need in here?
rMapLast :: ([FilePath] -> CacheDir) -> ActionFn -> String -> CutType -> RulesFn
rMapLast tmpFn actFn _ rtnType s@(_,cfg) e@(CutFun _ _ _ name exprs) = do
  liftIO $ putStrLn $ "rMapLast expr: " ++ render (pPrint e)

  initPaths <- mapM (rExpr s) (init exprs)
  (ExprPath lastsPath) <- rExpr s (last exprs)
  let inits = map (\(ExprPath p) -> p) initPaths
      (ExprPath outPath) = exprPathExplicit cfg True (SetOf rtnType) name [show e]
      (CacheDir mapTmp) = cacheDirUniq cfg "map_last" e

  outPath %> \_ -> aMapLastArgs cfg outPath inits mapTmp lastsPath

  -- This builds one of the list of out paths based on a .args file
  -- (made in the action above). It's a pretty roundabout way to do it!
  -- TODO ask ndmitchell if there's something much more elegant I'm missing
  (mapTmp </> "*") %> aMapLastMapTmp cfg tmpFn actFn

  return (ExprPath outPath)
rMapLast _ _ _ _ _ _ = error "bad argument to rMapLastTmps"

aMapLastArgs :: CutConfig -> FilePath -> [FilePath] -> FilePath -> FilePath -> Action ()
aMapLastArgs cfg outPath inits mapTmp lastsPath = do
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

-- TODO rename this something less confusing
aMapLastMapTmp :: CutConfig
               -> ([FilePath] -> CacheDir)
               -> (CutConfig -> CacheDir -> [ExprPath] -> Action a)
               -> FilePath -> Action ()
aMapLastMapTmp cfg tmpFn actFn out = do
  let argsPath = out <.> ".args" -- TODO clean up
  -- args <- debugReadLines cfg argsPath
  args <- fmap lines $ liftIO $ readFile argsPath
  let args' = map (cfgTmpDir cfg </>) args
      rels  = map (makeRelative $ cfgTmpDir cfg) args
  need args'
  let (CacheDir dir) = tmpFn rels -- relative paths for determinism!
      args'' = out:args'
  liftIO $ createDirectoryIfMissing True dir
  liftIO $ putStrLn $ "args passed to actFn: " ++ show args''
  _ <- actFn cfg (CacheDir dir) (map ExprPath args'')
  trackWrite [out]
