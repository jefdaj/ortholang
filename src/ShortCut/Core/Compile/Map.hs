module ShortCut.Core.Compile.Map where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic

import Development.Shake.FilePath  ((</>), (<.>))
import ShortCut.Core.Paths         (cacheDir, toCutPath, fromCutPath, exprPath,
                                    readPaths, writePaths, writeLits)
import ShortCut.Core.Debug         (debugAction, debugRules)
import System.Directory            (createDirectoryIfMissing)
import System.FilePath             (takeBaseName, makeRelative)
import ShortCut.Core.Util          (digest)

-----------------------------------------------------
-- simplified versions that take care of cache dir --
-----------------------------------------------------

rMapLastTmp :: ActionFn -> String -> RulesFn
rMapLastTmp actFn tmpPrefix s@(_,cfg) = mapFn s
  where
    tmpDir = CacheDir $ fromCutPath cfg $ cacheDir cfg tmpPrefix
    mapFn  = rMapLast (const tmpDir) actFn tmpPrefix

-- TODO use a hash for the cached path rather than the name, which changes!

-- takes an action fn and vectorizes the last arg (calls the fn with each of a
-- list of last args). returns a list of results. uses a new tmpDir each call.
rMapLastTmps :: ActionFn -> String -> RulesFn
rMapLastTmps fn tmpPrefix s@(_,cfg) e = rMapLast tmpFn fn tmpPrefix s e
  where
    -- TODO what if the same last arg is used in different mapping fns?
    --      will it be unique?
    tmpFn args = CacheDir $ fromCutPath cfg (cacheDir cfg tmpPrefix) </> digest args

--------------------
-- main algorithm --
--------------------

-- TODO rename to be clearly "each"-related and use .each for the map files
--
-- TODO put the .each in the cachedir of the regular fn
-- TODO and the final outfile in the expr dir of the regular fn:
--
--      cache/<fnname>/<hash of non-mapped args>.each
--                          |
--                          V
--      exprs/<fnname>/<hash of all args>.<ext>
--
--     That should be pretty doable as long as you change the outfile paths to
--     use hashes of the individual args rather than the whole expression:
--
--     exprs/<fnname>/<arg1hash>_<arg2hash>_<arg3hash>.<ext>
--
--     Then in rMapLastArgs (rename it something better) you can calculate what
--     the outpath will be and put the .args in its proper place, and in this
--     main fn you can calculate it too to make the mapTmp pattern.

-- TODO is the rtnType doing anything that you can't get from CutFun?
rMapLast :: ([FilePath] -> CacheDir) -> ActionFn -> String -> RulesFn
rMapLast tmpFn actFn prefix s@(_,cfg) e@(CutFun _ _ _ _ exprs) = do
  -- TODO make this an actual debug call
  -- liftIO $ putStrLn $ "rMapLast expr: " ++ render (pPrint e)
  initPaths <- mapM (rExpr s) (init exprs)
  (ExprPath lastsPath) <- rExpr s (last exprs)
  let inits = map (\(ExprPath p) -> p) initPaths
      outPath = fromCutPath cfg $ exprPath s e
      mapTmp = (fromCutPath cfg $ cacheDir cfg prefix) </> digest e
  -- This builds .args files then needs their actual non-.args outpaths, which
  -- will be built by the action below
  outPath %> \_ -> aMapLastArgs cfg outPath inits mapTmp lastsPath
  -- This builds one of the list of out paths based on a .args file
  -- (made in the action above). It's a pretty roundabout way to do it!
  -- TODO ask ndmitchell if there's something much more elegant I'm missing
  (mapTmp </> "*") %> aMapLastMapTmp cfg tmpFn actFn
  return $ debugRules cfg "rMapLast" e $ ExprPath outPath
rMapLast _ _ _ _ _ = error "bad argument to rMapLastTmps"

aMapLastArgs :: CutConfig -> FilePath -> [FilePath]
             -> FilePath -> FilePath -> Action ()
aMapLastArgs cfg outPath inits mapTmp lastsPath = do
  lastPaths <- readPaths cfg lastsPath -- TODO this needs a lit variant?
  -- this writes the .args files for use in the rule above
  (flip mapM_) lastPaths $ \p -> do
    -- TODO write the out path here too so all the args are together?
    liftIO $ putStrLn $ "p: " ++ show p
    let p'       = fromCutPath cfg p
        argsPath = mapTmp </> takeBaseName p' <.> "args" -- TODO use a hash here?
        argPaths = inits ++ [p'] -- TODO abs path bug here?
    -- liftIO $ putStrLn $ "p: " ++ show p'
    liftIO $ createDirectoryIfMissing True $ mapTmp
    writeLits cfg argsPath argPaths
  -- then we just trigger them and write to the overall outPath
  let outPaths  = map (\x -> mapTmp </> takeBaseName x) (map (fromCutPath cfg) lastPaths)
      outPaths' = map (toCutPath cfg) outPaths
  need outPaths
  let out = debugAction cfg "aMapLastArgs" outPath (outPath:inits ++ [mapTmp, lastsPath])
  writePaths cfg out outPaths'

-- TODO rename this something less confusing
aMapLastMapTmp :: CutConfig
               -> ([FilePath] -> CacheDir)
               -> (CutConfig -> CacheDir -> [ExprPath] -> Action a)
               -> FilePath -> Action ()
aMapLastMapTmp cfg tmpFn actFn out = do
  let argsPath = out <.> ".args" -- TODO clean up
  args <- fmap lines $ liftIO $ readFile argsPath -- TODO switch to readPaths?
  let args' = map (cfgTmpDir cfg </>) args
      rels  = map (makeRelative $ cfgTmpDir cfg) args
  need args'
  let (CacheDir dir) = tmpFn rels -- relative paths for determinism!
      args'' = out:args'
      out'   = debugAction cfg "aMapLastMapTmp" out args' -- TODO is this right?
  liftIO $ createDirectoryIfMissing True dir
  liftIO $ putStrLn $ "args passed to actFn: " ++ show args''
  _ <- actFn cfg (CacheDir dir) (map ExprPath args'')
  trackWrite [out']
