module ShortCut.Core.Compile.Map where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic

import Development.Shake.FilePath  ((</>), (<.>), (-<.>))
import ShortCut.Core.Paths         (cacheDir, toCutPath, fromCutPath, exprPath,
                                    readPaths, writePaths, writeLits, CutPath)
import ShortCut.Core.Debug         (debugAction, debugRules)
import System.Directory            (createDirectoryIfMissing)
import System.FilePath             (takeBaseName, takeFileName)
import ShortCut.Core.Util          (digest)

-----------------------------------------------------
-- simplified versions that take care of cache dir --
-----------------------------------------------------

rMapTmp :: (CutConfig -> CutPath -> [CutPath] -> Action ()) -> String -> RulesFn
rMapTmp actFn tmpPrefix s@(_,cfg) = mapFn s
  where
    tmpDir = cacheDir cfg tmpPrefix
    mapFn  = rMap (const tmpDir) actFn tmpPrefix

-- TODO use a hash for the cached path rather than the name, which changes!
-- takes an action fn and vectorizes the last arg (calls the fn with each of a
-- list of last args). returns a list of results. uses a new tmpDir each call.
rMapTmps :: (CutConfig -> CutPath -> [CutPath] -> Action ()) -> String -> RulesFn
rMapTmps fn tmpPrefix s@(_,cfg) e = rMap tmpFn fn tmpPrefix s e
  where
    -- TODO what if the same last arg is used in different mapping fns?
    --      will it be unique?
    tmpFn args = toCutPath cfg
               $ ((fromCutPath cfg $ cacheDir cfg tmpPrefix)) </> digest args

--------------------
-- main algorithm --
--------------------

{- This function is tricky because it has two separate parts that have to agree
 - on tmpfile names without talking to each other, and they also need to agree
 - with the non-mapped fn equivalents in the rest of the codebase. (Well they
 - don't have to, but if not work will be dupilcated)
 -
 - Normally I'd solve these problems by using expression paths, but there's an
 - extra constraint that the paths have to match up via a Shake pattern, which
 - means we can't know the files' contents or expressions--only the filenames
 - themselves.
 -
 - Given all that, the best I've been able to come up with is writing
 - intermediate ".args" files. Their filenames correspond to the expected
 - tmpfile (without ".args"), and when read they contain a list of paths to
 - pass the action to produce that tmpfile.
 -
 - But is there a simpler way? (Work on that later)
 -
 - TODO NEXT STEP: put the outtmp in the expr dir of the regular fn!
 -                 that way it'll automatically dedup at runtime right?
 -                 wait, how to prevent "does not exist" errors? low priority?
 -                 to do that, need to move/remove alternatives call
 -                 ... which doesn't work. fuck.
 -                 could hack it with extra ".done" files?
 -                 or hack it with alwaysRerun + doesFileExist?
 -                 or hack it with a set of "paths already used" in CutState?
 -                 or just keep to specific files rather than * patterns probably!
 -
 -                 also this might help:
 -                 expr paths should be based only on the expressions!
 -                 each module can dedup by content using content-hashed files too
 -                 those go in its cache dir and it just links to them as needed
 -
 -                 another way of putting it:
 -                 expr paths should be a Rules/compile-time thing
 -                 and cache paths an Actions/runtime thing
 -                 (distinction useful only if more than one expr per cache file?)
 -
 - TODO and the final outfile in the expr dir of the regular fn:
 -
 -      cache/<fnname>/<hash of non-mapped args>.each
 -                          |
 -                          V
 -      exprs/<fnname>/<hash of all args>.<ext>
 -
 -     That should be pretty doable as long as you change the outfile paths to
 -     use hashes of the individual args rather than the whole expression:
 -
 -     exprs/<fnname>/<arg1hash>_<arg2hash>_<arg3hash>.<ext>
 -
 -     Then in rMapArgs (rename it something better) you can calculate what
 -     the outpath will be and put the .args in its proper place, and in this
 -     main fn you can calculate it too to make the mapTmp pattern.
 -}

-- TODO centralize argsPath, which argPaths come from
-- TODO centralize outPaths?
-- TODO last of argPaths -> final out file?
-- TODO have aMapElem make a cached outfile, but also link to it from the expr path
--      that's inelegant but should solve the duplication right?

rMap :: ([CutPath] -> CutPath)
     -> (CutConfig -> CutPath -> [CutPath] -> Action ())
     -> String -> RulesFn
rMap tmpFn actFn prefix s@(_,cfg) e@(CutFun _ _ _ _ exprs) = do
  argInitPaths <- mapM (rExpr s) (init exprs)
  (ExprPath argsLastsPath) <- rExpr s (last exprs)
  let mainOutPath    = fromCutPath cfg $ exprPath s e
      argInitPaths'  = map (\(ExprPath p) -> toCutPath cfg p) argInitPaths
      argLastsPath'  = toCutPath cfg argsLastsPath
      elemCacheDir   = (fromCutPath cfg $ cacheDir cfg prefix) </> digest e
      elemCacheDir'  = toCutPath cfg elemCacheDir -- TODO redundant?
      elemCachePtn   = elemCacheDir </> "*" <.> extOf eType
      (ListOf eType) = typeOf e
  elemCachePtn %> aMapElem cfg tmpFn actFn
  mainOutPath  %> aMap cfg argInitPaths' elemCacheDir' eType argLastsPath'
  return $ debugRules cfg "rMap" e $ ExprPath mainOutPath
rMap _ _ _ _ _ = error "bad argument to rMapTmps"

-- This builds .args files then needs their actual non-.args outpaths, which
-- will be built by the action below
aMap :: CutConfig
     -> [CutPath] -> CutPath -> CutType -> CutPath -> FilePath
     -> Action ()
aMap cfg inits mapTmp eType lastsPath outPath = do
  lastPaths <- readPaths cfg lasts' -- TODO this needs a lit variant?
  mapM_ (aMapElemArgs cfg eType inits' tmp') lastPaths
  let outPaths  = map (\x -> tmp' </> takeBaseName x <.> extOf eType)
                      (map (fromCutPath cfg) lastPaths)
      outPaths' = map (toCutPath cfg) outPaths
  need outPaths
  let out = debugAction cfg "aMap" outPath (outPath:inits' ++ [tmp', lasts'])
  writePaths cfg out outPaths'
  where
    inits' = map (fromCutPath cfg) inits
    lasts' = fromCutPath cfg lastsPath
    tmp'   = fromCutPath cfg mapTmp

-- TODO can we remove eType here?
aMapElemArgs :: CutConfig
             -> CutType -> [FilePath] -> FilePath -> CutPath
             -> Action ()
aMapElemArgs cfg eType inits' tmp' p = do
  -- TODO write the out path here too so all the args are together?
  let p'       = fromCutPath cfg p
      -- TODO use a hash here?
      argsPath = tmp' </> takeFileName p' -<.> extOf eType <.> "args"
      argPaths = inits' ++ [p'] -- TODO abs path bug here?
  liftIO $ createDirectoryIfMissing True $ tmp'
  -- TODO these aren't lits! are they strings then?
  writeLits cfg argsPath argPaths

-- This builds one of the list of out paths based on a .args file
-- (made in the action above). It's a pretty roundabout way to do it!
-- TODO ask ndmitchell if there's something much more elegant I'm missing
-- TODO any way to make that last FilePath into a CutPath? does it even matter?
aMapElem :: CutConfig
         -> ([CutPath] -> CutPath)
         -> (CutConfig -> CutPath -> [CutPath] -> Action a)
         -> FilePath -> Action ()
aMapElem cfg tmpFn actFn out = do
  let argsPath = out <.> "args" -- TODO clean up
  -- liftIO $ putStrLn $ "argsPath (aMapElem): " ++ argsPath
  args <- readPaths cfg argsPath
  let args' = map (fromCutPath cfg) args
  -- liftIO $ putStrLn $ "argPaths (aMapElem): " ++ show args'
  need args'
  -- TODO fix actual tmpFns to use CutPaths (automatically deterministic!)
  let dir = tmpFn args
      dir' = fromCutPath cfg dir
      args'' = (toCutPath cfg out):args
      out'  = debugAction cfg "aMapElem" out args' -- TODO is this right?
  liftIO $ createDirectoryIfMissing True dir'
  -- liftIO $ putStrLn $ "out' (aMapElem): " ++ show out'
  -- liftIO $ putStrLn $ "args'': " ++ show args''
  _ <- actFn cfg dir args''
  trackWrite [out']
