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
 - But is there a simpler way?
 -
 - TODO rename to be clearly "each"-related and use .each for the map files
 -
 - TODO put the .each in the cachedir of the regular fn
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

rMap :: ([CutPath] -> CutPath)
         -> (CutConfig -> CutPath -> [CutPath] -> Action ())
         -> String -> RulesFn
rMap tmpFn actFn prefix s@(_,cfg) e@(CutFun _ _ _ _ exprs) = do
  -- TODO make this an actual debug call
  -- liftIO $ putStrLn $ "rMap expr: " ++ render (pPrint e)
  initPaths <- mapM (rExpr s) (init exprs)
  (ExprPath lastsPath) <- rExpr s (last exprs)
  let inits    = map (\(ExprPath p) -> toCutPath cfg p) initPaths
      lasts'   = toCutPath cfg lastsPath
      outPath  = exprPath s e
      outPath' = fromCutPath cfg outPath
      (ListOf t) = typeOf e
      mapTmp   = (fromCutPath cfg $ cacheDir cfg prefix) </> digest e -- <.> extOf t
      mapTmp'  = toCutPath cfg mapTmp
  -- This builds .args files then needs their actual non-.args outpaths, which
  -- will be built by the action below
  outPath' %> \_ -> aMapArgs cfg outPath inits mapTmp' t lasts'
  -- This builds one of the list of out paths based on a .args file
  -- (made in the action above). It's a pretty roundabout way to do it!
  -- TODO ask ndmitchell if there's something much more elegant I'm missing
  (mapTmp </> "*" <.> extOf t) %> aMapTmp cfg tmpFn actFn
  return $ debugRules cfg "rMap" e $ ExprPath outPath'
rMap _ _ _ _ _ = error "bad argument to rMapTmps"

aMapArgs :: CutConfig -> CutPath -> [CutPath]
             -> CutPath -> CutType -> CutPath -> Action ()
aMapArgs cfg outPath inits mapTmp etype lastsPath = do
  lastPaths <- readPaths cfg lasts' -- TODO this needs a lit variant?
  -- this writes the .args files for use in the rule above
  (flip mapM_) lastPaths $ \p -> do
    -- TODO write the out path here too so all the args are together?
    let p'       = fromCutPath cfg p
        -- TODO use a hash here?
        argsPath = tmp' </> takeFileName p' -<.> extOf etype <.> "args"
        argPaths = inits' ++ [p'] -- TODO abs path bug here?
    liftIO $ createDirectoryIfMissing True $ tmp'
    -- TODO these aren't lits! are they strings then?
    writeLits cfg argsPath argPaths
  -- then we just trigger them and write to the overall outPath
  let outPaths  = map (\x -> tmp' </> takeBaseName x <.> extOf etype)
                      (map (fromCutPath cfg) lastPaths)
      outPaths' = map (toCutPath cfg) outPaths
  need outPaths
  let out = debugAction cfg "aMapArgs" out' (out':inits' ++ [tmp', lasts'])
  writePaths cfg out outPaths'
  where
    out'   = fromCutPath cfg outPath
    inits' = map (fromCutPath cfg) inits
    lasts' = fromCutPath cfg lastsPath
    tmp'   = fromCutPath cfg mapTmp

-- TODO any way to make that last FilePath into a CutPath? does it even matter?
aMapTmp :: CutConfig
               -> ([CutPath] -> CutPath)
               -> (CutConfig -> CutPath -> [CutPath] -> Action a)
               -> FilePath -> Action ()
aMapTmp cfg tmpFn actFn out = do
  let argsPath = out <.> "args" -- TODO clean up
  args <- readPaths cfg argsPath
  let args' = map (fromCutPath cfg) args
  need args'
  -- TODO fix actual tmpFns to use CutPaths (automatically deterministic!)
  let dir = tmpFn args
      dir' = fromCutPath cfg dir
      args'' = (toCutPath cfg out):args
      out'  = debugAction cfg "aMapTmp" out args' -- TODO is this right?
  liftIO $ createDirectoryIfMissing True dir'
  _ <- actFn cfg dir args''
  trackWrite [out']
