module ShortCut.Core.Compile.Map where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic

import Development.Shake.FilePath  ((</>), (<.>), (-<.>))
import ShortCut.Core.Paths         (cacheDir, toCutPath, fromCutPath, exprPath,
                                    readPaths, writePaths, writeLits, CutPath,
                                    exprPathExplicit, resolveVars)
import ShortCut.Core.Debug         (debugAction, debugRules, debugTrackWrite)
import ShortCut.Core.Config        (wrappedCmd)
import System.Directory            (createDirectoryIfMissing)
import System.FilePath             (takeBaseName, takeFileName, takeDirectory)
import ShortCut.Core.Util          (digest, resolveSymlinks)
import Data.List (intersperse)
-- import Data.List.Utils (replace)

-----------------------------------------------------
-- simplified versions that take care of cache dir --
-----------------------------------------------------

-- TODO remove?
rMapTmp :: (CutConfig -> CutPath -> [CutPath] -> Action ()) -> String -> String -> RulesFn
rMapTmp actFn tmpPrefix singleName s@(_,cfg) = mapFn singleName s
  where
    tmpDir = cacheDir cfg tmpPrefix
    mapFn  = rMap (return . const tmpDir) actFn tmpPrefix

-- TODO use a hash for the cached path rather than the name, which changes!
-- takes an action fn and vectorizes the last arg (calls the fn with each of a
-- list of last args). returns a list of results. uses a new tmpDir each call.
rMapTmps :: (CutConfig -> CutPath -> [CutPath] -> Action ())
         -> String -> String -> RulesFn
rMapTmps actFn tmpPrefix singleName s@(_,cfg) e =
  rMap tmpFn actFn tmpPrefix singleName s e
  where
    hashes args = fmap (concat . intersperse "_" . map digest)
                       (resolveVars cfg args)
    tmpFn args = do
      base <- hashes args
      let dir = fromCutPath cfg $ cacheDir cfg tmpPrefix
      return $ toCutPath cfg (dir </> base)

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

rMap :: ([CutPath] -> IO CutPath) -> (CutConfig -> CutPath -> [CutPath] -> Action ()) -> String -> String -> RulesFn
rMap tmpFn actFn prefix singleName s@(_,cfg) e@(CutFun _ salt _ _ exprs) = do
  argInitPaths <- mapM (rExpr s) (init exprs)
  (ExprPath argsLastsPath) <- rExpr s (last exprs)
  let mainOutPath    = fromCutPath cfg $ exprPath s e
      argInitPaths'  = map (\(ExprPath p) -> toCutPath cfg p) argInitPaths
      argLastsPath'  = toCutPath cfg argsLastsPath
      elemCacheDir   = (fromCutPath cfg $ cacheDir cfg prefix) </> digest e
      elemCacheDir'  = toCutPath cfg elemCacheDir -- TODO redundant?
      elemCachePtn   = elemCacheDir </> "*" <.> extOf eType
      (ListOf eType) = typeOf e
  elemCachePtn %> aMapElem cfg eType tmpFn actFn singleName salt
  mainOutPath  %> aMap cfg argInitPaths' elemCacheDir' eType argLastsPath'
  return $ debugRules cfg "rMap" e $ ExprPath mainOutPath
rMap _ _ _ _ _ _ = error "bad argument to rMapTmps"

-- This builds .args files then needs their actual non-.args outpaths, which
-- will be built by the action below
aMap :: CutConfig
     -> [CutPath] -> CutPath -> CutType -> CutPath -> FilePath
     -> Action ()
aMap cfg inits mapTmp eType lastsPath outPath = do
  need inits'
  inits'' <- (fmap . map) (fromCutPath cfg) (liftIO $ resolveVars cfg inits)
  lastPaths <- readPaths cfg lasts' -- TODO this needs a lit variant?
  lastPaths' <- liftIO $ resolveVars cfg lastPaths
  mapM_ (aMapElemArgs cfg eType inits'' tmp') lastPaths'
  let outPaths  = map (\x -> tmp' </> takeBaseName x <.> extOf eType)
                      (map (fromCutPath cfg) lastPaths')
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
      -- ePath    = p' -<.> extOf eType
      argsPath = tmp' </> takeFileName p' -<.> extOf eType <.> "args"
      argPaths = inits' ++ [p'] -- TODO abs path bug here?
  liftIO $ putStrLn $ "p': " ++ p'
  liftIO $ putStrLn $ "tmp': " ++ tmp'
  -- liftIO $ putStrLn $ "ePath: " ++ ePath
  liftIO $ putStrLn $ "argsPath: " ++ argsPath
  -- liftIO $ putStrLn $ "link " ++ argsPath ++ " -> " ++ ePath
  liftIO $ createDirectoryIfMissing True $ tmp'
  -- These aren't really lits so much as a heterogenous list of path types, but
  -- this function still works OK so I haven't changed it to be more explicit
  -- yet. They do need the same checking I guess?
  writeLits cfg argsPath argPaths

-- Creates a symlink from what would be the expression path of the single fn to
-- the equivalent map tmpfile. It's a surprise in that Shake doesn't know it's
-- coming when calculating rule dependencies, but that should be OK because
-- it'll just adjust by skipping either this or the single rule, whichever
-- comes second.
-- aMapSurpriseElem :: CutConfig -> FilePath -> FilePath -> CutType -> String -> Action ()
-- aMapSurpriseElem cfg out' lastPath eType singleName = do
aMapSurpriseElem :: CutConfig
                 -> FilePath -> [FilePath] -> CutType -> String
                 -> Int -> Action ()
aMapSurpriseElem cfg out argPaths eType singleName salt = do
  -- make an additional symlink from what would be the single fn outpath
  -- (this is a hack to prevent duplicate work, but is also confusing!)
  -- TODO only when it doesn't exist yet!
  -- TODO pass singleName explicitly from the fn definitions
  -- let singleName = replace "_each" "" name -- TODO use singleName here
  -- let single = (replace mapName singleName lastPath) -<.> extOf eType -- TODO fix using singleName!
  -- let single = cfgTmpDir cfg </> "exprs" </> singleName </> takeFileName lastPath -<.> extOf eType
-- exprPathExplicit (_, cfg) prefix rtype salt hashes = toCutPath cfg path
  let hashes = map digest argPaths
      single = fromCutPath cfg $ exprPathExplicit cfg singleName eType salt hashes
  liftIO $ putStrLn $ "argPaths: " ++ show argPaths
  liftIO $ putStrLn $ "hashes: " ++ show hashes
  liftIO $ putStrLn $ "single: " ++ single
  -- TODO should link to for example exprs/crb_blast/<hash>.str.list
  --      whereas now it's cache/tables or whatever
  -- liftIO $ putStrLn $ "mapName: " ++ mapName
  -- liftIO $ putStrLn $ "singleName: " ++ singleName
  liftIO $ putStrLn $ "link " ++ out ++ " -> " ++ single
  liftIO $ createDirectoryIfMissing True $ takeDirectory single
  out' <- liftIO $ resolveSymlinks cfg out
  unit $ quietly $ wrappedCmd cfg [out'] [] "ln" ["-fs", out', single]
  debugTrackWrite cfg [out']

-- This builds one of the list of out paths based on a .args file
-- (made in the action above). It's a pretty roundabout way to do it!
-- TODO ask ndmitchell if there's something much more elegant I'm missing
-- TODO any way to make that last FilePath into a CutPath? does it even matter?
-- TODO can actFn here be looked up from the individal fn itsef passed in the definition?
-- TODO after singleFn works, can we remove tmpFn? (ok if not)
aMapElem :: CutConfig -> CutType
         -> ([CutPath] -> IO CutPath)
         -> (CutConfig -> CutPath -> [CutPath] -> Action ())
         -> String -> Int -> FilePath -> Action ()
aMapElem cfg eType tmpFn actFn singleName salt out = do
  let argsPath = out <.> "args" -- TODO clean up
  -- liftIO $ putStrLn $ "argsPath (aMapElem): " ++ argsPath
  args <- readPaths cfg argsPath
  args' <- liftIO $ resolveVars cfg args -- TODO remove?
  let args'' = map (fromCutPath cfg) args'
  -- liftIO $ putStrLn $ "argPaths (aMapElem): " ++ show args''
  need args''
  -- TODO fix actual tmpFns to use CutPaths (automatically deterministic!)
  dir <- liftIO $ tmpFn args
  let dir'   = fromCutPath cfg dir
      args''' = (toCutPath cfg out):args
      out'   = debugAction cfg "aMapElem" out args'' -- TODO is this right?
  -- args''' <- liftIO $ resolveVars cfg args'' -- TODO does this go in Basic.hs?
  liftIO $ createDirectoryIfMissing True dir'
  -- liftIO $ putStrLn $ "args'': " ++ show args''
  _ <- actFn cfg dir args'''
  trackWrite [out']
  aMapSurpriseElem cfg out' args'' eType singleName salt
