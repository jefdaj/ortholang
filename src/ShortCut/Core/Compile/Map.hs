module ShortCut.Core.Compile.Map where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic

import Development.Shake.FilePath  ((</>), (<.>), takeDirectory)
import ShortCut.Core.Paths         (cacheDir, toCutPath, fromCutPath, exprPath,
                                    readPaths, writePaths, writeLits, CutPath,
                                    exprPathExplicit)
import ShortCut.Core.Debug         (debugAction, debugRules, debugTrackWrite, debug)
import ShortCut.Core.Config        (wrappedCmd)
import System.Directory            (createDirectoryIfMissing)
import System.FilePath             (takeBaseName, takeFileName)
import ShortCut.Core.Util          (digest, resolveSymlinks)
import Data.List (intersperse)
import Control.Monad (when)
import Text.PrettyPrint.HughesPJClass
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
    tmpFn args = do
      args' <- liftIO $ mapM (resolveSymlinks cfg . fromCutPath cfg) args
      let base = concat $ intersperse "_" $ map digest args'
          dir  = fromCutPath cfg $ cacheDir cfg tmpPrefix
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

-- TODO remove prefix now that it's unused
-- TODO and remove singleName too right?

{- This separately hooks up aMapElem to operate on any .args files, and aMap to
 - generate some .args files then gather them into the overall list. Those two
 - then need to agree on tmpfiles, communicating only through the .args files.
 -}
rMap :: ([CutPath] -> IO CutPath) -> (CutConfig -> CutPath -> [CutPath] -> Action ())
     -> String -> String -> RulesFn
rMap tmpFn actFn _ singleName s@(_,cfg) e@(CutFun r salt _ _ exprs) = do
  argInitPaths <- mapM (rExpr s) (init exprs)
  (ExprPath argsLastsPath) <- rExpr s (last exprs)
  let mainOutPath    = fromCutPath cfg $ exprPath s e
      argInitPaths'  = map (\(ExprPath p) -> toCutPath cfg p) argInitPaths
      argLastsPath'  = toCutPath cfg argsLastsPath
      elemCacheDir   = (fromCutPath cfg $ cacheDir cfg "map") </> digest e
      elemCacheDir'  = toCutPath cfg elemCacheDir -- TODO redundant?
      (ListOf eType) = debug cfg ("type of '" ++ render (pPrint e) ++ "' (" ++ show e ++ ") is " ++ show r) r -- TODO shit, can be a ref? or something
      elemCachePtn   = elemCacheDir </> "*" <.> extOf eType
  elemCachePtn %> aMapElem cfg eType tmpFn actFn singleName salt
  mainOutPath  %> aMap cfg argInitPaths' elemCacheDir' eType argLastsPath'
  return $ debugRules cfg "rMap" e $ ExprPath mainOutPath
rMap _ _ _ _ _ _ = error "bad argument to rMap"

{- This calls aMapArgs to leave a .args file for each set of args, then gathers
 - up the corresponding outPaths and returns a list of them.
 -}
aMap :: CutConfig
     -> [CutPath] -> CutPath -> CutType -> CutPath -> FilePath
     -> Action ()
aMap cfg inits mapTmp eType lastsPath outPath = do
  need inits'
  liftIO $ putStrLn $ "tmp': " ++ show tmp'
  liftIO $ putStrLn $ "inits': " ++ show inits'
  liftIO $ putStrLn $ "lasts': " ++ show lasts'
  liftIO $ putStrLn $ "eType: " ++ show eType
  inits'' <- liftIO $ mapM (resolveSymlinks cfg) inits'
  lastPaths <- readPaths cfg lasts' -- TODO this needs a lit variant?
  lastPaths' <- liftIO $ mapM (resolveSymlinks cfg) (map (fromCutPath cfg) lastPaths)
  mapM_ (aMapArgs cfg eType inits'' tmp') (map (toCutPath cfg) lastPaths')
  let outPaths  = map (mapPath tmp' eType) lastPaths'
  need outPaths
  outPaths' <- (fmap . map) (toCutPath cfg) $ liftIO $ mapM (resolveSymlinks cfg) outPaths
  let out = debugAction cfg "aMap" outPath (outPath:inits' ++ [tmp', lasts'])
  writePaths cfg out outPaths'
  where
    inits' = map (fromCutPath cfg) inits
    lasts' = fromCutPath cfg lastsPath
    tmp'   = fromCutPath cfg mapTmp

-- TODO take + return CutPaths?
mapPath :: FilePath -> CutType -> FilePath -> FilePath
mapPath tmpDir eType path = tmpDir </> digest path <.> extOf eType

{- This leaves arguments in .args files for aMapElem to find.
 -
 - TODO should it write "lits" when they're really more like heterogenous path lists?
 -}
aMapArgs :: CutConfig
         -> CutType -> [FilePath] -> FilePath -> CutPath
         -> Action ()
aMapArgs cfg eType inits' tmp' p = do
  let p'       = fromCutPath cfg p
      argsPath = mapPath tmp' eType p' <.> "args"
      argPaths = inits' ++ [p'] -- TODO abs path bug here?
  liftIO $ createDirectoryIfMissing True $ tmp'
  writeLits cfg argsPath argPaths

{- This gathers together Rules-time and Action-time arguments and passes
 - everything to actFn. To save on duplicated computation it writes the same
 - outfile that would have come from the equivalent non-mapped (single)
 - function if that doesn't exist yet, then links to it from the real outPath.
 - Shake will be suprised because the single outPath wasn't declared in any
 - Rule beforehand, but it should be able to adjust and skip repeating it when
 - the time comes.
 -
 - TODO does it have a race condition for writing the single file?
 - TODO ask ndmitchell if there's something much more elegant I'm missing
 - TODO any way to make that last FilePath into a CutPath? does it even matter?
 - TODO can actFn here be looked up from the individal fn itsef passed in the definition?
 - TODO after singleFn works, can we remove tmpFn? (ok if not)
 -}
aMapElem :: CutConfig -> CutType
         -> ([CutPath] -> IO CutPath)
         -> (CutConfig -> CutPath -> [CutPath] -> Action ())
         -> String -> Int -> FilePath -> Action ()
aMapElem cfg eType tmpFn actFn singleName salt out = do
  let argsPath = out <.> "args"
  args <- readPaths cfg argsPath
  let args' = map (fromCutPath cfg) args
  args'' <- liftIO $ mapM (resolveSymlinks cfg) args' -- TODO remove?
  need args'
  dir <- liftIO $ tmpFn args
  let dir' = fromCutPath cfg dir
      out' = debugAction cfg "aMapElem" out args''
  liftIO $ createDirectoryIfMissing True dir'
  --  TODO in order to match exprPath should this NOT follow symlinks?
  let hashes  = map (digest . toCutPath cfg) args'' -- TODO make it match exprPath
      single  = exprPathExplicit cfg singleName eType salt hashes
      single' = fromCutPath cfg single
      args''' = single:map (toCutPath cfg) args''
  liftIO $ createDirectoryIfMissing True $ takeDirectory single'
  done <- doesFileExist single'
  when (not done) (actFn cfg dir args''' >> trackWrite [single'])
  -- TODO utility/paths fn "symlink" (unless that's aLink?)
  unit $ quietly $ wrappedCmd cfg [out'] [] "ln" ["-fs", single', out']
  trackWrite [out']
