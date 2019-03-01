module ShortCut.Core.Compile.Map
  ( rMap
  , rMapTmp
  , rMapTmps
  , rMapSimpleScript
  )
  where

-- If all goes well, this module will soon be phased out in favor of Compile.Map
-- TODO also it's named badly since this isn't vectorization!

import Development.Shake
import ShortCut.Core.Compile.Basic
import ShortCut.Core.Types
import Text.PrettyPrint.HughesPJClass

import Data.List                  (intersperse)
import Data.List.Utils            (replace)
import Development.Shake.FilePath ((</>), (<.>))
import ShortCut.Core.Actions      (readPaths, writePaths, symlink,
                                   readLit, writeLits, debugA, debugL, debugNeed)
import ShortCut.Core.Paths        (cacheDir, toCutPath, fromCutPath, exprPath,
                                   CutPath, exprPathExplicit, argHashes)
import ShortCut.Core.Util         (digest, resolveSymlinks, unlessExists,
                                   popFrom, insertAt)
import System.Directory           (createDirectoryIfMissing)

------------------------------------
-- simplified versions for export --
------------------------------------

-- for action functions that don't need a tmpdir
rMap :: Int -> (CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()) -> RulesFn
rMap index actFn = rVecMain index Nothing actFn'
  where
    actFn' cfg ref ids _ args = actFn cfg ref ids args -- drops unused tmpdir

-- for action functions that need one tmpdir reused between calls
rMapTmp :: Int -> (CutConfig -> Locks -> HashedSeqIDsRef -> CutPath -> [CutPath] -> Action ())
        -> String -> RulesFn
rMapTmp index actFn tmpPrefix s@(_, cfg, _, _) = rVecMain index (Just tmpFn) actFn s
  where
    tmpDir = cacheDir cfg tmpPrefix
    tmpFn  = return . const tmpDir

-- for action functions that need a unique tmpdir each call
-- TODO use a hash for the cached path rather than the name, which changes!
rMapTmps :: Int
          -> (CutConfig -> Locks -> HashedSeqIDsRef -> CutPath -> [CutPath] -> Action ())
          -> String -> RulesFn
rMapTmps index actFn tmpPrefix s@(_, cfg, _, _) e = rVecMain index (Just tmpFn) actFn s e
  where
    tmpFn args = do
      let base = concat $ intersperse "_" $ map digest args
          dir  = fromCutPath cfg $ cacheDir cfg tmpPrefix
      return $ toCutPath cfg (dir </> base)

{- Like rSimpleScript, but the last argument should be a list.
 - It will be evaluated and one call made to aSimpleScript with each element.
 -}
rMapSimpleScript :: Int -> String -> RulesFn
rMapSimpleScript index = rMap index . aSimpleScript

--------------------
-- main algorithm --
--------------------

{- This separately hooks up aVecElem to operate on any .args files, and aVec to
 - generate some .args files then gather them into the overall list. Those two
 - then need to agree on tmpfiles, communicating only through the .args files.
 -
 - The main reason for such trickiness is that things need to be hooked up in
 - the Rules monad, before knowing the contents of the list that will be mapped
 - over. Given that I'm not sure there's any way to avoid intermediate files,
 - but am open to alternatives if anyone thinks of something!
 -}
rVecMain :: Int -> Maybe ([CutPath] -> IO CutPath)
         -> (CutConfig -> Locks -> HashedSeqIDsRef -> CutPath -> [CutPath] -> Action ())
         -> RulesFn
rVecMain mapIndex mTmpFn actFn s@(_, cfg, ref, ids) e@(CutFun r seed _ name exprs) = do
  let mapIndex' = mapIndex - 1 -- index arguments from 1 rather than 0
      (mappedExpr, regularExprs) = popFrom mapIndex' exprs
  regularArgPaths <- mapM (rExpr s) regularExprs
  (ExprPath mappedArgsPath) <- rExpr s mappedExpr
  let singleName     = replace "_each" "" name
      mainOutPath    = fromCutPath cfg $ exprPath s e
      regularArgPaths'  = map (\(ExprPath p) -> toCutPath cfg p) regularArgPaths
      argLastsPath'  = toCutPath cfg mappedArgsPath
      elemCacheDir   = (fromCutPath cfg $ cacheDir cfg "each") </> hashFun s e
      elemCacheDir'  = toCutPath cfg elemCacheDir -- TODO redundant?
      elemCachePtn   = elemCacheDir </> "*" <.> extOf eType
      (ListOf eType) = debug cfg ("type of '" ++ render (pPrint e)
                                  ++ "' (" ++ show e ++ ") is " ++ show r) r
  elemCachePtn %> aVecElem cfg ref ids eType mTmpFn actFn singleName seed
  mainOutPath  %> aVecMain cfg ref ids mapIndex' regularArgPaths' elemCacheDir' eType argLastsPath'
  return $ debugRules cfg "rVecMain" e $ ExprPath mainOutPath
rVecMain _ _ _ _ _ = fail "bad argument to rVecMain"

hashFun :: CutState -> CutExpr -> String
hashFun st e@(CutFun _ s _ n _) = digest $ [n, show s] ++ argHashes st e
hashFun _ _ = error "hashFun only hashes function calls so far"

{- This calls aVecArgs to leave a .args file for each set of args, then gathers
 - up the corresponding outPaths and returns a list of them.
 -}
aVecMain :: CutConfig -> Locks -> HashedSeqIDsRef -> Int
         -> [CutPath] -> CutPath -> CutType -> CutPath -> FilePath
         -> Action ()
aVecMain cfg ref ids mapIndex regularArgs mapTmpDir eType mappedArg outPath = do
  debugNeed cfg "aVecMain" regularArgs'
  let resolve = resolveSymlinks $ Just $ cfgTmpDir cfg
  regularArgs'' <- liftIO $ mapM resolve regularArgs'
  mappedPaths  <- readPaths cfg ref mappedArgList'
  mappedPaths' <- liftIO $ mapM resolve $ map (fromCutPath cfg) mappedPaths
  debugL cfg $ "aVecMain mappedPaths': " ++ show mappedPaths'
  mapM_ (aVecArgs cfg ref ids mapIndex eType regularArgs'' mapTmpDir')
        (map (toCutPath cfg) mappedPaths') -- TODO wrong if lits?
  let outPaths = map (eachPath cfg mapTmpDir' eType) mappedPaths'
  debugNeed cfg "aVecMain" outPaths
  outPaths' <- liftIO $ mapM resolve outPaths
  let out = debugA cfg "aVecMain" outPath
              (outPath:regularArgs' ++ [mapTmpDir', mappedArgList'])
  if eType `elem` [str, num]
    then mapM (readLit cfg ref) outPaths' >>= writeLits cfg ref out
    else writePaths cfg ref out $ map (toCutPath cfg) outPaths'
  where
    regularArgs'   = map (fromCutPath cfg) regularArgs
    mappedArgList' = fromCutPath cfg mappedArg
    mapTmpDir'     = fromCutPath cfg mapTmpDir

-- TODO take + return CutPaths?
-- TODO blast really might be nondeterministic here now that paths are hashed!
eachPath :: CutConfig -> FilePath -> CutType -> FilePath -> FilePath
eachPath cfg tmpDir eType path = tmpDir </> hash' <.> extOf eType
  where
    path' = toCutPath cfg path
    hash  = digest path'
    hash' = debug cfg ("hash of " ++ show path' ++ " is " ++ hash) hash

-- This leaves arguments in .args files for aVecElem to find.
-- TODO put mapIndex and mappedArg together, and rename that something with path
aVecArgs :: CutConfig -> Locks -> HashedSeqIDsRef -> Int
         -> CutType -> [FilePath] -> FilePath -> CutPath
         -> Action ()
aVecArgs cfg ref _ mapIndex eType regularArgs' tmp' mappedArg = do
  let mappedArg' = fromCutPath cfg mappedArg
      argsPath   = eachPath cfg tmp' eType mappedArg' <.> "args"
      -- argPaths   = regularArgs' ++ [mappedArg'] -- TODO abs path bug here?
      argPaths   = insertAt mapIndex mappedArg' regularArgs'
      argPaths'  = map (toCutPath cfg) argPaths
  debugL cfg $ "aVecArgs mappedArg': " ++ show mappedArg'
  debugL cfg $ "aVecArgs argsPath: " ++ show argsPath
  debugL cfg $ "aVecArgs argPaths: " ++ show argPaths
  debugL cfg $ "aVecArgs argPaths': " ++ show argPaths'
  writePaths cfg ref argsPath argPaths'

{- This gathers together Rules-time and Action-time arguments and passes
 - everything to actFn. To save on duplicated computation it writes the same
 - outfile that would have come from the equivalent non-mapped (single)
 - function if that doesn't exist yet, then links to it from the real outPath.
 - Shake will be suprised because the single outPath wasn't declared in any
 - Rule beforehand, but it should be able to adjust and skip repeating it when
 - the time comes.
 -
 - TODO does it have a race condition for writing the single file?
 - TODO any way to make that last FilePath into a CutPath? does it even matter?
 - TODO can actFn here be looked up from the individal fn itsef passed in the definition?
 - TODO after singleFn works, can we remove tmpFn? (ok if not)
 -}
aVecElem :: CutConfig -> Locks -> HashedSeqIDsRef -> CutType
         -> Maybe ([CutPath] -> IO CutPath)
         -> (CutConfig -> Locks -> HashedSeqIDsRef -> CutPath -> [CutPath] -> Action ())
         -> String -> RandomSeed -> FilePath -> Action ()
aVecElem cfg ref ids eType tmpFn actFn singleName seed out = do
  let argsPath = out <.> "args"
  args <- readPaths cfg ref argsPath
  let args' = map (fromCutPath cfg) args
  args'' <- liftIO $ mapM (resolveSymlinks $ Just $ cfgTmpDir cfg) args' -- TODO remove?
  debugNeed cfg "aVecElem" args'
  dir <- liftIO $ case tmpFn of
    Nothing -> return $ cacheDir cfg "each" -- TODO any better option than this or undefined?
    Just fn -> do
      d <- fn args
      let d' = fromCutPath cfg d
      createDirectoryIfMissing True d'
      return d
  let out' = debugA cfg "aVecElem" (toCutPath cfg out) args''
      -- TODO in order to match exprPath should this NOT follow symlinks?
      hashes  = map (digest . toCutPath cfg) args'' -- TODO make it match exprPath
      single  = exprPathExplicit cfg singleName eType seed hashes
      single' = fromCutPath cfg single
      args''' = single:map (toCutPath cfg) args''
  -- TODO any risk of single' being made after we test for it here?
  unlessExists single' $ actFn cfg ref ids dir args'''
  symlink cfg ref out' single
