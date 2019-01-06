
-- module "compiles" it by translating it into a set of Shake build rules. To
-- actually run the rules, use `eval` in the Interpret module.

-- TODO add more descriptive runtime error for canonicalizePath failing b/c no file
-- TODO see if you can avoid making more than one absolute symlink per input file
-- TODO make systematically sure there's only one rule for each file
-- TODO pass tmpDir as a config option somehow, and verbosity

-- TODO why doesn't turning down the verbosity actually work?

module Detourrr.Core.Compile.Basic
--   ( compileScript
--   , rBop
--   , rExpr
--   , rList
--   , addPrefixes
--   )
  where

-- TODO does turning of traces radically speed up the interpreter?
import Debug.Trace       (trace)

import Development.Shake
import Detourrr.Core.Types
import Detourrr.Core.Pretty
import qualified Data.Map as M

import Detourrr.Core.Paths (cacheDir, exprPath, exprPathExplicit, toRrrPath,
                            fromRrrPath, varPath, RrrPath)

import Data.IORef                 (atomicModifyIORef)
import Data.List                  (intersperse)
import Development.Shake.FilePath ((</>), (<.>))
import Detourrr.Core.Actions      (wrappedCmdWrite, debugA, debugL, debugNeed,
                                   readLit, readLits, writeLit, writeLits, hashContent,
                                   readLitPaths, hashContent, writePaths, symlink)
import Detourrr.Core.Locks        (withWriteLock')
import Detourrr.Core.Sanitize     (hashIDsFile, writeHashedIDs, readHashedIDs)
import Detourrr.Core.Util         (absolutize, resolveSymlinks, stripWhiteSpace,
                                   digest, removeIfExists)
import System.FilePath            (takeExtension)


debug :: RrrConfig -> String -> a -> a
debug cfg msg rtn = if cfgDebug cfg then trace msg rtn else rtn

-- TODO restrict to RrrExpr?
-- TODO put in rExpr to catch everything at once? but misses which fn was called
debugRules :: (Pretty a, Show b) => RrrConfig -> String -> a -> b -> b
debugRules cfg name input out = debug cfg msg out
  where
    ren = render $ pPrint input
    msg = name ++ " compiled '" ++ ren ++ "' to " ++ show out

------------------------------
-- compile the Detourrr AST --
------------------------------

rExpr :: RrrState -> RrrExpr -> Rules ExprPath
rExpr s e@(RrrLit _ _ _      ) = rLit s e
rExpr s e@(RrrRef _ _ _ _    ) = rRef s e
rExpr s e@(RrrList _ _ _ _   ) = rList s e
rExpr s e@(RrrBop _ _ _ n _ _) = rulesByName s e n -- TODO turn into Fun?
rExpr s e@(RrrFun _ _ _ n _  ) = rulesByName s e n
rExpr _   (RrrRules (CompiledExpr _ rules)) = rules

-- TODO remove once no longer needed (parser should find fns)
rulesByName :: RrrState -> RrrExpr -> String -> Rules ExprPath
rulesByName s@(_, cfg, _, _) expr name = case findFunction cfg name of
  Nothing -> error $ "no such function '" ++ name ++ "'"
  Just f  -> (fRules f) s expr

rAssign :: RrrState -> RrrAssign -> Rules (RrrVar, VarPath)
rAssign s@(_, cfg, _, _) (var, expr) = do
  (ExprPath path) <- rExpr s expr
  path' <- rVar s var expr $ toRrrPath cfg path
  let res  = (var, path')
      res' = debugRules cfg "rAssign" (var, expr) res
  return res'

-- TODO how to fail if the var doesn't exist??
--      (or, is that not possible for a typechecked AST?)
compileScript :: RrrState -> Maybe String -> Rules ResPath
compileScript s@(as, _, _, _) permHash = do
  -- TODO this can't be done all in parallel because they depend on each other,
  --      but can parts of it be parallelized? or maybe it doesn't matter because
  --      evaluating the code itself is always faster than the system commands
  rpaths <- mapM (rAssign s) as
  case lookup (RrrVar res) rpaths of
    Nothing -> fail "no result variable. that's not right!"
    Just (VarPath r) -> return $ ResPath r
  where
    -- p here is "result" + the permutation name/hash if there is one right?
    res = case permHash of
      Nothing -> "result"
      Just h  -> "result." ++ h

-- write a literal value from Detourrr source code to file
rLit :: RrrState -> RrrExpr -> Rules ExprPath
rLit s@(_, cfg, ref, ids) expr = do
  let path  = exprPath s expr -- absolute paths allowed!
      path' = debugRules cfg "rLit" expr $ fromRrrPath cfg path
  path' %> \_ -> aLit cfg ref ids expr path
  return (ExprPath path')

-- TODO take the path, not the expression?
aLit :: RrrConfig -> Locks -> HashedSeqIDsRef -> RrrExpr -> RrrPath -> Action ()
aLit cfg ref _ expr out = writeLit cfg ref out'' ePath -- TODO too much dedup?
  where
    paths :: RrrExpr -> FilePath
    paths (RrrLit _ _ p) = p
    paths _ = error "bad argument to paths"
    ePath = paths expr
    out'  = fromRrrPath cfg out
    out'' = debugA cfg "aLit" out' [ePath, out']

rList :: RrrState -> RrrExpr -> Rules ExprPath
-- TODO is this the bug? refers to a list of other empty lists, no?
-- rList s e@(RrrList Empty _ _ _) = rListLits s e -- TODO remove? rListPaths?
rList s e@(RrrList rtn _ _ _)
  | rtn `elem` [Empty, str, num] = rListLits  s e -- TODO does Empty fit here?
  | otherwise                    = rListPaths s e
rList _ _ = error "bad arguemnt to rList"

-- special case for empty lists
-- TODO is a special type for this really needed?
-- rListEmpty :: RrrState -> RrrExpr -> Rules ExprPath
-- rListEmpty s@(_,cfg,ref) e@(RrrList Empty _ _ _) = do
--   let link  = exprPath s e
--       link' = debugRules cfg "rListEmpty" e $ fromRrrPath cfg link
--   link' %> \_ -> aListEmpty cfg ref ids link
--   return (ExprPath link')
-- rListEmpty _ e = error $ "bad arguemnt to rListEmpty: " ++ show e

-- TODO is this actually needed? seems the same as lits or paths really
--      (also, is there a need to write empty lists at all?)
-- aListEmpty :: RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> Action ()
-- aListEmpty cfg ref ids link = writeLits cfg ref link'' [] -- TODO error here?
--   where
--     link'  = fromRrrPath cfg link
--     link'' = debugAction cfg "aListEmpty" link' [link']

-- special case for writing lists of strings or numbers as a single file
rListLits :: RrrState -> RrrExpr -> Rules ExprPath
rListLits s@(_, cfg, ref, ids) e@(RrrList _ _ _ exprs) = do
  litPaths <- mapM (rExpr s) exprs
  let litPaths' = map (\(ExprPath p) -> toRrrPath cfg p) litPaths
  outPath' %> \_ -> aListLits cfg ref ids litPaths' outPath
  return (ExprPath outPath')
  where
    outPath  = exprPath s e
    outPath' = debugRules cfg "rListLits" e $ fromRrrPath cfg outPath
rListLits _ e = error $ "bad argument to rListLits: " ++ show e

-- TODO put this in a cache dir by content hash and link there
aListLits :: RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> RrrPath -> Action ()
aListLits cfg ref _ paths outPath = do
  -- need paths'
  lits <- mapM (readLit cfg ref) paths'
  let lits' = map stripWhiteSpace lits -- TODO insert <<emptylist>> here?
  debugL cfg $ "aListLits lits': " ++ show lits'
  writeLits cfg ref out'' lits'
  where
    out'   = fromRrrPath cfg outPath
    out''  = debugA cfg "aListLits" out' (out':paths')
    paths' = map (fromRrrPath cfg) paths

-- regular case for writing a list of links to some other file type
rListPaths :: RrrState -> RrrExpr -> Rules ExprPath
rListPaths s@(_, cfg, ref, ids) e@(RrrList rtn salt _ exprs) = do
  paths <- mapM (rExpr s) exprs
  let paths'   = map (\(ExprPath p) -> toRrrPath cfg p) paths
      hash     = digest $ concat $ map digest paths'
      outPath  = exprPathExplicit cfg "list" (ListOf rtn) salt [hash]
      outPath' = debugRules cfg "rListPaths" e $ fromRrrPath cfg outPath
  outPath' %> \_ -> aListPaths cfg ref ids paths' outPath
  return (ExprPath outPath')
rListPaths _ _ = error "bad arguemnts to rListPaths"

-- works on everything but lits: paths or empty lists
aListPaths :: RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> RrrPath -> Action ()
aListPaths cfg ref _ paths outPath = do
  debugNeed cfg "aListPaths" paths'
  paths'' <- liftIO $ mapM (resolveSymlinks $ Just $ cfgTmpDir cfg) paths'
  debugNeed cfg "aListPaths" paths''
  let paths''' = map (toRrrPath cfg) paths'' -- TODO not working?
  writePaths cfg ref out'' paths'''
  where
    out'   = fromRrrPath cfg outPath
    out''  = debugA cfg "aListPaths" out' (out':paths')
    paths' = map (fromRrrPath cfg) paths -- TODO remove this

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
rRef :: RrrState -> RrrExpr -> Rules ExprPath
rRef (_, cfg, _, _) e@(RrrRef _ _ _ var) = return $ ePath $ varPath cfg var e
  where
    ePath p = ExprPath $ debugRules cfg "rRef" e $ fromRrrPath cfg p
rRef _ _ = error "bad argument to rRef"

-- Creates a symlink from varname to expression file.
-- TODO unify with rLink2, rLoad etc?
-- TODO do we need both the RrrExpr and ExprPath? seems like RrrExpr would do
rVar :: RrrState -> RrrVar -> RrrExpr -> RrrPath -> Rules VarPath
rVar (_, cfg, ref, ids) var expr oPath = do
  vPath' %> \_ -> aVar cfg ref ids vPath oPath
  return (VarPath vPath')
  where
    vPath  = varPath cfg var expr
    vPath' = debugRules cfg "rVar" var $ fromRrrPath cfg vPath

aVar :: RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> RrrPath -> Action ()
aVar cfg ref _ vPath oPath = do
  alwaysRerun
  debugNeed cfg "aVar" [oPath']
  withWriteLock' ref vPath' $ liftIO $ removeIfExists vPath'
  -- TODO should it sometimes symlink and sometimes copy?
  -- TODO before copying, think about how you might have to re-hash again!
  symlink cfg ref vPath'' oPath
  -- ids' <- liftIO $ readIORef ids
  -- unhashIDsFile cfg ref ids' oPath vPath''
  where
    oPath'  = fromRrrPath cfg oPath
    vPath'  = fromRrrPath cfg vPath
    vPath'' = debugA cfg "aVar" vPath [vPath', oPath']

-- Handles the actual rule generation for all binary operators.
-- TODO can it be factored out somehow? seems almost trivial now...
rBop :: RrrState -> RrrExpr -> (RrrExpr, RrrExpr)
      -> Rules (ExprPath, ExprPath, ExprPath)
rBop s@(_, cfg, _, _) e@(RrrBop _ _ _ _ _ _) (n1, n2) = do
  (ExprPath p1) <- rExpr s n1
  (ExprPath p2) <- rExpr s n2
  let path  = fromRrrPath cfg $ exprPath s e
      path' = debugRules cfg "rBop" e path
  return (ExprPath p1, ExprPath p2, ExprPath path')
rBop _ _ _ = error "bad argument to rBop"

------------------------------
-- [t]ypechecking functions --
------------------------------

typeError :: [RrrType] -> [RrrType] -> String
typeError expected actual =
  "Type error: expected " ++ show expected
           ++ " but got " ++ show actual

-- TODO this should fail for type errors like multiplying a list by a num!
defaultTypeCheck :: [RrrType] -> RrrType
                 -> [RrrType] -> Either String RrrType
defaultTypeCheck expected returned actual =
  if actual `typesMatch` expected
    then Right returned
    else Left $ typeError expected actual

--------------------------
-- links to input files --
--------------------------

{- Takes a string with the filepath to load. Creates a trivial expression file
 - that's just a symlink to the given path. These should be the only absolute
 - links, and the only ones that point outside the temp dir.
 - TODO still true?
 -}
mkLoad :: Bool -> String -> RrrType -> RrrFunction
mkLoad hashSeqIDs name rtn = RrrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str] rtn
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name [str] rtn
  , fFixity    = Prefix
  , fRules     = rLoad hashSeqIDs
  }

{- Like cLoad, except it operates on a list of strings. Note that you can also
 - load lists using cLoad, but it's not recommended because then you have to
 - write the list in a file, whereas this can handle literal lists in the
 - source code.
 -}
mkLoadList :: Bool -> String -> RrrType -> RrrFunction
mkLoadList hashSeqIDs name rtn = RrrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [(ListOf str)] (ListOf rtn)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name [(ListOf str)] (ListOf rtn)
  , fFixity    = Prefix
  , fRules     = rLoadList hashSeqIDs
  }

-- The paths here are a little confusing: expr is a str of the path we want to
-- link to. So after compiling it we get a path to *that str*, and have to read
-- the file to access it. Then we want to `ln` to the file it points to.
rLoad :: Bool -> RrrState -> RrrExpr -> Rules ExprPath
rLoad hashSeqIDs s@(_, cfg, ref, ids) e@(RrrFun _ _ _ _ [p]) = do
  (ExprPath strPath) <- rExpr s p
  out' %> \_ -> aLoad hashSeqIDs cfg ref ids (toRrrPath cfg strPath) out
  return (ExprPath out')
  where
    out  = exprPath s e
    out' = fromRrrPath cfg out
rLoad _ _ _ = error "bad argument to rLoad"

aLoadHash :: Bool -> RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> String -> Action RrrPath
aLoadHash hashSeqIDs cfg ref ids src ext = do
  alwaysRerun
  -- liftIO $ putStrLn $ "running aLoadHash"
  debugNeed cfg "aLoadHash" [src']
  md5 <- hashContent cfg ref src -- TODO permission error here?
  let tmpDir'   = fromRrrPath cfg $ cacheDir cfg "load" -- TODO should IDs be written to this + _ids.txt?
      hashPath' = tmpDir' </> md5 <.> ext
      hashPath  = toRrrPath cfg hashPath'
  if not hashSeqIDs
    then symlink cfg ref hashPath src
    else do
      let idsPath' = hashPath' <.> "ids"
          idsPath  = toRrrPath cfg idsPath'
      done <- doesFileExist idsPath'
      newIDs <- if done
        then do 
          -- liftIO $ putStrLn "reading previously hashed ids"
          readHashedIDs cfg ref idsPath
        else do
          -- liftIO $ putStrLn "hashing ids for the first time"
          newIDs <- hashIDsFile cfg ref src hashPath
          writeHashedIDs cfg ref idsPath newIDs
          return newIDs
      liftIO $ atomicModifyIORef ids $ \is -> (M.union newIDs is, ())
  -- ids' <- liftIO $ readIORef ids
  -- liftIO $ putStrLn $ "total is now " ++ show (length $ M.keys ids') ++ " ids"
  -- liftIO $ putStrLn $ show $ M.keys ids'
  return hashPath
  where
    src' = fromRrrPath cfg src

aLoad :: Bool -> RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> RrrPath -> Action ()
aLoad hashSeqIDs cfg ref ids strPath outPath = do
  debugNeed cfg "aLoad" [strPath']
  pth <- readLitPaths cfg ref strPath'
  src' <- liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) $ fromRrrPath cfg $ head pth -- TODO safer!
  -- debugL cfg $ "aLoad src': '" ++ src' ++ "'"
  -- debugL cfg $ "aLoad outPath': '" ++ outPath' ++ "'"
  hashPath <- aLoadHash hashSeqIDs cfg ref ids (toRrrPath cfg src') (takeExtension outPath')
  -- let hashPath'    = fromRrrPath cfg hashPath
      -- hashPathRel' = ".." </> ".." </> makeRelative (cfgTmpDir cfg) hashPath'
  symlink cfg ref outPath'' hashPath
  -- debugTrackWrite cfg [outPath'] -- TODO WTF? why does this not get called by symlink?
  where
    strPath'  = fromRrrPath cfg strPath
    outPath'  = fromRrrPath cfg outPath
    outPath'' = debugA cfg "aLoad" outPath [strPath', outPath']

rLoadList :: Bool -> RulesFn
rLoadList hashSeqIDs s e@(RrrFun (ListOf r) _ _ _ [es])
  | r `elem` [str, num] = rLoadListLits s es
  | otherwise = rLoadListLinks hashSeqIDs s e
rLoadList _ _ _ = error "bad arguments to rLoadList"

-- special case for lists of str and num
-- TODO remove rtn and use (typeOf expr)?
-- TODO is this different from rListOne, except in its return type?
-- TODO is it different from rLink? seems like it's just a copy/link operation...
-- TODO don't need to hash seqids here right?
rLoadListLits :: RulesFn
rLoadListLits s@(_, cfg, ref, ids) expr = do
  (ExprPath litsPath') <- rExpr s expr
  let litsPath = toRrrPath cfg litsPath'
  outPath' %> \_ -> aLoadListLits cfg ref ids outPath litsPath
  return (ExprPath outPath')
  where
    outPath  = exprPath s expr
    outPath' = fromRrrPath cfg outPath

aLoadListLits :: RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> RrrPath -> Action ()
aLoadListLits cfg ref _ outPath litsPath = do
  let litsPath' = fromRrrPath cfg litsPath
      out       = debugA cfg "aLoadListLits" outPath' [outPath', litsPath']
  lits  <- readLits cfg ref litsPath'
  lits' <- liftIO $ mapM absolutize lits
  writeLits cfg ref out lits'
  where
    outPath' = fromRrrPath cfg outPath

-- regular case for lists of any other file type
rLoadListLinks :: Bool -> RulesFn
rLoadListLinks hashSeqIDs s@(_, cfg, ref, ids) (RrrFun rtn salt _ _ [es]) = do
  (ExprPath pathsPath) <- rExpr s es
  let hash     = digest $ toRrrPath cfg pathsPath
      outPath  = exprPathExplicit cfg "list" rtn salt [hash]
      outPath' = fromRrrPath cfg outPath
  outPath' %> \_ -> aLoadListLinks hashSeqIDs cfg ref ids (toRrrPath cfg pathsPath) outPath
  return (ExprPath outPath')
rLoadListLinks _ _ _ = error "bad arguments to rLoadListLinks"

aLoadListLinks :: Bool -> RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> RrrPath -> Action ()
aLoadListLinks hashSeqIDs cfg ref ids pathsPath outPath = do
  -- Careful! The user will write paths relative to workdir and those come
  -- through as a (ListOf str) here; have to read as Strings and convert to
  -- RrrPaths
  paths <- readLitPaths cfg ref pathsPath'
  let paths' = map (fromRrrPath cfg) paths
  paths'' <- liftIO $ mapM (resolveSymlinks $ Just $ cfgTmpDir cfg) paths'
  let paths''' = map (toRrrPath cfg) paths''
  hashPaths <- mapM (\p -> aLoadHash hashSeqIDs cfg ref ids p
                         $ takeExtension $ fromRrrPath cfg p) paths'''
  let hashPaths' = map (fromRrrPath cfg) hashPaths
  -- debugL cfg $ "about to need: " ++ show paths''
  debugNeed cfg "aLoadListLinks" hashPaths'
  writePaths cfg ref out hashPaths
  where
    outPath'   = fromRrrPath cfg outPath
    pathsPath' = fromRrrPath cfg pathsPath
    out = debugA cfg "aLoadListLinks" outPath' [outPath', pathsPath']

-- based on https://stackoverflow.com/a/18627837
-- uniqLines :: Ord a => [a] -> [a]
-- uniqLines = unlines . toList . fromList . lines

-- takes an action fn with any number of args and calls it with a tmpdir.
-- TODO rename something that goes with the map fns?
rSimple :: (RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ()) -> RulesFn
rSimple actFn = rSimple' Nothing actFn'
  where
    actFn' cfg ref ids _ args = actFn cfg ref ids args -- drop unused tmpdir

rSimpleTmp :: String
           -> (RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> [RrrPath] -> Action ())
           -> RulesFn
rSimpleTmp prefix = rSimple' (Just prefix)

{- For scripts that just need some args passed to them. The first will be the
 - outPath, and the rest actual args. The string is the script name.
 -}
rSimpleScript :: String -> RulesFn
rSimpleScript = rSimple . aSimpleScript

rSimpleScriptPar :: String -> RulesFn
rSimpleScriptPar = rSimple . aSimpleScriptPar

rSimpleScriptNoFix :: String -> RulesFn
rSimpleScriptNoFix = rSimple . aSimpleScriptNoFix

aSimpleScriptNoFix :: String -> (RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ())
aSimpleScriptNoFix = aSimpleScript' False False

aSimpleScript :: String -> (RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ())
aSimpleScript = aSimpleScript' False True

aSimpleScriptPar :: String -> (RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ())
aSimpleScriptPar = aSimpleScript' True True

aSimpleScript' :: Bool -> Bool -> String -> (RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ())
aSimpleScript' parCmd fixEmpties script cfg ref ids (out:ins) = aSimple' cfg ref ids out actFn Nothing ins
  where
    -- TODO is tmpDir used here at all? should it be?
    -- TODO match []?
    actFn c r _ t (o:is) = let o'  = fromRrrPath c o -- TODO better var names here
                               t'  = fromRrrPath c t
                               is' = map (fromRrrPath c) is
                           in wrappedCmdWrite parCmd fixEmpties c r o' is' [] [Cwd t'] script (o':is')
    actFn _ _ _ _ _ = error "bad argument to aSimpleScript actFn"
aSimpleScript' _ _ _ _ _ _ as = error $ "bad argument to aSimpleScript: " ++ show as

rSimple' :: Maybe String
         -> (RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> [RrrPath] -> Action ())
         -> RulesFn
rSimple' mTmpPrefix actFn s@(_, cfg, ref, ids) e@(RrrFun _ _ _ _ exprs) = do
  argPaths <- mapM (rExpr s) exprs
  let argPaths' = map (\(ExprPath p) -> toRrrPath cfg p) argPaths
  outPath' %> \_ -> aSimple' cfg ref ids outPath actFn mTmpDir argPaths'
  return (ExprPath outPath')
  where
    mTmpDir  = fmap (cacheDir cfg) mTmpPrefix -- TODO tables bug here?
    outPath  = exprPath s e
    outPath' = fromRrrPath cfg outPath
rSimple' _ _ _ _ = error "bad argument to rSimple'"

-- TODO aSimpleScript that calls aSimple' with a wrappedCmd as the actFn
-- TODO rSimpleScript that calls rSimple + that

-- TODO need to handle empty lists here?
aSimple' ::  RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath
         -> (RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> [RrrPath] -> Action ())
         -> Maybe RrrPath -> [RrrPath] -> Action ()
aSimple' cfg ref ids outPath actFn mTmpDir argPaths = do
  debugNeed cfg "aSimple'" argPaths'
  argPaths'' <- liftIO $ mapM (fmap (toRrrPath cfg) . resolveSymlinks (Just $ cfgTmpDir cfg)) argPaths'
  let o' = debug cfg ("aSimple' outPath': " ++ outPath' ++ "'") outPath
      as = debug cfg ("aSimple' argsPaths'': " ++ show argPaths'') argPaths''
  actFn cfg ref ids tmpDir (o':as)
  trackWrite [out] -- TODO remove?
  where
    -- TODO probably not "simple tmp" anymore... remove? rename?
    hashes     = concat $ intersperse "_" $ map digest argPaths'
    argPaths'  = map (fromRrrPath cfg) argPaths
    outPath'   = fromRrrPath cfg outPath
    out = debugA cfg "aSimple'" outPath' (outPath':tmpDir':argPaths')
    (tmpDir, tmpDir') = case mTmpDir of
                Nothing  -> (toRrrPath cfg $ cfgTmpDir cfg, cfgTmpDir cfg)
                Just dir -> (toRrrPath cfg d, d)
                  where
                    d = fromRrrPath cfg dir </> hashes
