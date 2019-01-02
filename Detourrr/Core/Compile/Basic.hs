
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

import Detourrr.Core.Paths (cacheDir, exprPath, exprPathExplicit, toDtrPath,
                            fromDtrPath, varPath, DtrPath)

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


debug :: DtrConfig -> String -> a -> a
debug cfg msg rtn = if cfgDebug cfg then trace msg rtn else rtn

-- TODO restrict to DtrExpr?
-- TODO put in rExpr to catch everything at once? but misses which fn was called
debugRules :: (Pretty a, Show b) => DtrConfig -> String -> a -> b -> b
debugRules cfg name input out = debug cfg msg out
  where
    ren = render $ pPrint input
    msg = name ++ " compiled '" ++ ren ++ "' to " ++ show out

------------------------------
-- compile the Detourrr AST --
------------------------------

rExpr :: DtrState -> DtrExpr -> Rules ExprPath
rExpr s e@(DtrLit _ _ _      ) = rLit s e
rExpr s e@(DtrRef _ _ _ _    ) = rRef s e
rExpr s e@(DtrList _ _ _ _   ) = rList s e
rExpr s e@(DtrBop _ _ _ n _ _) = rulesByName s e n -- TODO turn into Fun?
rExpr s e@(DtrFun _ _ _ n _  ) = rulesByName s e n
rExpr _   (DtrRules (CompiledExpr _ rules)) = rules

-- TODO remove once no longer needed (parser should find fns)
rulesByName :: DtrState -> DtrExpr -> String -> Rules ExprPath
rulesByName s@(_, cfg, _, _) expr name = case findFunction cfg name of
  Nothing -> error $ "no such function '" ++ name ++ "'"
  Just f  -> (fRules f) s expr

rAssign :: DtrState -> DtrAssign -> Rules (DtrVar, VarPath)
rAssign s@(_, cfg, _, _) (var, expr) = do
  (ExprPath path) <- rExpr s expr
  path' <- rVar s var expr $ toDtrPath cfg path
  let res  = (var, path')
      res' = debugRules cfg "rAssign" (var, expr) res
  return res'

-- TODO how to fail if the var doesn't exist??
--      (or, is that not possible for a typechecked AST?)
compileScript :: DtrState -> Maybe String -> Rules ResPath
compileScript s@(as, _, _, _) permHash = do
  -- TODO this can't be done all in parallel because they depend on each other,
  --      but can parts of it be parallelized? or maybe it doesn't matter because
  --      evaluating the code itself is always faster than the system commands
  rpaths <- mapM (rAssign s) as
  case lookup (DtrVar res) rpaths of
    Nothing -> fail "no result variable. that's not right!"
    Just (VarPath r) -> return $ ResPath r
  where
    -- p here is "result" + the permutation name/hash if there is one right?
    res = case permHash of
      Nothing -> "result"
      Just h  -> "result." ++ h

-- write a literal value from Detourrr source code to file
rLit :: DtrState -> DtrExpr -> Rules ExprPath
rLit s@(_, cfg, ref, ids) expr = do
  let path  = exprPath s expr -- absolute paths allowed!
      path' = debugRules cfg "rLit" expr $ fromDtrPath cfg path
  path' %> \_ -> aLit cfg ref ids expr path
  return (ExprPath path')

-- TODO take the path, not the expression?
aLit :: DtrConfig -> Locks -> HashedSeqIDsRef -> DtrExpr -> DtrPath -> Action ()
aLit cfg ref _ expr out = writeLit cfg ref out'' ePath -- TODO too much dedup?
  where
    paths :: DtrExpr -> FilePath
    paths (DtrLit _ _ p) = p
    paths _ = error "bad argument to paths"
    ePath = paths expr
    out'  = fromDtrPath cfg out
    out'' = debugA cfg "aLit" out' [ePath, out']

rList :: DtrState -> DtrExpr -> Rules ExprPath
-- TODO is this the bug? refers to a list of other empty lists, no?
-- rList s e@(DtrList Empty _ _ _) = rListLits s e -- TODO remove? rListPaths?
rList s e@(DtrList rtn _ _ _)
  | rtn `elem` [Empty, str, num] = rListLits  s e -- TODO does Empty fit here?
  | otherwise                    = rListPaths s e
rList _ _ = error "bad arguemnt to rList"

-- special case for empty lists
-- TODO is a special type for this really needed?
-- rListEmpty :: DtrState -> DtrExpr -> Rules ExprPath
-- rListEmpty s@(_,cfg,ref) e@(DtrList Empty _ _ _) = do
--   let link  = exprPath s e
--       link' = debugRules cfg "rListEmpty" e $ fromDtrPath cfg link
--   link' %> \_ -> aListEmpty cfg ref ids link
--   return (ExprPath link')
-- rListEmpty _ e = error $ "bad arguemnt to rListEmpty: " ++ show e

-- TODO is this actually needed? seems the same as lits or paths really
--      (also, is there a need to write empty lists at all?)
-- aListEmpty :: DtrConfig -> Locks -> HashedSeqIDsRef -> DtrPath -> Action ()
-- aListEmpty cfg ref ids link = writeLits cfg ref link'' [] -- TODO error here?
--   where
--     link'  = fromDtrPath cfg link
--     link'' = debugAction cfg "aListEmpty" link' [link']

-- special case for writing lists of strings or numbers as a single file
rListLits :: DtrState -> DtrExpr -> Rules ExprPath
rListLits s@(_, cfg, ref, ids) e@(DtrList _ _ _ exprs) = do
  litPaths <- mapM (rExpr s) exprs
  let litPaths' = map (\(ExprPath p) -> toDtrPath cfg p) litPaths
  outPath' %> \_ -> aListLits cfg ref ids litPaths' outPath
  return (ExprPath outPath')
  where
    outPath  = exprPath s e
    outPath' = debugRules cfg "rListLits" e $ fromDtrPath cfg outPath
rListLits _ e = error $ "bad argument to rListLits: " ++ show e

-- TODO put this in a cache dir by content hash and link there
aListLits :: DtrConfig -> Locks -> HashedSeqIDsRef -> [DtrPath] -> DtrPath -> Action ()
aListLits cfg ref _ paths outPath = do
  -- need paths'
  lits <- mapM (readLit cfg ref) paths'
  let lits' = map stripWhiteSpace lits -- TODO insert <<emptylist>> here?
  debugL cfg $ "aListLits lits': " ++ show lits'
  writeLits cfg ref out'' lits'
  where
    out'   = fromDtrPath cfg outPath
    out''  = debugA cfg "aListLits" out' (out':paths')
    paths' = map (fromDtrPath cfg) paths

-- regular case for writing a list of links to some other file type
rListPaths :: DtrState -> DtrExpr -> Rules ExprPath
rListPaths s@(_, cfg, ref, ids) e@(DtrList rtn salt _ exprs) = do
  paths <- mapM (rExpr s) exprs
  let paths'   = map (\(ExprPath p) -> toDtrPath cfg p) paths
      hash     = digest $ concat $ map digest paths'
      outPath  = exprPathExplicit cfg "list" (ListOf rtn) salt [hash]
      outPath' = debugRules cfg "rListPaths" e $ fromDtrPath cfg outPath
  outPath' %> \_ -> aListPaths cfg ref ids paths' outPath
  return (ExprPath outPath')
rListPaths _ _ = error "bad arguemnts to rListPaths"

-- works on everything but lits: paths or empty lists
aListPaths :: DtrConfig -> Locks -> HashedSeqIDsRef -> [DtrPath] -> DtrPath -> Action ()
aListPaths cfg ref _ paths outPath = do
  debugNeed cfg "aListPaths" paths'
  paths'' <- liftIO $ mapM (resolveSymlinks $ Just $ cfgTmpDir cfg) paths'
  debugNeed cfg "aListPaths" paths''
  let paths''' = map (toDtrPath cfg) paths'' -- TODO not working?
  writePaths cfg ref out'' paths'''
  where
    out'   = fromDtrPath cfg outPath
    out''  = debugA cfg "aListPaths" out' (out':paths')
    paths' = map (fromDtrPath cfg) paths -- TODO remove this

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
rRef :: DtrState -> DtrExpr -> Rules ExprPath
rRef (_, cfg, _, _) e@(DtrRef _ _ _ var) = return $ ePath $ varPath cfg var e
  where
    ePath p = ExprPath $ debugRules cfg "rRef" e $ fromDtrPath cfg p
rRef _ _ = error "bad argument to rRef"

-- Creates a symlink from varname to expression file.
-- TODO unify with rLink2, rLoad etc?
-- TODO do we need both the DtrExpr and ExprPath? seems like DtrExpr would do
rVar :: DtrState -> DtrVar -> DtrExpr -> DtrPath -> Rules VarPath
rVar (_, cfg, ref, ids) var expr oPath = do
  vPath' %> \_ -> aVar cfg ref ids vPath oPath
  return (VarPath vPath')
  where
    vPath  = varPath cfg var expr
    vPath' = debugRules cfg "rVar" var $ fromDtrPath cfg vPath

aVar :: DtrConfig -> Locks -> HashedSeqIDsRef -> DtrPath -> DtrPath -> Action ()
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
    oPath'  = fromDtrPath cfg oPath
    vPath'  = fromDtrPath cfg vPath
    vPath'' = debugA cfg "aVar" vPath [vPath', oPath']

-- Handles the actual rule generation for all binary operators.
-- TODO can it be factored out somehow? seems almost trivial now...
rBop :: DtrState -> DtrExpr -> (DtrExpr, DtrExpr)
      -> Rules (ExprPath, ExprPath, ExprPath)
rBop s@(_, cfg, _, _) e@(DtrBop _ _ _ _ _ _) (n1, n2) = do
  (ExprPath p1) <- rExpr s n1
  (ExprPath p2) <- rExpr s n2
  let path  = fromDtrPath cfg $ exprPath s e
      path' = debugRules cfg "rBop" e path
  return (ExprPath p1, ExprPath p2, ExprPath path')
rBop _ _ _ = error "bad argument to rBop"

------------------------------
-- [t]ypechecking functions --
------------------------------

typeError :: [DtrType] -> [DtrType] -> String
typeError expected actual =
  "Type error: expected " ++ show expected
           ++ " but got " ++ show actual

-- TODO this should fail for type errors like multiplying a list by a num!
defaultTypeCheck :: [DtrType] -> DtrType
                 -> [DtrType] -> Either String DtrType
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
mkLoad :: Bool -> String -> DtrType -> DtrFunction
mkLoad hashSeqIDs name rtn = DtrFunction
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
mkLoadList :: Bool -> String -> DtrType -> DtrFunction
mkLoadList hashSeqIDs name rtn = DtrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [(ListOf str)] (ListOf rtn)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name [(ListOf str)] (ListOf rtn)
  , fFixity    = Prefix
  , fRules     = rLoadList hashSeqIDs
  }

-- The paths here are a little confusing: expr is a str of the path we want to
-- link to. So after compiling it we get a path to *that str*, and have to read
-- the file to access it. Then we want to `ln` to the file it points to.
rLoad :: Bool -> DtrState -> DtrExpr -> Rules ExprPath
rLoad hashSeqIDs s@(_, cfg, ref, ids) e@(DtrFun _ _ _ _ [p]) = do
  (ExprPath strPath) <- rExpr s p
  out' %> \_ -> aLoad hashSeqIDs cfg ref ids (toDtrPath cfg strPath) out
  return (ExprPath out')
  where
    out  = exprPath s e
    out' = fromDtrPath cfg out
rLoad _ _ _ = error "bad argument to rLoad"

aLoadHash :: Bool -> DtrConfig -> Locks -> HashedSeqIDsRef -> DtrPath -> String -> Action DtrPath
aLoadHash hashSeqIDs cfg ref ids src ext = do
  alwaysRerun
  -- liftIO $ putStrLn $ "running aLoadHash"
  debugNeed cfg "aLoadHash" [src']
  md5 <- hashContent cfg ref src -- TODO permission error here?
  let tmpDir'   = fromDtrPath cfg $ cacheDir cfg "load" -- TODO should IDs be written to this + _ids.txt?
      hashPath' = tmpDir' </> md5 <.> ext
      hashPath  = toDtrPath cfg hashPath'
  if not hashSeqIDs
    then symlink cfg ref hashPath src
    else do
      let idsPath' = hashPath' <.> "ids"
          idsPath  = toDtrPath cfg idsPath'
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
    src' = fromDtrPath cfg src

aLoad :: Bool -> DtrConfig -> Locks -> HashedSeqIDsRef -> DtrPath -> DtrPath -> Action ()
aLoad hashSeqIDs cfg ref ids strPath outPath = do
  debugNeed cfg "aLoad" [strPath']
  pth <- readLitPaths cfg ref strPath'
  src' <- liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) $ fromDtrPath cfg $ head pth -- TODO safer!
  -- debugL cfg $ "aLoad src': '" ++ src' ++ "'"
  -- debugL cfg $ "aLoad outPath': '" ++ outPath' ++ "'"
  hashPath <- aLoadHash hashSeqIDs cfg ref ids (toDtrPath cfg src') (takeExtension outPath')
  -- let hashPath'    = fromDtrPath cfg hashPath
      -- hashPathRel' = ".." </> ".." </> makeRelative (cfgTmpDir cfg) hashPath'
  symlink cfg ref outPath'' hashPath
  -- debugTrackWrite cfg [outPath'] -- TODO WTF? why does this not get called by symlink?
  where
    strPath'  = fromDtrPath cfg strPath
    outPath'  = fromDtrPath cfg outPath
    outPath'' = debugA cfg "aLoad" outPath [strPath', outPath']

rLoadList :: Bool -> RulesFn
rLoadList hashSeqIDs s e@(DtrFun (ListOf r) _ _ _ [es])
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
  let litsPath = toDtrPath cfg litsPath'
  outPath' %> \_ -> aLoadListLits cfg ref ids outPath litsPath
  return (ExprPath outPath')
  where
    outPath  = exprPath s expr
    outPath' = fromDtrPath cfg outPath

aLoadListLits :: DtrConfig -> Locks -> HashedSeqIDsRef -> DtrPath -> DtrPath -> Action ()
aLoadListLits cfg ref _ outPath litsPath = do
  let litsPath' = fromDtrPath cfg litsPath
      out       = debugA cfg "aLoadListLits" outPath' [outPath', litsPath']
  lits  <- readLits cfg ref litsPath'
  lits' <- liftIO $ mapM absolutize lits
  writeLits cfg ref out lits'
  where
    outPath' = fromDtrPath cfg outPath

-- regular case for lists of any other file type
rLoadListLinks :: Bool -> RulesFn
rLoadListLinks hashSeqIDs s@(_, cfg, ref, ids) (DtrFun rtn salt _ _ [es]) = do
  (ExprPath pathsPath) <- rExpr s es
  let hash     = digest $ toDtrPath cfg pathsPath
      outPath  = exprPathExplicit cfg "list" rtn salt [hash]
      outPath' = fromDtrPath cfg outPath
  outPath' %> \_ -> aLoadListLinks hashSeqIDs cfg ref ids (toDtrPath cfg pathsPath) outPath
  return (ExprPath outPath')
rLoadListLinks _ _ _ = error "bad arguments to rLoadListLinks"

aLoadListLinks :: Bool -> DtrConfig -> Locks -> HashedSeqIDsRef -> DtrPath -> DtrPath -> Action ()
aLoadListLinks hashSeqIDs cfg ref ids pathsPath outPath = do
  -- Careful! The user will write paths relative to workdir and those come
  -- through as a (ListOf str) here; have to read as Strings and convert to
  -- DtrPaths
  paths <- readLitPaths cfg ref pathsPath'
  let paths' = map (fromDtrPath cfg) paths
  paths'' <- liftIO $ mapM (resolveSymlinks $ Just $ cfgTmpDir cfg) paths'
  let paths''' = map (toDtrPath cfg) paths''
  hashPaths <- mapM (\p -> aLoadHash hashSeqIDs cfg ref ids p
                         $ takeExtension $ fromDtrPath cfg p) paths'''
  let hashPaths' = map (fromDtrPath cfg) hashPaths
  -- debugL cfg $ "about to need: " ++ show paths''
  debugNeed cfg "aLoadListLinks" hashPaths'
  writePaths cfg ref out hashPaths
  where
    outPath'   = fromDtrPath cfg outPath
    pathsPath' = fromDtrPath cfg pathsPath
    out = debugA cfg "aLoadListLinks" outPath' [outPath', pathsPath']

-- based on https://stackoverflow.com/a/18627837
-- uniqLines :: Ord a => [a] -> [a]
-- uniqLines = unlines . toList . fromList . lines

-- takes an action fn with any number of args and calls it with a tmpdir.
-- TODO rename something that goes with the map fns?
rSimple :: (DtrConfig -> Locks -> HashedSeqIDsRef -> [DtrPath] -> Action ()) -> RulesFn
rSimple actFn = rSimple' Nothing actFn'
  where
    actFn' cfg ref ids _ args = actFn cfg ref ids args -- drop unused tmpdir

rSimpleTmp :: String
           -> (DtrConfig -> Locks -> HashedSeqIDsRef -> DtrPath -> [DtrPath] -> Action ())
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

aSimpleScriptNoFix :: String -> (DtrConfig -> Locks -> HashedSeqIDsRef -> [DtrPath] -> Action ())
aSimpleScriptNoFix = aSimpleScript' False False

aSimpleScript :: String -> (DtrConfig -> Locks -> HashedSeqIDsRef -> [DtrPath] -> Action ())
aSimpleScript = aSimpleScript' False True

aSimpleScriptPar :: String -> (DtrConfig -> Locks -> HashedSeqIDsRef -> [DtrPath] -> Action ())
aSimpleScriptPar = aSimpleScript' True True

aSimpleScript' :: Bool -> Bool -> String -> (DtrConfig -> Locks -> HashedSeqIDsRef -> [DtrPath] -> Action ())
aSimpleScript' parCmd fixEmpties script cfg ref ids (out:ins) = aSimple' cfg ref ids out actFn Nothing ins
  where
    -- TODO is tmpDir used here at all? should it be?
    -- TODO match []?
    actFn c r _ t (o:is) = let o'  = fromDtrPath c o -- TODO better var names here
                               t'  = fromDtrPath c t
                               is' = map (fromDtrPath c) is
                           in wrappedCmdWrite parCmd fixEmpties c r o' is' [] [Cwd t'] script (o':is')
    actFn _ _ _ _ _ = error "bad argument to aSimpleScript actFn"
aSimpleScript' _ _ _ _ _ _ as = error $ "bad argument to aSimpleScript: " ++ show as

rSimple' :: Maybe String
         -> (DtrConfig -> Locks -> HashedSeqIDsRef -> DtrPath -> [DtrPath] -> Action ())
         -> RulesFn
rSimple' mTmpPrefix actFn s@(_, cfg, ref, ids) e@(DtrFun _ _ _ _ exprs) = do
  argPaths <- mapM (rExpr s) exprs
  let argPaths' = map (\(ExprPath p) -> toDtrPath cfg p) argPaths
  outPath' %> \_ -> aSimple' cfg ref ids outPath actFn mTmpDir argPaths'
  return (ExprPath outPath')
  where
    mTmpDir  = fmap (cacheDir cfg) mTmpPrefix -- TODO tables bug here?
    outPath  = exprPath s e
    outPath' = fromDtrPath cfg outPath
rSimple' _ _ _ _ = error "bad argument to rSimple'"

-- TODO aSimpleScript that calls aSimple' with a wrappedCmd as the actFn
-- TODO rSimpleScript that calls rSimple + that

-- TODO need to handle empty lists here?
aSimple' ::  DtrConfig -> Locks -> HashedSeqIDsRef -> DtrPath
         -> (DtrConfig -> Locks -> HashedSeqIDsRef -> DtrPath -> [DtrPath] -> Action ())
         -> Maybe DtrPath -> [DtrPath] -> Action ()
aSimple' cfg ref ids outPath actFn mTmpDir argPaths = do
  debugNeed cfg "aSimple'" argPaths'
  argPaths'' <- liftIO $ mapM (fmap (toDtrPath cfg) . resolveSymlinks (Just $ cfgTmpDir cfg)) argPaths'
  let o' = debug cfg ("aSimple' outPath': " ++ outPath' ++ "'") outPath
      as = debug cfg ("aSimple' argsPaths'': " ++ show argPaths'') argPaths''
  actFn cfg ref ids tmpDir (o':as)
  trackWrite [out] -- TODO remove?
  where
    -- TODO probably not "simple tmp" anymore... remove? rename?
    hashes     = concat $ intersperse "_" $ map digest argPaths'
    argPaths'  = map (fromDtrPath cfg) argPaths
    outPath'   = fromDtrPath cfg outPath
    out = debugA cfg "aSimple'" outPath' (outPath':tmpDir':argPaths')
    (tmpDir, tmpDir') = case mTmpDir of
                Nothing  -> (toDtrPath cfg $ cfgTmpDir cfg, cfgTmpDir cfg)
                Just dir -> (toDtrPath cfg d, d)
                  where
                    d = fromDtrPath cfg dir </> hashes
