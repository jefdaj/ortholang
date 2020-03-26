{-|
This module "compiles" an expression it by translating it into a set of Shake
build rules. To actually run the rules, use `eval` in the Interpret module.

TODO add more descriptive runtime error for canonicalizePath failing b/c no file

TODO see if you can avoid making more than one absolute symlink per input file

TODO make systematically sure there's only one rule for each file

TODO pass tmpDir as a config option somehow, and verbosity

TODO why doesn't turning down the verbosity actually work?
-}

module OrthoLang.Core.Compile.Basic
  ( compileScript
  , curl
  , debug
  , debugRules
  , defaultTypeCheck
  , mkLoad
  , mkLoadList
  -- , rBop
  , rExpr
  , typeError

  -- * Things to deprecate and then remove
  , rulesByName
  )
  where

-- TODO does turning of traces radically speed up the interpreter?

import Development.Shake
import Development.Shake.FilePath (isAbsolute)
import OrthoLang.Core.Types
import OrthoLang.Core.Pretty
import qualified Data.Map.Strict as M

import OrthoLang.Core.Paths (cacheDir, exprPath, exprPathExplicit, toOrthoLangPath,
                            fromOrthoLangPath, varPath, OrthoLangPath)

import Data.IORef                 (atomicModifyIORef')
import Data.List                  (isPrefixOf, isInfixOf)
import Development.Shake.FilePath ((</>), (<.>), takeFileName)
import OrthoLang.Core.Actions      (runCmd, CmdDesc(..), traceA, debugA, need',
                                   readLit, readLits, writeLit, writeLits, hashContent,
                                   readLitPaths, writePaths, symlink)
import OrthoLang.Core.Sanitize     (hashIDsFile2, readHashedIDs)
import OrthoLang.Core.Util         (absolutize, resolveSymlinks, stripWhiteSpace,
                                   digest, removeIfExists, headOrDie, trace, unlessExists)
import System.FilePath            (takeExtension)
import System.Exit                (ExitCode(..))
import System.Directory           (createDirectoryIfMissing)

import Data.Maybe (isJust, fromJust)

import OrthoLang.Core.Paths (insertNewRulesDigest)
import System.IO.Unsafe (unsafePerformIO)


debug :: OrthoLangConfig -> String -> a -> a
debug cfg msg rtn = if isJust (cfgDebug cfg) then trace "core.compile" msg rtn else rtn

-- TODO restrict to OrthoLangExpr?
-- TODO put in rExpr to catch everything at once? but misses which fn was called
debugRules :: (Pretty a, Show b) => OrthoLangConfig -> String -> a -> b -> b
debugRules cfg name input out = debug cfg msg out
  where
    ren = render $ pPrint input
    msg = name ++ " compiled '" ++ ren ++ "' to " ++ show out


------------------------------
-- compile the OrthoLang AST --
------------------------------

-- This should return the same outPath as the old RulesFns, without doing anything else.
-- TODO remove it once the new rules are all written
-- deprecatedRules :: OrthoLangState -> OrthoLangExpr -> Rules ExprPath
-- deprecatedRules s@(_, cfg, _, _) e = return $ ExprPath out'
--   where
--     out  = exprPath s e
--     out' = fromOrthoLangPath cfg $ exprPath s e

-- for functions with fNewRules, ignore fOldRules and return Nothing immediately. otherwise carry on as normal
-- TODO wait! it's the rules that might not need to be returned, not the path, right?
--            that actually makes it easy to use the same function types but not do any actual rules :D

-- TODO insert digests as they're compiled here, not as they're parsed
--      (because some are generated automatically, not parsed at all!)
-- TODO and put them into the state explicitly without this IORef hack

withInsertNewRulesDigests:: OrthoLangState -> [OrthoLangExpr] -> a -> a
withInsertNewRulesDigests s es a = unsafePerformIO $ do
  mapM_ (insertNewRulesDigest s) es
  return a

rExpr :: RulesFn
rExpr s@(_, cfg, _, _) e = withInsertNewRulesDigests s [e] $ rExpr' s e

rExpr' :: RulesFn
rExpr' s e@(OrthoLangLit _ _ _      ) = rLit s e
rExpr' s e@(OrthoLangRef _ _ _ _    ) = rRef s e
rExpr' s e@(OrthoLangList _ _ _ _   ) = rList s e -- TODO deprecate this?
rExpr' s e@(OrthoLangFun _ _ _ n _  ) = rulesByName s e n -- TODO deprecate this
rExpr' _   (OrthoLangRules (CompiledExpr _ _ rules)) = rules
rExpr' s e@(OrthoLangBop t r ds _ e1 e2) = withInsertNewRulesDigests s [e1,e2,es] $ rExpr s fn
  where
    es = OrthoLangList t r ds [e1, e2]
    fn = OrthoLangFun t r ds (prefixOf e) [es]

-- This is in the process of being replaced with fNewRules,
-- so we ignore any function that already has that field written.
rulesByName :: OrthoLangState -> OrthoLangExpr -> String -> Rules ExprPath
rulesByName s e@(OrthoLangFun _ _ _ _ es) n = withInsertNewRulesDigests s es $ rulesByName' s e n
rulesByName _ _ n = error $ "bad argument to rulesByName: " ++ n

rulesByName' s@(_, cfg, _, _) expr name = case findFunction cfg name of
  Nothing -> error $ "no such function '" ++ name ++ "'"
  Just f  -> case fNewRules f of
               Nothing -> if "load_" `isPrefixOf` fName f
                            then (fOldRules f) s $ setSalt 0 expr
                            else (fOldRules f) s expr
               Just _ -> return $ ExprPath $ fromOrthoLangPath cfg $ exprPath s expr

rAssign :: OrthoLangState -> OrthoLangAssign -> Rules (OrthoLangVar, VarPath)
rAssign s@(_, cfg, _, _) (var, expr) = do
  (ExprPath path) <- rExpr s expr
  path' <- rVar s var expr $ toOrthoLangPath cfg path
  let res  = (var, path')
      res' = debugRules cfg "rAssign" (var, expr) res
  return res'

-- TODO how to fail if the var doesn't exist??
--      (or, is that not possible for a typechecked AST?)
-- TODO remove permHash
compileScript :: OrthoLangState -> ReplaceID -> Rules ResPath
compileScript s@(as, _, _, _) _ = do
  -- TODO this can't be done all in parallel because they depend on each other,
  --      but can parts of it be parallelized? or maybe it doesn't matter because
  --      evaluating the code itself is always faster than the system commands
  rpaths <- mapM (rAssign s) as
  res <- case lookupResult rpaths of
    Nothing -> fmap (\(ExprPath p) -> p) $ rExpr s $ fromJust $ lookupResult $ ensureResult as
    Just r  -> fmap (\(VarPath  p) -> p) $ return r
  return $ ResPath res
  -- where
    -- p here is "result" + the permutation name/hash if there is one right?
    -- res = case permHash of
      -- Nothing -> "result"
      -- Just h  -> "result." ++ h

-- write a literal value from OrthoLang source code to file
rLit :: RulesFn
rLit s@(_, cfg, ref, ids) expr = do
  let path  = exprPath s expr -- absolute paths allowed!
      path' = debugRules cfg "rLit" expr $ fromOrthoLangPath cfg path
  path' %> \_ -> aLit cfg ref ids expr path
  return (ExprPath path')

-- TODO take the path, not the expression?
-- TODO these actions all need to decode their dependencies from the outpath rather than the expression
aLit :: OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangExpr -> OrthoLangPath -> Action ()
aLit cfg ref _ expr out = writeLit cfg ref out'' ePath -- TODO too much dedup?
  where
    paths :: OrthoLangExpr -> FilePath
    paths (OrthoLangLit _ _ p) = p
    paths _ = fail "bad argument to paths"
    ePath = paths expr
    out'  = fromOrthoLangPath cfg out
    out'' = traceA "aLit" out' [ePath, out']

rList :: RulesFn
-- TODO is this the bug? refers to a list of other empty lists, no?
-- rList s e@(OrthoLangList Empty _ _ _) = rListLits s e -- TODO remove? rListPaths?
rList s e@(OrthoLangList rtn _ _ es)
  | rtn `elem` [Empty, str, num] = withInsertNewRulesDigests s (e:es) $ rListLits  s e -- TODO does Empty fit here?
  | otherwise                    = withInsertNewRulesDigests s (e:es) $ rListPaths s e
rList _ _ = error "bad arguemnt to rList"

-- special case for writing lists of strings or numbers as a single file
rListLits :: RulesFn
rListLits s@(_, cfg, ref, ids) e@(OrthoLangList _ _ _ exprs) = do
  litPaths <- mapM (rExpr s) exprs
  let litPaths' = map (\(ExprPath p) -> toOrthoLangPath cfg p) litPaths
  outPath' %> \_ -> aListLits cfg ref ids litPaths' outPath
  return (ExprPath outPath')
  where
    outPath  = exprPath s e
    outPath' = debugRules cfg "rListLits" e $ fromOrthoLangPath cfg outPath
rListLits _ e = error $ "bad argument to rListLits: " ++ show e

-- TODO put this in a cache dir by content hash and link there
aListLits :: OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> OrthoLangPath -> Action ()
aListLits cfg ref _ paths outPath = do
  need paths'
  lits <- mapM (readLit cfg ref) paths'
  let lits' = map stripWhiteSpace lits -- TODO insert <<emptylist>> here?
  debugA "ortholang.core.compile.basic.aListLits" $ "lits': " ++ show lits'
  writeLits cfg ref out'' lits'
  where
    out'   = fromOrthoLangPath cfg outPath
    out''  = traceA "aListLits" out' (out':paths')
    paths' = map (fromOrthoLangPath cfg) paths

-- regular case for writing a list of links to some other file type
-- TODO hash mismatch error here?
rListPaths :: RulesFn
rListPaths s@(_, cfg, ref, ids) e@(OrthoLangList rtn salt _ exprs) = do
  paths <- mapM (rExpr s) exprs
  let paths'   = map (\(ExprPath p) -> toOrthoLangPath cfg p) paths
      -- hash     = digest $ concat $ map digest paths'
      -- outPath  = exprPathExplicit cfg "list" (ListOf rtn) salt [hash]
      outPath  = exprPath s e
      outPath' = debugRules cfg "rListPaths" e $ fromOrthoLangPath cfg outPath
  outPath' %> \_ -> aListPaths cfg ref ids paths' outPath
  return (ExprPath outPath')
rListPaths _ _ = error "bad arguemnts to rListPaths"

-- works on everything but lits: paths or empty lists
aListPaths :: OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> OrthoLangPath -> Action ()
aListPaths cfg ref _ paths outPath = do
  need' cfg ref "ortholang.core.compile.basic.aListPaths" paths'
  paths'' <- liftIO $ mapM (resolveSymlinks $ Just $ cfgTmpDir cfg) paths'
  need' cfg ref "ortholang.core.compile.basic.aListPaths" paths''
  let paths''' = map (toOrthoLangPath cfg) paths'' -- TODO not working?
  writePaths cfg ref out'' paths'''
  where
    out'   = fromOrthoLangPath cfg outPath
    out''  = traceA "aListPaths" out' (out':paths')
    paths' = map (fromOrthoLangPath cfg) paths -- TODO remove this

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
rRef :: RulesFn
rRef (_, cfg, _, _) e@(OrthoLangRef _ _ _ var) = return $ ePath $ varPath cfg var e
  where
    ePath p = ExprPath $ debugRules cfg "rRef" e $ fromOrthoLangPath cfg p
rRef _ _ = fail "bad argument to rRef"

-- Creates a symlink from varname to expression file.
-- TODO unify with rLink2, rLoad etc?
-- TODO do we need both the OrthoLangExpr and ExprPath? seems like OrthoLangExpr would do
rVar :: OrthoLangState -> OrthoLangVar -> OrthoLangExpr -> OrthoLangPath -> Rules VarPath
rVar (_, cfg, ref, ids) var expr oPath = do
  vPath' %> \_ -> aVar cfg ref ids vPath oPath
  return (VarPath vPath')
  where
    vPath  = varPath cfg var expr
    vPath' = debugRules cfg "rVar" var $ fromOrthoLangPath cfg vPath

aVar :: OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangPath -> OrthoLangPath -> Action ()
aVar cfg ref _ vPath oPath = do
  alwaysRerun
  need' cfg ref "ortholang.core.compile.basic.aVar" [oPath']
  liftIO $ removeIfExists ref vPath'
  -- TODO should it sometimes symlink and sometimes copy?
  -- TODO before copying, think about how you might have to re-hash again!
  symlink cfg ref vPath'' oPath
  -- ids' <- liftIO $ readIORef ids
  -- unhashIDsFile cfg ref ids' oPath vPath''
  where
    oPath'  = fromOrthoLangPath cfg oPath
    vPath'  = fromOrthoLangPath cfg vPath
    vPath'' = traceA "aVar" vPath [vPath', oPath']


------------------------------
-- [t]ypechecking functions --
------------------------------

typeError :: [OrthoLangType] -> [OrthoLangType] -> String
typeError expected actual =
  "Type error: expected " ++ show expected
           ++ " but got " ++ show actual

-- TODO this should fail for type errors like multiplying a list by a num!
defaultTypeCheck :: [OrthoLangType] -> OrthoLangType
                 -> [OrthoLangType] -> Either String OrthoLangType
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
mkLoad :: Bool -> String -> OrthoLangType -> OrthoLangFunction
mkLoad hashSeqIDs name rtn = OrthoLangFunction
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [str] rtn
  , fTypeDesc  = mkTypeDesc name [str] rtn
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rLoad hashSeqIDs
  }

{- Like cLoad, except it operates on a list of strings. Note that you can also
 - load lists using cLoad, but it's not recommended because then you have to
 - write the list in a file, whereas this can handle literal lists in the
 - source code.
 -}
mkLoadList :: Bool -> String -> OrthoLangType -> OrthoLangFunction
mkLoadList hashSeqIDs name rtn = OrthoLangFunction
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [(ListOf str)] (ListOf rtn)
  , fTypeDesc  = mkTypeDesc name [(ListOf str)] (ListOf rtn)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rLoadList hashSeqIDs
  }

-- The paths here are a little confusing: expr is a str of the path we want to
-- link to. So after compiling it we get a path to *that str*, and have to read
-- the file to access it. Then we want to `ln` to the file it points to.
rLoad :: Bool -> RulesFn
rLoad hashSeqIDs s@(_, cfg, ref, ids) e@(OrthoLangFun _ _ _ _ [p]) = do
  (ExprPath strPath) <- rExpr s p
  out' %> \_ -> aLoad hashSeqIDs cfg ref ids (toOrthoLangPath cfg strPath) out
  return (ExprPath out')
  where
    out  = exprPath s e
    out' = fromOrthoLangPath cfg out
rLoad _ _ _ = fail "bad argument to rLoad"

-- TODO is running this lots of times at once the problem?
-- TODO see if shake exports code for only hashing when timestamps change
-- TODO remove ext? not sure it does anything
aLoadHash :: Bool -> OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangPath -> String -> Action OrthoLangPath
aLoadHash hashSeqIDs cfg ref ids src _ = do
  alwaysRerun
  -- liftIO $ putStrLn $ "aLoadHash " ++ show src
  need' cfg ref "ortholang.core.compile.basic.aLoadHash" [src']
  md5 <- hashContent cfg ref src -- TODO permission error here?
  let tmpDir'   = fromOrthoLangPath cfg $ cacheDir cfg "load" -- TODO should IDs be written to this + _ids.txt?
      hashPath' = tmpDir' </> md5 -- <.> ext
      hashPath  = toOrthoLangPath cfg hashPath'
  if not hashSeqIDs
    then symlink cfg ref hashPath src
    else do
      let idsPath' = hashPath' <.> "ids"
          idsPath  = toOrthoLangPath cfg idsPath'

      unlessExists idsPath' $ hashIDsFile2 cfg ref src hashPath

      let (OrthoLangPath k) = hashPath
          v = takeFileName src'
      newIDs <- readHashedIDs cfg ref idsPath
      liftIO $ atomicModifyIORef' ids $
        \h@(HashedIDs {hFiles = f, hSeqIDs = s}) -> (h { hFiles  = M.insert k v f
                                                       , hSeqIDs = M.insert k newIDs s}, ())

  return hashPath
  where
    src' = fromOrthoLangPath cfg src

-- This is hacky, but should work with multiple protocols like http(s):// and ftp://
isURL :: String -> Bool
isURL s = "://" `isInfixOf` take 10 s

-- TODO move to Load module?
curl :: OrthoLangConfig -> Locks -> String -> Action OrthoLangPath
curl cfg ref url = do
  -- liftIO $ putStrLn $ "url: " ++ url
  let verbosity = if isJust (cfgDebug cfg) then "" else "-s"
      cDir      = fromOrthoLangPath cfg $ cacheDir cfg "curl"
      outPath   = cDir </> digest url
  liftIO $ createDirectoryIfMissing True cDir
  runCmd cfg ref $ CmdDesc
    { cmdBinary = "download.sh"
    , cmdArguments = [outPath, url, verbosity]
    , cmdFixEmpties = False
    , cmdParallel = False
    , cmdInPatterns = []
    , cmdOutPath = outPath
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions = []
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [outPath]
    }
  return $ toOrthoLangPath cfg outPath

aLoad :: Bool -> OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangPath -> OrthoLangPath -> Action ()
aLoad hashSeqIDs cfg ref ids strPath outPath = do
  alwaysRerun
  need' cfg ref "ortholang.core.compile.basic.aLoad" [strPath']
  pth <- fmap (headOrDie "read lits in aLoad failed") $ readLits cfg ref strPath' -- TODO safer!
  -- liftIO $ putStrLn $ "pth: " ++ pth
  pth' <- if isURL pth
            then curl cfg ref pth
            else fmap (toOrthoLangPath cfg . toAbs) $ liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) pth
  -- liftIO $ putStrLn $ "pth': " ++ show pth'
  -- src' <- if isURL pth
            -- then curl cfg ref pth
            -- else 
  -- liftIO $ putStrLn $ "src': " ++ src'
  -- debugA $ "aLoad src': '" ++ src' ++ "'"
  -- debugA $ "aLoad outPath': '" ++ outPath' ++ "'"
  -- TODO why doesn't this rerun?
  hashPath <- aLoadHash hashSeqIDs cfg ref ids pth' (takeExtension outPath')
  -- let hashPath'    = fromOrthoLangPath cfg hashPath
      -- hashPathRel' = ".." </> ".." </> makeRelative (cfgTmpDir cfg) hashPath'
  symlink cfg ref outPath'' hashPath
  -- trackWrite' cfg [outPath'] -- TODO WTF? why does this not get called by symlink?
  where
    strPath'  = fromOrthoLangPath cfg strPath
    outPath'  = fromOrthoLangPath cfg outPath
    outPath'' = traceA "aLoad" outPath [strPath', outPath']
    toAbs line = if isAbsolute line
                   then line
                   else cfgWorkDir cfg </> line

rLoadList :: Bool -> RulesFn
rLoadList hashSeqIDs s e@(OrthoLangFun (ListOf r) _ _ _ [es])
  | r `elem` [str, num] = rLoadListLits s es
  | otherwise = rLoadListPaths hashSeqIDs s e
rLoadList _ _ _ = fail "bad arguments to rLoadList"

-- special case for lists of str and num
-- TODO remove rtn and use (typeOf expr)?
-- TODO is this different from rListOne, except in its return type?
-- TODO is it different from rLink? seems like it's just a copy/link operation...
-- TODO don't need to hash seqids here right?
rLoadListLits :: RulesFn
rLoadListLits s@(_, cfg, ref, ids) expr = do
  (ExprPath litsPath') <- rExpr s expr
  let litsPath = toOrthoLangPath cfg litsPath'
  outPath' %> \_ -> aLoadListLits cfg ref ids outPath litsPath
  return (ExprPath outPath')
  where
    outPath  = exprPath s expr
    outPath' = fromOrthoLangPath cfg outPath

aLoadListLits :: OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangPath -> OrthoLangPath -> Action ()
aLoadListLits cfg ref _ outPath litsPath = do
  let litsPath' = fromOrthoLangPath cfg litsPath
      out       = traceA "aLoadListLits" outPath' [outPath', litsPath']
  lits  <- readLits cfg ref litsPath'
  lits' <- liftIO $ mapM absolutize lits
  writeLits cfg ref out lits'
  where
    outPath' = fromOrthoLangPath cfg outPath

-- regular case for lists of any other file type
-- TODO hash mismatch here?
rLoadListPaths :: Bool -> RulesFn
rLoadListPaths hashSeqIDs s@(_, cfg, ref, ids) e@(OrthoLangFun rtn salt _ _ [es]) = do
  (ExprPath pathsPath) <- rExpr s es
  -- let hash     = digest $ toOrthoLangPath cfg pathsPath
  --     outPath  = exprPathExplicit cfg "list" rtn salt [hash]
  let outPath  = exprPath s e
      outPath' = fromOrthoLangPath cfg outPath
  outPath' %> \_ -> aLoadListLinks hashSeqIDs cfg ref ids (toOrthoLangPath cfg pathsPath) outPath
  return (ExprPath outPath')
rLoadListPaths _ _ _ = fail "bad arguments to rLoadListPaths"

aLoadListLinks :: Bool -> OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangPath -> OrthoLangPath -> Action ()
aLoadListLinks hashSeqIDs cfg ref ids pathsPath outPath = do
  -- Careful! The user will write paths relative to workdir and those come
  -- through as a (ListOf str) here; have to read as Strings and convert to
  -- OrthoLangPaths
  -- alwaysRerun -- TODO does this help?
  paths <- readLitPaths cfg ref pathsPath'
  let paths' = map (fromOrthoLangPath cfg) paths
  paths'' <- liftIO $ mapM (resolveSymlinks $ Just $ cfgTmpDir cfg) paths'
  let paths''' = map (toOrthoLangPath cfg) paths''
  hashPaths <- mapM (\p -> aLoadHash hashSeqIDs cfg ref ids p
                         $ takeExtension $ fromOrthoLangPath cfg p) paths'''
  let hashPaths' = map (fromOrthoLangPath cfg) hashPaths
  -- debugA $ "about to need: " ++ show paths''
  need' cfg ref "ortholang.core.compile.basic.aLoadListLinks" hashPaths'
  writePaths cfg ref out hashPaths
  where
    outPath'   = fromOrthoLangPath cfg outPath
    pathsPath' = fromOrthoLangPath cfg pathsPath
    out = traceA "aLoadListLinks" outPath' [outPath', pathsPath']
