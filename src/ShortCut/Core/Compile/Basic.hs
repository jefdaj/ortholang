-- Once text has been parsed into an abstract syntax tree (Parse.hs), this
-- module "compiles" it by translating it into a set of Shake build rules. To
-- actually run the rules, use `eval` in the Interpret module.

-- TODO add more descriptive runtime error for canonicalizePath failing b/c no file
-- TODO see if you can avoid making more than one absolute symlink per input file
-- TODO make systematically sure there's only one rule for each file
-- TODO pass tmpDir as a config option somehow, and verbosity

-- TODO why doesn't turning down the verbosity actually work?

module ShortCut.Core.Compile.Basic
--   ( compileScript
--   , rBop
--   , rExpr
--   , rList
--   , addPrefixes
--   )
  where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Paths (cacheDir, exprPath, exprPathExplicit, toCutPath,
                            fromCutPath, varPath, CutPath)

import Control.Monad              (when)
import Data.List                  (find, intersperse)
import Development.Shake.FilePath ((</>), (<.>))
import ShortCut.Core.Debug        (debugAction, debugRules)
import ShortCut.Core.Actions      (removeIfExists, wrappedCmdWrite,
                                   readLit, readLits, writeLits, hashContent,
                                   readLitPaths, hashContent, writePaths, symlink,
                                   writeDeduped, debugTrackWrite)
import ShortCut.Core.Util         (absolutize, resolveSymlinks, stripWhiteSpace,
                                   digest, typesMatch)
import System.Directory           (createDirectoryIfMissing)
import System.FilePath            (takeExtension)


------------------------------
-- compile the ShortCut AST --
------------------------------

rExpr :: CutState -> CutExpr -> Rules ExprPath
rExpr s e@(CutLit _ _ _      ) = rLit s e
rExpr s e@(CutRef _ _ _ _    ) = rRef s e
rExpr s e@(CutList _ _ _ _   ) = rList s e
rExpr s e@(CutBop _ _ _ n _ _) = rulesByName s e n -- TODO turn into Fun?
rExpr s e@(CutFun _ _ _ n _  ) = rulesByName s e n

-- TODO remove once no longer needed (parser should find fns)
rulesByName :: CutState -> CutExpr -> String -> Rules ExprPath
rulesByName s@(_,cfg) expr name = case findByName cfg name of
  Nothing -> error $ "no such function '" ++ name ++ "'"
  Just f  -> (fRules f) s expr

-- TODO remove once no longer needed (parser should find fns)
findByName :: CutConfig -> String -> Maybe CutFunction
findByName cfg name = find (\f -> fName f == name) fs
  where
    ms = cfgModules cfg
    fs = concatMap mFunctions ms

rAssign :: CutState -> CutAssign -> Rules (CutVar, VarPath)
rAssign s@(_,cfg) (var, expr) = do
  (ExprPath path) <- rExpr s expr
  path' <- rVar s var expr $ toCutPath cfg path
  let res  = (var, path')
      res' = debugRules cfg "rAssign" (var, expr) res
  return res'

-- TODO how to fail if the var doesn't exist??
--      (or, is that not possible for a typechecked AST?)
compileScript :: CutState -> Maybe String -> Rules ResPath
compileScript s@(as,_) permHash = do
  -- TODO this can't be done all in parallel because they depend on each other,
  --      but can parts of it be parallelized? or maybe it doesn't matter because
  --      evaluating the code itself is always faster than the system commands
  rpaths <- mapM (rAssign s) as
  case lookup (CutVar res) rpaths of
    Nothing -> fail "no result variable. that's not right!"
    Just (VarPath r) -> return $ ResPath r
  where
    -- p here is "result" + the permutation name/hash if there is one right?
    res = case permHash of
      Nothing -> "result"
      Just h  -> "result." ++ h

-- write a literal value from ShortCut source code to file
rLit :: CutState -> CutExpr -> Rules ExprPath
rLit s@(_,cfg) expr = do
  let path  = exprPath s expr -- absolute paths allowed!
      path' = debugRules cfg "rLit" expr $ fromCutPath cfg path
  path' %> \_ -> aLit cfg expr path
  return (ExprPath path')

-- TODO take the path, not the expression?
aLit :: CutConfig -> CutExpr -> CutPath -> Action ()
aLit cfg expr out = writeDeduped cfg writeLits out'' [ePath] -- TODO too much dedup?
  where
    paths :: CutExpr -> FilePath
    paths (CutLit _ _ p) = p
    paths _ = error "bad argument to paths"
    ePath = paths expr
    out'  = fromCutPath cfg out
    out'' = debugAction cfg "aLit" out' [ePath, out']

rList :: CutState -> CutExpr -> Rules ExprPath
-- TODO is this the bug? refers to a list of other empty lists, no?
-- rList s e@(CutList Empty _ _ _) = rListEmpty s e -- TODO remove?
rList s e@(CutList rtn _ _ _)
  | rtn `elem` [str, num] = rListLits  s e
  | otherwise             = rListPaths s e
rList _ _ = error "bad arguemnt to rList"

-- special case for empty lists
-- TODO is a special type for this really needed?
rListEmpty :: (CutScript, CutConfig) -> CutExpr -> Rules ExprPath
rListEmpty s@(_,cfg) e@(CutList Empty _ _ _) = do
  let link  = exprPath s e
      link' = debugRules cfg "rListEmpty" e $ fromCutPath cfg link
  link' %> \_ -> aListEmpty cfg link
  return (ExprPath link')
rListEmpty _ e = error $ "bad arguemnt to rListEmpty: " ++ show e

-- TODO is this actually needed? seems the same as lits or paths really
--      (also, is there a need to write empty lists at all?)
aListEmpty :: CutConfig -> CutPath -> Action ()
aListEmpty cfg link = do
  -- TODO should the wrappedCmd stuff be CutPaths or plain FilePaths?
  wrappedCmdWrite cfg link'' [link''] [] "touch" [link'] -- TODO quietly?
  -- debugTrackWrite cfg [link''] -- TODO this should use CutPaths
  where
    link'  = fromCutPath cfg link
    link'' = debugAction cfg "aListEmpty" link' [link']

-- special case for writing lists of strings or numbers as a single file
rListLits :: (CutScript, CutConfig) -> CutExpr -> Rules ExprPath
rListLits s@(_,cfg) e@(CutList _ _ _ exprs) = do
  litPaths <- mapM (rExpr s) exprs
  let litPaths' = map (\(ExprPath p) -> toCutPath cfg p) litPaths
  outPath' %> \_ -> aListLits cfg litPaths' outPath
  return (ExprPath outPath')
  where
    outPath  = exprPath s e
    outPath' = debugRules cfg "rListLits" e $ fromCutPath cfg outPath
rListLits _ e = error $ "bad argument to rListLits: " ++ show e

-- TODO put this in a cache dir by content hash and link there
aListLits :: CutConfig -> [CutPath] -> CutPath -> Action ()
aListLits cfg paths outPath = do
  need paths'
  lits <- mapM (readLit cfg) paths'
  let lits' = map stripWhiteSpace lits
  writeDeduped cfg writeLits out'' lits'
  where
    out'   = fromCutPath cfg outPath
    out''  = debugAction cfg "aListLits" out' (out':paths')
    paths' = map (fromCutPath cfg) paths

-- regular case for writing a list of links to some other file type
rListPaths :: (CutScript, CutConfig) -> CutExpr -> Rules ExprPath
rListPaths s@(_,cfg) e@(CutList rtn salt _ exprs) = do
  paths <- mapM (rExpr s) exprs
  let paths'   = map (\(ExprPath p) -> toCutPath cfg p) paths
      hash     = digest $ concat $ map digest paths'
      outPath  = exprPathExplicit cfg "list" (ListOf rtn) salt [hash]
      outPath' = debugRules cfg "rListPaths" e $ fromCutPath cfg outPath
  outPath' %> \_ -> aListPaths cfg paths' outPath
  return (ExprPath outPath')
rListPaths _ _ = error "bad arguemnts to rListPaths"

-- works on everything but lits: paths or empty lists
aListPaths :: CutConfig -> [CutPath] -> CutPath -> Action ()
aListPaths cfg paths outPath = do
  need paths'
  paths'' <- liftIO $ mapM (resolveSymlinks cfg True) paths'
  need paths''
  let paths''' = map (toCutPath cfg) paths'' -- TODO not working?
  writeDeduped cfg writePaths out'' paths'''
  where
    out'   = fromCutPath cfg outPath
    out''  = debugAction cfg "aListPaths" out' (out':paths')
    paths' = map (fromCutPath cfg) paths -- TODO remove this

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
rRef :: CutState -> CutExpr -> Rules ExprPath
rRef (_,cfg) e@(CutRef _ _ _ var) = return $ ePath $ varPath cfg var e
  where
    ePath p = ExprPath $ debugRules cfg "rRef" e $ fromCutPath cfg p
rRef _ _ = error "bad argument to rRef"

-- Creates a symlink from varname to expression file.
-- TODO unify with rLink2, rLoad etc?
-- TODO do we need both the CutExpr and ExprPath? seems like CutExpr would do
rVar :: CutState -> CutVar -> CutExpr -> CutPath -> Rules VarPath
rVar (_,cfg) var expr oPath = do
  vPath' %> \_ -> aVar cfg vPath oPath
  return (VarPath vPath')
  where
    vPath  = varPath cfg var expr
    vPath' = debugRules cfg "rVar" var $ fromCutPath cfg vPath

aVar :: CutConfig -> CutPath -> CutPath -> Action ()
aVar cfg vPath oPath = do
  alwaysRerun
  liftIO $ removeIfExists vPath'
  need [oPath']
  -- liftIO $ createDirectoryIfMissing True $ takeDirectory link'
  symlink cfg vPath'' oPath
  -- debugTrackWrite cfg [vPath'] -- TODO why doesn't symlink handle this??
  where
    oPath'  = fromCutPath cfg oPath
    vPath'  = fromCutPath cfg vPath
    vPath'' = debugAction cfg "aVar" vPath [vPath', oPath']
    -- TODO utility fn for these? and also for ln using them?
    -- destr  = ".." </> (makeRelative (cfgTmpDir cfg) dest')
    -- linkr  = ".." </> (makeRelative (cfgTmpDir cfg) link')

-- Handles the actual rule generation for all binary operators.
-- TODO can it be factored out somehow? seems almost trivial now...
rBop :: CutState -> CutExpr -> (CutExpr, CutExpr)
      -> Rules (ExprPath, ExprPath, ExprPath)
rBop s@(_,cfg) e@(CutBop _ _ _ _ _ _) (n1, n2) = do
  (ExprPath p1) <- rExpr s n1
  (ExprPath p2) <- rExpr s n2
  let path  = fromCutPath cfg $ exprPath s e
      path' = debugRules cfg "rBop" e path
  return (ExprPath p1, ExprPath p2, ExprPath path')
rBop _ _ _ = error "bad argument to rBop"

------------------------------
-- [t]ypechecking functions --
------------------------------

typeError :: [CutType] -> [CutType] -> String
typeError expected actual =
  "Type error:\nexpected " ++ show expected
           ++ "\nbut got " ++ show actual

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
      oPath  = fromCutPath cfg $ exprPath s expr
  oPath %> \_ -> aOneArgScript cfg oPath script tmpDir argPath
  return (ExprPath oPath)
rOneArgScript _ _ _ _ = error "bad argument to rOneArgScript"

aOneArgScript :: CutConfig -> String
              -> FilePath -> FilePath -> FilePath -> Action ()
aOneArgScript cfg oPath script tmpDir argPath = do
  need [argPath]
  liftIO $ createDirectoryIfMissing True tmpDir
  let oPath' = debugAction cfg "aOneArgScript" oPath [oPath,script,tmpDir,argPath]
  quietly $ wrappedCmdWrite cfg oPath' [oPath'] [] script [tmpDir, oPath, argPath]
  -- trackWrite [oPath']

-- for scripts that take one arg and return a list of lits
-- TODO this should put tmpfiles in cache/<script name>!
-- TODO name something more explicitly about fasta files?
rOneArgListScript :: FilePath -> FilePath -> CutState -> CutExpr -> Rules ExprPath
rOneArgListScript tmpName script s@(_,cfg) expr@(CutFun _ _ _ _ [fa]) = do
  (ExprPath faPath) <- rExpr s fa
  let tmpDir  = fromCutPath cfg $ cacheDir cfg tmpName
      outPath = fromCutPath cfg $ exprPath s expr
  outPath %> \_ -> aOneArgListScript cfg outPath script tmpDir faPath
  return (ExprPath outPath)
rOneArgListScript _ _ _ _ = error "bad argument to rOneArgListScript"

aOneArgListScript :: CutConfig -> FilePath
                  -> String -> FilePath -> FilePath -> Action ()
aOneArgListScript cfg outPath script tmpDir faPath = do
  need [faPath]
  liftIO $ createDirectoryIfMissing True tmpDir
  let out = debugAction cfg "aOneArgListScript" outPath [outPath, script, tmpDir, faPath]
  wrappedCmdWrite cfg out [out] [Cwd tmpDir] script [out, faPath]
  -- debugTrackWrite cfg [out]

--------------------------
-- links to input files --
--------------------------

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
  , fRules     = rLoad
  }

{- Like cLoad, except it operates on a list of strings. Note that you can also
 - load lists using cLoad, but it's not recommended because then you have to
 - write the list in a file, whereas this can handle literal lists in the
 - source code.
 -}
mkLoadList :: String -> CutType -> CutFunction
mkLoadList name rtn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [(ListOf str)] (ListOf rtn)
  , fFixity    = Prefix
  , fRules     = rLoadList
  }

-- The paths here are a little confusing: expr is a str of the path we want to
-- link to. So after compiling it we get a path to *that str*, and have to read
-- the file to access it. Then we want to `ln` to the file it points to.
rLoad :: CutState -> CutExpr -> Rules ExprPath
rLoad s@(_,cfg) e@(CutFun _ _ _ _ [p]) = do
  (ExprPath strPath) <- rExpr s p
  out' %> \_ -> aLoad cfg (toCutPath cfg strPath) out
  return (ExprPath out')
  where
    out  = exprPath s e
    out' = fromCutPath cfg out
rLoad _ _ = error "bad argument to rLoad"

aLoadHash :: CutConfig -> CutPath -> String -> Action CutPath
aLoadHash cfg src ext = do
  need [src']
  md5 <- hashContent cfg src -- TODO permission error here?
  let tmpDir'   = fromCutPath cfg $ cacheDir cfg "load"
      hashPath' = tmpDir' </> md5 <.> ext
      hashPath  = toCutPath cfg hashPath'
  -- Careful! Removing some of this once caused lockfiles to conflict and some
  -- tests froze until I figured it out :(
  done <- doesFileExist hashPath'
  when (not done) $ do
    liftIO $ createDirectoryIfMissing True tmpDir'
    symlink cfg hashPath src
    debugTrackWrite cfg [hashPath'] -- TODO WTF? why does this not get called by symlink?

  return hashPath
  where
    src' = fromCutPath cfg src

aLoad :: CutConfig -> CutPath -> CutPath -> Action ()
aLoad cfg strPath outPath = do
  need [strPath']
  pth <- readLitPaths cfg strPath'
  src' <- liftIO $ resolveSymlinks cfg True $ fromCutPath cfg $ head pth -- TODO safer!
  hashPath <- aLoadHash cfg (toCutPath cfg src') (takeExtension outPath')
  -- let hashPath'    = fromCutPath cfg hashPath
      -- hashPathRel' = ".." </> ".." </> makeRelative (cfgTmpDir cfg) hashPath'
  symlink cfg outPath'' hashPath
  debugTrackWrite cfg [outPath'] -- TODO WTF? why does this not get called by symlink?
  where
    strPath'  = fromCutPath cfg strPath
    outPath'  = fromCutPath cfg outPath
    outPath'' = debugAction cfg "aLoad" outPath [strPath', outPath']

rLoadList :: RulesFn
rLoadList s e@(CutFun (ListOf r) _ _ _ [es])
  | r `elem` [str, num] = rLoadListLits s es
  | otherwise = rLoadListLinks s e
rLoadList _ _ = error "bad arguments to rLoadList"

-- special case for lists of str and num
-- TODO remove rtn and use (typeOf expr)?
-- TODO is this different from rListOne, except in its return type?
-- TODO is it different from rLink? seems like it's just a copy/link operation...
rLoadListLits :: RulesFn
rLoadListLits s@(_,cfg) expr = do
  (ExprPath litsPath') <- rExpr s expr
  let litsPath = toCutPath cfg litsPath'
  outPath' %> \_ -> aLoadListLits cfg outPath litsPath
  return (ExprPath outPath')
  where
    outPath  = exprPath s expr
    outPath' = fromCutPath cfg outPath

aLoadListLits :: CutConfig -> CutPath -> CutPath -> Action ()
aLoadListLits cfg outPath litsPath = do
  let litsPath' = fromCutPath cfg litsPath
      out       = debugAction cfg "aLoadListLits" outPath' [outPath', litsPath']
  lits  <- readLits cfg litsPath'
  lits' <- liftIO $ mapM absolutize lits
  writeDeduped cfg writeLits out lits'
  where
    outPath' = fromCutPath cfg outPath

-- regular case for lists of any other file type
rLoadListLinks :: RulesFn
rLoadListLinks s@(_,cfg) (CutFun rtn salt _ _ [es]) = do
  (ExprPath pathsPath) <- rExpr s es
  let hash     = digest $ toCutPath cfg pathsPath
      outPath  = exprPathExplicit cfg "list" rtn salt [hash]
      outPath' = fromCutPath cfg outPath
  outPath' %> \_ -> aLoadListLinks cfg (toCutPath cfg pathsPath) outPath
  return (ExprPath outPath')
rLoadListLinks _ _ = error "bad arguments to rLoadListLinks"

aLoadListLinks :: CutConfig -> CutPath -> CutPath -> Action ()
aLoadListLinks cfg pathsPath outPath = do
  -- Careful! The user will write paths relative to workdir and those come
  -- through as a (ListOf str) here; have to read as Strings and convert to
  -- CutPaths
  paths <- readLitPaths cfg pathsPath'
  let paths' = map (fromCutPath cfg) paths
  paths'' <- liftIO $ mapM (resolveSymlinks cfg True) paths'
  let paths''' = map (toCutPath cfg) paths''
  hashPaths <- mapM (\p -> aLoadHash cfg p
                         $ takeExtension $ fromCutPath cfg p) paths'''
  let hashPaths' = map (fromCutPath cfg) hashPaths
  -- liftIO $ putStrLn $ "about to need: " ++ show paths''
  need hashPaths'
  writeDeduped cfg writePaths out hashPaths
  where
    outPath'   = fromCutPath cfg outPath
    pathsPath' = fromCutPath cfg pathsPath
    out = debugAction cfg "aLoadListLinks" outPath' [outPath', pathsPath']

-- based on https://stackoverflow.com/a/18627837
-- uniqLines :: Ord a => [a] -> [a]
-- uniqLines = unlines . toList . fromList . lines

-- takes an action fn with any number of args and calls it with a tmpdir.
-- TODO rename something that goes with the map fns?
rSimple :: (CutConfig -> [CutPath] -> Action ()) -> RulesFn
rSimple actFn = rSimple' Nothing actFn'
  where
    actFn' cfg _ args = actFn cfg args -- drop unused tmpdir

rSimpleTmp :: String
           -> (CutConfig -> CutPath -> [CutPath] -> Action ())
           -> RulesFn
rSimpleTmp prefix = rSimple' (Just prefix)

{- For scripts that just need some args passed to them. The first will be the
 - outPath, and the rest actual args. The string is the script name.
 -}
rSimpleScript :: String -> RulesFn
rSimpleScript = rSimple . aSimpleScript

aSimpleScript :: String -> (CutConfig -> [CutPath] -> Action ())
aSimpleScript script cfg (out:args) = aSimple' cfg out actFn Nothing args
  where
    actFn c o as = let o' = fromCutPath c o
                   in wrappedCmdWrite cfg o' [o'] [] script $ map (fromCutPath c) as
aSimpleScript _ _ as = error $ "bad argument to aSimpleScript: " ++ show as

-- TODO rSimpleScriptTmp?

rSimple' :: Maybe String
         -> (CutConfig -> CutPath -> [CutPath] -> Action ())
         -> RulesFn
rSimple' mTmpPrefix actFn s@(_,cfg) e@(CutFun _ _ _ _ exprs) = do
  argPaths <- mapM (rExpr s) exprs
  let argPaths' = map (\(ExprPath p) -> toCutPath cfg p) argPaths
  outPath' %> \_ -> aSimple' cfg outPath actFn mTmpDir argPaths'
  return (ExprPath outPath')
  where
    mTmpDir  = fmap (cacheDir cfg) mTmpPrefix -- TODO tables bug here?
    outPath  = exprPath s e
    outPath' = fromCutPath cfg outPath
rSimple' _ _ _ _ = error "bad argument to rSimple'"

-- TODO aSimpleScript that calls aSimple' with a wrappedCmd as the actFn
-- TODO rSimpleScript that calls rSimple + that

aSimple' :: CutConfig -> CutPath
         -> (CutConfig -> CutPath -> [CutPath] -> Action ())
         -> Maybe CutPath -> [CutPath] -> Action ()
aSimple' cfg outPath actFn mTmpDir argPaths = do
  need argPaths'
  argPaths'' <- liftIO $ mapM (fmap (toCutPath cfg) . resolveSymlinks cfg True) argPaths'
  liftIO $ createDirectoryIfMissing True tmpDir'
  actFn cfg tmpDir (outPath:argPaths'')
  trackWrite [out]
  where
    -- TODO probably not "simple tmp" anymore... remove? rename?
    hashes     = concat $ intersperse "_" $ map digest argPaths'
    argPaths'  = map (fromCutPath cfg) argPaths
    outPath'   = fromCutPath cfg outPath
    out = debugAction cfg "aSimple'" outPath' (outPath':tmpDir':argPaths')
    (tmpDir, tmpDir') = case mTmpDir of
                Nothing  -> (toCutPath cfg $ cfgTmpDir cfg, cfgTmpDir cfg)
                Just dir -> (toCutPath cfg d, d)
                  where
                    d = fromCutPath cfg dir </> hashes
