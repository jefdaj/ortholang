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
                            fromCutPath, varPath, writePaths, CutPath, readLitPaths,
                            readLit, readLits, writeLits, hashContent)

import Control.Monad               (when)
import Data.List                   (find, sort, intersperse)
import Data.Maybe                  (fromJust)
import Development.Shake.FilePath  ((</>), (<.>))
import ShortCut.Core.Cmd           (wrappedCmd)
import ShortCut.Core.Debug         (debugTrackWrite, debugAction, debugRules)
import ShortCut.Core.Util          (absolutize, resolveSymlinks, stripWhiteSpace,
                                    digest, typesMatch)
import System.Directory            (createDirectoryIfMissing)
import System.FilePath             (takeDirectory, makeRelative)


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
  -- liftIO $ putStrLn $ "trying to look up result"
  let (VarPath r) = fromJust $ lookup (CutVar res) rpaths
  -- return $ ResPath $ makeRelative (cfgTmpDir cfg) r
  return $ ResPath r
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
  wrappedCmd cfg [link'] [] "touch" [link'] -- TODO quietly?
  debugTrackWrite cfg [link''] -- TODO this should use CutPaths
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

{- This ensures that when two lists have the same content, their expression
 - paths will be links to the same cached path. That causes them to get
 - properly deduplicated when used in a set operation. It also makes the .tree
 - test files much stricter, since they'll change if any list element changes.
 -
 - TODO does it need to handle a race condition when writing to the cache?
 - TODO any reason to keep original extensions instead of all using .txt?
 -      oh, if we're testing extensions anywhere. lets not do that though
 -}
writeDeduped :: Show a => CutConfig
             -> (CutConfig -> FilePath -> a -> Action ())
             -> FilePath -> a -> Action ()
writeDeduped cfg writeFn outPath content = do
  let cDir     = fromCutPath cfg $ cacheDir cfg "list" -- TODO make relative to expr
      cache    = cDir </> digest content <.> "txt"
      cacheRel = ".." </> ".." </> makeRelative (cfgTmpDir cfg) cache
  done <- doesFileExist cache
  liftIO $ createDirectoryIfMissing True cDir
  when (not done) (writeFn cfg cache content)
  wrappedCmd cfg [outPath] [] "ln" ["-fs", cacheRel, outPath] -- TODO quietly?
  debugTrackWrite cfg [outPath]

-- TODO put this in a cache dir by content hash and link there
aListLits :: CutConfig -> [CutPath] -> CutPath -> Action ()
aListLits cfg paths outPath = do
  need paths'
  lits <- mapM (readLit cfg) paths'
  let lits' = sort $ map stripWhiteSpace lits
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
  paths'' <- liftIO $ mapM (resolveSymlinks cfg) paths'
  need paths''
  let paths''' = map (toCutPath cfg) paths''
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
rVar (_,cfg) var expr dest = do
  link' %> \_ -> aVar cfg dest link
  return (VarPath link')
  where
    link  = varPath cfg var expr
    link' = debugRules cfg "rVar" var $ fromCutPath cfg link

aVar :: CutConfig -> CutPath -> CutPath -> Action ()
aVar cfg dest link = do
  alwaysRerun
  need [dest']
  liftIO $ createDirectoryIfMissing True $ takeDirectory link'
  wrappedCmd cfg [link'] [] "ln" ["-fs", destr, link''] -- TODO quietly?
  debugTrackWrite cfg [link'']
  where
    dest'  = fromCutPath cfg dest
    link'  = fromCutPath cfg link
    link'' = debugAction cfg "aVar" link' [link', dest']
    -- TODO utility fn for these? and also for ln using them?
    destr  = ".." </> (makeRelative (cfgTmpDir cfg) dest')
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
  quietly $ unit $ wrappedCmd cfg [oPath] [] script [tmpDir, oPath, argPath]
  let oPath' = debugAction cfg "aOneArgScript" oPath [oPath,script,tmpDir,argPath]
  trackWrite [oPath']

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
  wrappedCmd cfg [outPath] [Cwd tmpDir] script [outPath, faPath]
  let out = debugAction cfg "aOneArgListScript" outPath [outPath, script, tmpDir, faPath]
  debugTrackWrite cfg [out]

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

-- TODO add extensions?
aLoadHash :: CutConfig -> CutPath -> Action CutPath
aLoadHash cfg src = do
  need [src']
  md5 <- hashContent cfg src
  let tmpDir'      = fromCutPath cfg $ cacheDir cfg "load"
      hashPath'    = tmpDir' </> md5
      hashPath     = toCutPath cfg hashPath'
  done <- doesFileExist hashPath'
  when (not done) $ do
    liftIO $ createDirectoryIfMissing True tmpDir'
    unit $ quietly $ wrappedCmd cfg [hashPath'] [] "ln" ["-fs", src', hashPath']
    debugTrackWrite cfg [hashPath']
  return hashPath
  where
    src' = fromCutPath cfg src

aLoad :: CutConfig -> CutPath -> CutPath -> Action ()
aLoad cfg strPath outPath = do
  need [strPath']
  pth <- readLitPaths cfg strPath'
  src' <- liftIO $ resolveSymlinks cfg $ fromCutPath cfg $ head pth -- TODO safer!
  hashPath <- aLoadHash cfg $ toCutPath cfg src'
  let hashPath'    = fromCutPath cfg hashPath
      hashPathRel' = ".." </> ".." </> makeRelative (cfgTmpDir cfg) hashPath'
  unit $ quietly $ wrappedCmd cfg [outPath''] [] "ln" ["-fs", hashPathRel', outPath'']
  debugTrackWrite cfg [outPath'']
  where
    strPath' = fromCutPath cfg strPath
    outPath' = fromCutPath cfg outPath
    outPath'' = debugAction cfg "aLoad" outPath' [strPath', outPath']

rLoadList :: RulesFn
rLoadList s e@(CutFun r _ _ _ [es])
  | r `elem` [ListOf str, ListOf num] = rLoadListLits s es
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
  paths'' <- liftIO $ mapM (resolveSymlinks cfg) paths'
  let paths''' = map (toCutPath cfg) paths''
  hashPaths <- mapM (aLoadHash cfg) paths'''
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
    actFn c o as = wrappedCmd cfg [fromCutPath c o] [] script
                 $ map (fromCutPath c) as
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
  argPaths'' <- liftIO $ mapM (fmap (toCutPath cfg) . resolveSymlinks cfg) argPaths'
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
