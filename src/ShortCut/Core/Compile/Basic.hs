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

-- TODO remove Old path fns
import qualified ShortCut.Core.Compile.Paths as Old
import ShortCut.Core.Paths3 (cacheDir, exprPath, exprPathExplicit, toCutPath, fromCutPath)
-- import Path (fromCutPath cfg, fromCutPath cfg)

import Data.List                   (find, sort)
import Data.Maybe                  (fromJust)
import Development.Shake.FilePath  ((</>))
import ShortCut.Core.Config        (wrappedCmd)
import ShortCut.Core.Debug         (debugReadFile, debugWriteLines, debugWriteFile,
                                    debugTrackWrite, debugReadLines, debugAction,
                                    debugRules)
import ShortCut.Core.Util          (absolutize, resolveSymlinks, stripWhiteSpace,
                                    digest)
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
  path  <- rExpr s expr
  path' <- rVar s var expr path
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
  let path  = fromCutPath cfg $ exprPath s expr -- absolute paths allowed!
      path' = debugRules cfg "rLit" expr path
  path %> aLit cfg expr
  return (ExprPath path')

rList :: CutState -> CutExpr -> Rules ExprPath
rList s e@(CutList EmptyList _ _ _) = rListEmpty s e -- TODO remove?
rList s e@(CutList rtn _ _ _)
  | rtn `elem` [str, num] = rListLits s e
  | otherwise = rListPaths s e
rList _ _ = error "bad arguemnt to rList"

-- special case for empty lists
-- TODO is a special type for this really needed?
rListEmpty :: (CutScript, CutConfig) -> CutExpr -> Rules ExprPath
rListEmpty s@(_,cfg) e@(CutList EmptyList _ _ _) = do
  let link  = fromCutPath cfg $ exprPath s e
      link' = debugRules cfg "rListEmpty" e link
  link %> \_ -> aListEmpty cfg link
  return (ExprPath link')
rListEmpty _ e = error $ "bad arguemnt to rListEmpty: " ++ show e

-- TODO is this actually needed? seems the same as lits or paths really
aListEmpty :: CutConfig -> FilePath -> Action ()
aListEmpty cfg link = do
  wrappedCmd cfg [link] [] "touch" [link] -- TODO quietly?
  debugTrackWrite cfg [link']
  where
    link' = debugAction cfg "aListEmpty" link [link]

-- special case for writing lists of strings or numbers as a single file
rListLits :: (CutScript, CutConfig) -> CutExpr -> Rules ExprPath
rListLits s@(_,cfg) e@(CutList _ _ _ exprs) = do
  litPaths <- mapM (rExpr s) exprs
  let litPaths' = map (\(ExprPath p) -> p) litPaths
      relPaths  = map (makeRelative $ cfgTmpDir cfg) litPaths'
      outPath  = fromCutPath cfg $ exprPath s e
      outPath' = debugRules cfg "rListLits" e outPath
  outPath %> \_ -> aListLits cfg outPath relPaths
  return (ExprPath outPath')
rListLits _ e = error $ "bad argument to rListLits: " ++ show e

aListLits :: CutConfig -> FilePath -> [FilePath] -> Action ()
aListLits cfg outPath relPaths = do
  let paths = map (cfgTmpDir cfg </>) relPaths -- TODO utility fn for this
  need paths
  lits <- mapM (debugReadFile cfg) paths
  let lits' = sort $ map stripWhiteSpace lits
      out'  = debugAction cfg "aListLits" outPath (outPath:relPaths)
  debugWriteLines cfg out' lits'

-- regular case for writing a list of links to some other file type
rListPaths :: (CutScript, CutConfig) -> CutExpr -> Rules ExprPath
rListPaths s@(_,cfg) e@(CutList rtn salt _ exprs) = do
  paths <- mapM (rExpr s) exprs
  let paths'   = map (\(ExprPath p) -> p) paths
      hash     = digest $ concat $ map (digest . toCutPath cfg) paths' -- TODO utility fn
      outPath  = fromCutPath cfg $ exprPathExplicit s "list" (ListOf rtn) salt [hash]
      outPath' = debugRules cfg "rListPaths" e outPath
  outPath %> \_ -> aListPaths cfg outPath paths'
  return (ExprPath outPath')
rListPaths _ _ = error "bad arguemnts to rListPaths"

-- works on everything but lits: paths or empty lists
aListPaths :: CutConfig -> FilePath -> [FilePath] -> Action ()
aListPaths cfg outPath paths = do
  need paths
  let out = debugAction cfg "aListPaths" outPath (outPath:paths)
  debugWriteLines cfg out paths

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
rRef :: CutState -> CutExpr -> Rules ExprPath
rRef (_,cfg) e@(CutRef _ _ _ var) = return $ ePath $ Old.varPath cfg var e
  where
    ePath (VarPath p) = ExprPath $ debugRules cfg "rRef" e p
rRef _ _ = error "bad argument to rRef"

-- Creates a symlink from varname to expression file.
-- TODO unify with rLink2, rLoadOne etc?
-- TODO do we need both the CutExpr and ExprPath? seems like CutExpr would do
rVar :: CutState -> CutVar -> CutExpr -> ExprPath -> Rules VarPath
rVar (_,cfg) var expr (ExprPath dest) = do
  let (VarPath link) = Old.varPath cfg var expr
      -- TODO is this needed? maybe just have links be absolute?
      linkd = debugRules cfg "rVar" var link
  link %> \_ -> aVar cfg dest link
  return (VarPath linkd)

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

-- this mostly checks equality, but also has to deal with how an empty list can
-- be any kind of list
-- TODO is there any more elegant way? this seems error-prone...
typeMatches :: CutType -> CutType -> Bool
typeMatches EmptyList  (ListOf _) = True
typeMatches (ListOf _) EmptyList  = True
typeMatches a b = a == b

typesMatch :: [CutType] -> [CutType] -> Bool
typesMatch as bs = sameLength && allMatch
  where
    sameLength = length as == length bs
    allMatch   = all (\(a,b) -> a `typeMatches` b) (zip as bs)

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

--------------------------
-- links to input files --
--------------------------

-- The paths here are a little confusing: expr is a str of the path we want to
-- link to. So after compiling it we get a path to *that str*, and have to read
-- the file to access it. Then we want to `ln` to the file it points to.
rLink :: CutState -> CutExpr -> CutExpr -> Rules ExprPath
rLink s@(_,cfg) outExpr strExpr = do
  (ExprPath strPath) <- rExpr s strExpr
  let outPath = fromCutPath cfg $ exprPath s outExpr
  outPath %> aLink cfg strPath
  return (ExprPath outPath)

aLink :: CutConfig -> FilePath -> FilePath -> Action ()
aLink cfg strPath outPath = do
  pth <- fmap stripWhiteSpace $ readFile' strPath
  src <- liftIO $ absolutize pth -- TODO make relative to workdir instead
  need [src]
  unit $ quietly $ wrappedCmd cfg [outPath] [] "ln" ["-fs", src, outPath]
  let out = debugAction cfg "aLink" outPath [outPath, strPath]
  debugTrackWrite cfg [out]

-- TODO remove this?
rLoadOne :: RulesFn
rLoadOne s e@(CutFun _ _ _ _ [p]) = rLink s e p
rLoadOne _ _ = error "bad argument to rLoadOne"

rLoadList :: RulesFn
rLoadList s e@(CutFun _ _ _ _ [es])
  | typeOf es `elem` [ListOf str, ListOf num] = rLoadListLits s es
  | otherwise = rLoadListLinks s e
rLoadList _ _ = error "bad arguments to rLoadList"

-- special case for lists of str and num
-- TODO remove rtn and use (typeOf expr)?
-- TODO is this different from rListOne, except in its return type?
-- TODO is it different from rLink? seems like it's just a copy/link operation...
rLoadListLits :: RulesFn
rLoadListLits s@(_,cfg) expr = do
  (ExprPath litsPath) <- rExpr s expr
  -- let relPath = makeRelative (cfgTmpDir cfg) litsPath
      -- (ExprPath outPath) = Old.exprPathExplicit cfg True (ListOf rtn) "cut_list" [relPath]
  let outPath = fromCutPath cfg $ exprPath s expr
  outPath %> \_ -> aLoadListLits cfg outPath litsPath
  return (ExprPath outPath)

-- regular case for lists of any other file type
rLoadListLinks :: RulesFn
rLoadListLinks s@(_,cfg) e@(CutFun _ _ _ _ [es]) = do
  (ExprPath pathsPath) <- rExpr s es
  -- TODO is relPath enough to make sure it's unique??
  let relPath = makeRelative (cfgTmpDir cfg) pathsPath
      (ExprPath outPath) = Old.exprPathExplicit cfg True (typeOf e) "cut_list" [relPath]
  outPath %> \_ -> aLoadListLinks cfg outPath pathsPath
  return (ExprPath outPath)
rLoadListLinks _ _ = error "bad arguments to rLoadListLinks"

-- based on https://stackoverflow.com/a/18627837
-- uniqLines :: Ord a => [a] -> [a]
-- uniqLines = unlines . toList . fromList . lines

-- takes an action fn with any number of args and calls it with a tmpdir.
rSimpleTmp :: ActionFn -> String -> CutType -> RulesFn
rSimpleTmp actFn tmpPrefix _ s@(_,cfg) e@(CutFun _ _ _ _ exprs) = do
  argPaths <- mapM (rExpr s) exprs
  let tmpDir  = fromCutPath cfg $ cacheDir cfg tmpPrefix -- TODO tables bug here?
      outPath = fromCutPath cfg $ exprPath s e
  outPath %> \_ -> aSimpleTmp cfg outPath actFn tmpDir argPaths
  return (ExprPath outPath)
rSimpleTmp _ _ _ _ _ = error "bad argument to rSimpleTmp"

-------------
-- actions --
-------------

aVar :: CutConfig -> FilePath -> FilePath -> Action ()
aVar cfg dest link = do
  let destr  = ".." </> (makeRelative (cfgTmpDir cfg) dest)
      linkr  = ".." </> (makeRelative (cfgTmpDir cfg) link)
      link'  = debugAction cfg "aVar" link [link, dest]
  alwaysRerun
  need [dest]
  liftIO $ createDirectoryIfMissing True $ takeDirectory link
  wrappedCmd cfg [linkr] [] "ln" ["-fs", destr, link] -- TODO quietly?
  debugTrackWrite cfg [link']

-- TODO take the path, not the expression?
aLit :: CutConfig -> CutExpr -> FilePath -> Action ()
aLit cfg expr out = debugWriteFile cfg out' $ ePath ++ "\n"
  where
    paths :: CutExpr -> FilePath
    paths (CutLit _ _ p) = p
    paths _ = error "bad argument to paths"
    ePath = paths expr
    out' = debugAction cfg "aLit" out [ePath, out]

-- from ModuleAPI --

aOneArgScript :: CutConfig -> String
              -> FilePath -> FilePath -> FilePath -> Action ()
aOneArgScript cfg oPath script tmpDir argPath = do
  need [argPath]
  liftIO $ createDirectoryIfMissing True tmpDir
  quietly $ unit $ wrappedCmd cfg [oPath] [] script [tmpDir, oPath, argPath]
  let oPath' = debugAction cfg "aOneArgScript" oPath [oPath,script,tmpDir,argPath]
  trackWrite [oPath']

aOneArgListScript :: CutConfig -> FilePath
                  -> String -> FilePath -> FilePath -> Action ()
aOneArgListScript cfg outPath script tmpDir faPath = do
  need [faPath]
  liftIO $ createDirectoryIfMissing True tmpDir
  wrappedCmd cfg [outPath] [Cwd tmpDir] script [outPath, faPath]
  -- debugWriteFile cfg outPath out
  let out = debugAction cfg "aOneArgListScript" outPath [outPath, script, tmpDir, faPath]
  debugTrackWrite cfg [out]

aLoadListLits :: CutConfig -> FilePath -> FilePath -> Action ()
aLoadListLits cfg outPath litsPath = do
  lits  <- debugReadLines cfg litsPath -- TODO strip?
  lits' <- liftIO $ mapM absolutize lits -- TODO does this mess up non-paths?
  let out = debugAction cfg "aLoadListLits" outPath [outPath, litsPath]
  debugWriteLines cfg out lits'

aLoadListLinks :: CutConfig -> FilePath -> FilePath -> Action ()
aLoadListLinks cfg outPath pathsPath = do
    paths <- fmap (map (cfgTmpDir cfg </>)) (debugReadLines cfg pathsPath)
    need paths
    paths' <- liftIO $ mapM resolveSymlinks paths
    -- need paths'
    let out = debugAction cfg "aLoadListLinks" outPath [outPath, pathsPath]
    debugWriteLines cfg out paths'

aSimpleTmp :: CutConfig -> FilePath -> ActionFn -> FilePath -> [ExprPath] -> Action ()
aSimpleTmp cfg outPath actFn tmpDir argPaths = do
  let argPaths' = map (\(ExprPath p) -> p) argPaths
  need argPaths'
  liftIO $ createDirectoryIfMissing True tmpDir
  actFn cfg (CacheDir tmpDir) ([ExprPath outPath] ++ argPaths)
  let out = debugAction cfg "aSimpleTmp" outPath (outPath:tmpDir:argPaths') -- TODO actFn?
  trackWrite [out]
