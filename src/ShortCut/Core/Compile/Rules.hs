-- Once text has been parsed into an abstract syntax tree (Parse.hs), this
-- module "compiles" it by translating it into a set of Shake build rules. To
-- actually run the rules, use `eval` in the Interpret module.

-- TODO add more descriptive runtime error for canonicalizePath failing b/c no file
-- TODO see if you can avoid making more than one absolute symlink per input file
-- TODO make systematically sure there's only one rule for each file
-- TODO pass tmpDir as a config option somehow, and verbosity

-- TODO why doesn't turning down the verbosity actually work?

module ShortCut.Core.Compile.Rules
--   ( compileScript
--   , rBop
--   , rExpr
--   , rSet
--   , addPrefixes
--   )
  where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Paths
import ShortCut.Core.Compile.Actions

import ShortCut.Core.Debug        (debugCompiler)
import Data.List                  (find)
import Data.Maybe                 (fromJust)
import System.FilePath            (makeRelative)

-- from ModuleAPI --
-- import Data.Set                   (fromList, toList)
import Development.Shake.FilePath ((</>), (<.>))
import ShortCut.Core.Paths        (cacheDir, cacheDirUniq, exprPath, exprPathExplicit)
-- import ShortCut.Core.Compile.Rules      (rExpr)
import ShortCut.Core.Debug        (debugTrackWrite, debugWriteLines, debugReadLines)
import System.Directory           (createDirectoryIfMissing)
import ShortCut.Core.Config       (wrappedCmd)
import ShortCut.Core.Util         (absolutize, resolveSymlinks)
import Text.PrettyPrint.HughesPJClass

--------------------------------------------------------
-- prefix variable names so duplicates don't conflict --
--------------------------------------------------------

-- TODO only mangle the specific vars we want changed!

mangleExpr :: (CutVar -> CutVar) -> CutExpr -> CutExpr
mangleExpr _ e@(CutLit  _ _ _) = e
mangleExpr fn (CutRef  t n vs v      ) = CutRef  t n (map fn vs)   (fn v)
mangleExpr fn (CutBop  t n vs s e1 e2) = CutBop  t n (map fn vs) s (mangleExpr fn e1) (mangleExpr fn e2)
mangleExpr fn (CutFun  t n vs s es   ) = CutFun  t n (map fn vs) s (map (mangleExpr fn) es)
mangleExpr fn (CutSet t n vs   es   ) = CutSet t n (map fn vs)   (map (mangleExpr fn) es)

mangleAssign :: (CutVar -> CutVar) -> CutAssign -> CutAssign
mangleAssign fn (var, expr) = (fn var, mangleExpr fn expr)

mangleScript :: (CutVar -> CutVar) -> CutScript -> CutScript
mangleScript fn = map (mangleAssign fn)

-- TODO pad with zeros?
-- Add a "dupN." prefix to each variable name in the path from independent
-- -> dependent variable, using a list of those varnames
addPrefix :: String -> (CutVar -> CutVar)
addPrefix p (CutVar s) = CutVar $ s ++ "." ++ p

-- TODO should be able to just apply this to a duplicate script section right?
addPrefixes :: String -> CutScript -> CutScript
addPrefixes p = mangleScript (addPrefix p)


------------------------------
-- compile the ShortCut AST --
------------------------------

rExpr :: CutState -> CutExpr -> Rules ExprPath
rExpr s e@(CutLit _ _ _      ) = rLit s e
rExpr s e@(CutRef _ _ _ _    ) = rRef s e
rExpr s e@(CutSet _ _ _ _    ) = rSet s e
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
      res' = debugCompiler cfg "rAssign" (var, expr) res
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
rLit (_,cfg) expr = do
  let (ExprPath path) = exprPath cfg False expr [] -- absolute paths allowed!
      path' = debugCompiler cfg "rLit" expr path
  path %> aLit cfg expr
  return (ExprPath path')

rSet :: CutState -> CutExpr -> Rules ExprPath
rSet s e@(CutSet EmptySet _ _ _) = rSetEmpty s e
rSet s e@(CutSet rtn _ _ _)
  | rtn `elem` [str, num] = rSetLits s e
  | otherwise = rSetPaths s e
rSet _ _ = error "bad arguemnt to rSet"

-- special case for empty lists
-- TODO is a special type for this really needed?
rSetEmpty :: (CutScript, CutConfig) -> CutExpr -> Rules ExprPath
rSetEmpty (_,cfg) e@(CutSet EmptySet _ _ _) = do
  let (ExprPath link) = exprPath cfg True e []
      link' = debugCompiler cfg "rSetEmpty" e link
  link %> \_ -> aSetEmpty cfg link
  return (ExprPath link')
rSetEmpty _ e = error $ "bad arguemnt to rSetEmpty: " ++ show e

-- special case for writing lists of strings or numbers as a single file
rSetLits :: (CutScript, CutConfig) -> CutExpr -> Rules ExprPath
rSetLits s@(_,cfg) e@(CutSet rtn _ _ exprs) = do
  litPaths <- mapM (rExpr s) exprs
  let litPaths' = map (\(ExprPath p) -> p) litPaths
      relPaths  = map (makeRelative $ cfgTmpDir cfg) litPaths'
      (ExprPath outPath) = exprPathExplicit cfg True (SetOf rtn) "cut_set" relPaths
      outPath' = debugCompiler cfg "rSetLits" e outPath
  outPath %> \_ -> aSetLits cfg outPath relPaths
  return (ExprPath outPath')
rSetLits _ e = error $ "bad argument to rSetLits: " ++ show e

-- regular case for writing a list of links to some other file type
rSetPaths :: (CutScript, CutConfig) -> CutExpr -> Rules ExprPath
rSetPaths s@(_,cfg) e@(CutSet rtn _ _ exprs) = do
  paths <- mapM (rExpr s) exprs
  let paths'   = map (\(ExprPath p) -> p) paths
      relPaths = map (makeRelative $ cfgTmpDir cfg) paths'
      (ExprPath outPath) = exprPathExplicit cfg True (SetOf rtn) "cut_set" relPaths
      outPath' = debugCompiler cfg "rSetPaths" e outPath
  outPath %> \_ -> aSetPaths cfg outPath paths'
  return (ExprPath outPath')
rSetPaths _ _ = error "bad arguemnts to rSetPaths"

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
rRef :: CutState -> CutExpr -> Rules ExprPath
rRef (_,cfg) e@(CutRef _ _ _ var) = return $ ePath $ varPath cfg var e
  where
    ePath (VarPath p) = ExprPath $ debugCompiler cfg "rRef" e p
rRef _ _ = error "bad argument to rRef"

-- Creates a symlink from varname to expression file.
-- TODO unify with rLink2, rLoadOne etc?
-- TODO do we need both the CutExpr and ExprPath? seems like CutExpr would do
rVar :: CutState -> CutVar -> CutExpr -> ExprPath -> Rules VarPath
rVar (_,cfg) var expr (ExprPath dest) = do
  let (VarPath link) = varPath cfg var expr
      -- TODO is this needed? maybe just have links be absolute?
      linkd = debugCompiler cfg "rVar" var link
  link %> \_ -> aVar cfg dest link
  return (VarPath linkd)

-- Handles the actual rule generation for all binary operators;
-- basically the `paths` functions with pattern matching factored out.
-- Some of the complication is just making sure paths don't depend on tmpdir,
-- and some is that I wrote this near the beginning, when I didn't have
-- many of the patterns worked out yet. Feel free to update...
rBop :: CutState -> CutType -> CutExpr -> (CutExpr, CutExpr)
      -> Rules (ExprPath, ExprPath, ExprPath)
rBop s@(_,cfg) t e@(CutBop _ salt _ name _ _) (n1, n2) = do
  (ExprPath p1) <- rExpr s n1
  (ExprPath p2) <- rExpr s n2
  let rel1  = makeRelative (cfgTmpDir cfg) p1
      rel2  = makeRelative (cfgTmpDir cfg) p2
      path  = exprPathExplicit cfg True t "cut_bop" [show salt, name, rel1, rel2]
      path' = debugCompiler cfg "rBop" e path
  return (ExprPath p1, ExprPath p2, path')
rBop _ _ _ _ = error "bad argument to rBop"

-- from ModuleAPI --

------------------------------
-- [t]ypechecking functions --
------------------------------

typeError :: [CutType] -> [CutType] -> String
typeError expected actual =
  "Type error:\nexpected " ++ show expected
           ++ "\nbut got " ++ show actual

-- this mostly checks equality, but also has to deal with how an empty set can
-- be any kind of set
-- TODO is there any more elegant way? this seems error-prone...
typeMatches :: CutType -> CutType -> Bool
typeMatches EmptySet  (SetOf _) = True
typeMatches (SetOf _) EmptySet  = True
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
      (ExprPath oPath) = exprPath cfg True expr []
  oPath %> \_ -> aOneArgScript cfg oPath script tmpDir argPath
  return (ExprPath oPath)
rOneArgScript _ _ _ _ = error "bad argument to rOneArgScript"

-- for scripts that take one arg and return a list of lits
-- TODO this should put tmpfiles in cache/<script name>!
-- TODO name something more explicitly about fasta files?
rOneArgListScript :: FilePath -> FilePath -> CutState -> CutExpr -> Rules ExprPath
rOneArgListScript tmpName script s@(_,cfg) expr@(CutFun _ _ _ _ [fa]) = do
  (ExprPath faPath) <- rExpr s fa
  let (CacheDir tmpDir ) = cacheDir cfg tmpName
      (ExprPath outPath) = exprPath cfg True expr []
  outPath %> \_ -> aOneArgListScript cfg outPath script tmpDir faPath
  return (ExprPath outPath)
rOneArgListScript _ _ _ _ = error "bad argument to rOneArgListScript"

-- The paths here are a little confusing: expr is a str of the path we want to
-- link to. So after compiling it we get a path to *that str*, and have to read
-- the file to access it. Then we want to `ln` to the file it points to.
-- TODO should this go in Compile.hs?
rLink :: CutState -> CutExpr -> CutType -> String -> Rules ExprPath
rLink s@(_,cfg) expr rtype prefix = do
  (ExprPath strPath) <- rExpr s expr -- TODO is this the issue?
  -- TODO only depend on final expressions
  -- ok without ["outPath"]?
  let (ExprPath outPath) = exprPathExplicit cfg True rtype prefix [show expr]
  outPath %> \_ -> aLink cfg outPath strPath
  return (ExprPath outPath)

-- TODO remove this?
rLoadOne :: CutType -> RulesFn
rLoadOne t s (CutFun _ _ _ n [p]) = rLink s p t n
rLoadOne _ _ _ = error "bad argument to rLoadOne"

rLoadList :: RulesFn
rLoadList s e@(CutFun (SetOf rtn) _ _ _ [es])
  | typeOf es `elem` [SetOf str, SetOf num] = rLoadListOne rtn s es
  | otherwise = rLoadListMany s e
rLoadList _ _ = error "bad arguments to rLoadList"

-- special case for lists of str and num
-- TODO remove rtn and use (typeOf expr)?
-- TODO is this different from rSetOne, except in its return type?
-- TODO is it different from rLink? seems like it's just a copy/link operation...
rLoadListOne :: CutType -> RulesFn
rLoadListOne rtn s@(_,cfg) expr = do
  (ExprPath litsPath) <- rExpr s expr
  let relPath = makeRelative (cfgTmpDir cfg) litsPath
      (ExprPath outPath) = exprPathExplicit cfg True (SetOf rtn) "cut_set" [relPath]
  outPath %> \_ -> aLoadListOne cfg outPath litsPath
  return (ExprPath outPath)

-- regular case for lists of any other file type
rLoadListMany :: RulesFn
rLoadListMany s@(_,cfg) e@(CutFun _ _ _ _ [es]) = do
  (ExprPath pathsPath) <- rExpr s es
  -- TODO is relPath enough to make sure it's unique??
  let relPath = makeRelative (cfgTmpDir cfg) pathsPath
      (ExprPath outPath) = exprPathExplicit cfg True (typeOf e) "cut_set" [relPath]
  outPath %> \_ -> aLoadListMany cfg outPath pathsPath
  return (ExprPath outPath)
rLoadListMany _ _ = error "bad arguments to rLoadListMany"

-- based on https://stackoverflow.com/a/18627837
-- uniqLines :: Ord a => [a] -> [a]
-- uniqLines = unlines . toList . fromList . lines

-- takes an action fn with any number of args and calls it with a tmpdir.
rSimpleTmp :: ActionFn -> String -> CutType -> RulesFn
rSimpleTmp actFn tmpPrefix _ s@(_,cfg) e@(CutFun _ _ _ _ exprs) = do
  argPaths <- mapM (rExpr s) exprs
  let (ExprPath outPath) = exprPath cfg True e []
      (CacheDir tmpDir ) = cacheDir cfg tmpPrefix -- TODO tables bug here?
  outPath %> \_ -> aSimpleTmp cfg outPath actFn tmpDir argPaths
  return (ExprPath outPath)
rSimpleTmp _ _ _ _ _ = error "bad argument to rSimpleTmp"

rMapLastTmp :: ActionFn -> String -> CutType -> RulesFn
rMapLastTmp actFn tmpPrefix t s@(_,cfg) = mapFn t s
  where
    tmpDir = cacheDir cfg tmpPrefix
    mapFn  = rMapLast (const tmpDir) actFn tmpPrefix

-- TODO use a hash for the cached path rather than the name, which changes!

-- takes an action fn and vectorizes the last arg (calls the fn with each of a
-- list of last args). returns a list of results. uses a new tmpDir each call.
rMapLastTmps :: ActionFn -> String -> CutType -> RulesFn
rMapLastTmps fn tmpPrefix t s@(_,cfg) e = rMapLast tmpFn fn tmpPrefix t s e
  where
    -- TODO what if the same last arg is used in different mapping fns?
    --      will it be unique?
    tmpFn args = cacheDirUniq cfg tmpPrefix args

-- common code factored out from the two functions above
-- TODO put the .args and final functions in the cachedir of the regular fn?
-- TODO now that the new Shake strategy works, clean it up!
-- TODO sprinkle some need in here?
rMapLast :: ([FilePath] -> CacheDir) -> ActionFn -> String -> CutType -> RulesFn
rMapLast tmpFn actFn _ rtnType s@(_,cfg) e@(CutFun _ _ _ name exprs) = do
  liftIO $ putStrLn $ "rMapLast expr: " ++ render (pPrint e)

  initPaths <- mapM (rExpr s) (init exprs)
  (ExprPath lastsPath) <- rExpr s (last exprs)
  let inits = map (\(ExprPath p) -> p) initPaths
      (ExprPath outPath) = exprPathExplicit cfg True (SetOf rtnType) name [show e]
      (CacheDir mapTmp) = cacheDirUniq cfg "map_last" e

  outPath %> \_ -> aMapLastArgs cfg outPath inits mapTmp lastsPath

  -- This builds one of the list of out paths based on a .args file
  -- (made in the action above). It's a pretty roundabout way to do it!
  -- TODO ask ndmitchell if there's something much more elegant I'm missing
  (mapTmp </> "*") %> aMapLastMapTmp cfg tmpFn actFn

  return (ExprPath outPath)
rMapLast _ _ _ _ _ _ = error "bad argument to rMapLastTmps"
