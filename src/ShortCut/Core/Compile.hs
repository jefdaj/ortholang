-- Once text has been parsed into an abstract syntax tree (Parse.hs), this
-- module "compiles" it by translating it into a set of Shake build rules. To
-- actually run the rules, use `eval` in the Interpret module.

-- TODO add more descriptive runtime error for canonicalizePath failing b/c no file
-- TODO see if you can avoid making more than one absolute symlink per input file
-- TODO make systematically sure there's only one rule for each file
-- TODO pass tmpDir as a config option somehow, and verbosity

-- TODO why doesn't turning down the verbosity actually work?

module ShortCut.Core.Compile
  ( compileScript
  , cBop
  , cExpr
  , cSet
  , addPrefixes
  )
  where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Paths

import ShortCut.Core.Debug        (debugCompiler, debugReadFile,
                                   debugWriteFile, debugWriteLines)
import ShortCut.Core.Util         (resolveSymlinks, stripWhiteSpace)
import Data.List                  (find, sort)
import Data.Maybe                 (fromJust)
import Development.Shake.FilePath ((</>))
import System.FilePath            (makeRelative, takeDirectory, takeFileName)
import System.Directory           (createDirectoryIfMissing)
import ShortCut.Core.Config       (wrappedCmd)

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

cExpr :: CutState -> CutExpr -> Rules ExprPath
cExpr s e@(CutLit  _ _ _      ) = cLit s e
cExpr s e@(CutRef  _ _ _ _    ) = cRef s e
cExpr s e@(CutSet _ _ _ _    ) = cSet s e
cExpr s e@(CutBop  _ _ _ n _ _) = compileByName s e n -- TODO turn into Fun?
cExpr s e@(CutFun  _ _ _ n _  ) = compileByName s e n

-- TODO remove once no longer needed (parser should find fns)
compileByName :: CutState -> CutExpr -> String -> Rules ExprPath
compileByName s@(_,cfg) expr name = case findByName cfg name of
  Nothing -> error $ "no such function '" ++ name ++ "'"
  Just f  -> (fCompiler f) s expr

-- TODO remove once no longer needed (parser should find fns)
findByName :: CutConfig -> String -> Maybe CutFunction
findByName cfg name = find (\f -> fName f == name) fs
  where
    ms = cfgModules cfg
    fs = concatMap mFunctions ms

cAssign :: CutState -> CutAssign -> Rules (CutVar, VarPath)
cAssign s@(_,cfg) (var, expr) = do
  path  <- cExpr s expr
  path' <- cVar s var expr path
  let res  = (var, path')
      res' = debugCompiler cfg "cAssign" (var, expr) res
  return res'

-- TODO how to fail if the var doesn't exist??
--      (or, is that not possible for a typechecked AST?)
compileScript :: CutState -> Maybe String -> Rules ResPath
compileScript s@(as,cfg) permHash = do
  -- TODO this can't be done all in parallel because they depend on each other,
  --      but can parts of it be parallelized? or maybe it doesn't matter because
  --      evaluating the code itself is always faster than the system commands
  rpaths <- mapM (cAssign s) as
  let (VarPath r) = fromJust $ lookup (CutVar res) rpaths
  -- return $ ResPath $ makeRelative (cfgTmpDir cfg) r
  return $ ResPath r
  where
    -- p here is "result" + the permutation name/hash if there is one right?
    res = case permHash of
      Nothing -> "result"
      Just h  -> "result." ++ h

-- write a literal value from ShortCut source code to file
cLit :: CutState -> CutExpr -> Rules ExprPath
cLit (_,cfg) expr = do
  let (ExprPath path) = exprPath cfg False expr [] -- absolute paths allowed!
      path' = debugCompiler cfg "cLit" expr path
  path %> \out -> debugWriteFile cfg out $ paths expr ++ "\n"
  return (ExprPath path')
  where
    paths :: CutExpr -> FilePath
    paths (CutLit _ _ p) = p
    paths _ = error "bad argument to paths"

cSet :: CutState -> CutExpr -> Rules ExprPath
cSet s e@(CutSet EmptySet _ _ _) = cSetEmpty s e
cSet s e@(CutSet rtn _ _ _)
  | rtn `elem` [str, num] = cSetLits s e
  | otherwise = cSetPaths s e
cSet _ _ = error "bad arguemnt to cSet"

-- special case for empty lists
-- TODO is a special type for this really needed?
cSetEmpty :: (CutScript, CutConfig) -> CutExpr -> Rules ExprPath
cSetEmpty (_,cfg) e@(CutSet EmptySet _ _ _) = do
  let (ExprPath link) = exprPath cfg True e []
      link' = debugCompiler cfg "cSetEmpty" e link
  link %> \_ -> wrappedCmd cfg [link] [] "touch" [link] -- TODO quietly?
  return (ExprPath link')
cSetEmpty _ e = error $ "bad arguemnt to cSetEmpty: " ++ show e

-- special case for writing lists of strings or numbers as a single file
cSetLits :: (CutScript, CutConfig) -> CutExpr -> Rules ExprPath
cSetLits s@(_,cfg) e@(CutSet rtn _ _ exprs) = do
  litPaths <- mapM (cExpr s) exprs
  let litPaths' = map (\(ExprPath p) -> p) litPaths
      relPaths  = map (makeRelative $ cfgTmpDir cfg) litPaths'
      (ExprPath outPath) = exprPathExplicit cfg True (SetOf rtn) "cut_set" relPaths
      outPath' = debugCompiler cfg "cSetLits" e outPath
  outPath %> \_ -> do
    lits  <- mapM (\p -> debugReadFile cfg $ cfgTmpDir cfg </> p) relPaths
    let lits' = sort $ map stripWhiteSpace lits
    debugWriteLines cfg outPath lits'
  return (ExprPath outPath')
cSetLits _ e = error $ "bad argument to cSetLits: " ++ show e

-- regular case for writing a list of links to some other file type
cSetPaths :: (CutScript, CutConfig) -> CutExpr -> Rules ExprPath
cSetPaths s@(_,cfg) e@(CutSet rtn _ _ exprs) = do
  paths <- mapM (cExpr s) exprs
  let paths'   = map (\(ExprPath p) -> p) paths
      relPaths = map (makeRelative $ cfgTmpDir cfg) paths'
      (ExprPath outPath) = exprPathExplicit cfg True (SetOf rtn) "cut_set" relPaths
      outPath' = debugCompiler cfg "cSetPaths" e outPath
  outPath %> \_ -> do
    need paths'
    -- TODO yup bug was here! any reason to keep it?
    -- paths'' <- liftIO $ mapM resolveSymlinks paths'
    debugWriteLines cfg outPath paths'
  return (ExprPath outPath')
cSetPaths _ _ = error "bad arguemnts to cSetPaths"

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
cRef :: CutState -> CutExpr -> Rules ExprPath
cRef (_,cfg) e@(CutRef _ _ _ var) = return $ ePath $ varPath cfg var e
  where
    ePath (VarPath p) = ExprPath $ debugCompiler cfg "cRef" e p
cRef _ _ = error "bad argument to cRef"

-- Creates a symlink from varname to expression file.
-- TODO unify with cLink2, cLoadOne etc?
-- TODO do we need both the CutExpr and ExprPath? seems like CutExpr would do
cVar :: CutState -> CutVar -> CutExpr -> ExprPath -> Rules VarPath
cVar (_,cfg) var expr (ExprPath dest) = do
  let (VarPath link) = varPath cfg var expr
      -- TODO is this needed? maybe just have links be absolute?
      destr  = ".." </> (makeRelative (cfgTmpDir cfg) dest)
      linkr  = ".." </> (makeRelative (cfgTmpDir cfg) link)
      linkd = debugCompiler cfg "cVar" var link
  link %> \_ -> do
    alwaysRerun
    need [dest]
    liftIO $ createDirectoryIfMissing True $ takeDirectory link
    wrappedCmd cfg [linkr] [] "ln" ["-fs", destr, link] -- TODO quietly?
  return (VarPath linkd)

-- Handles the actual rule generation for all binary operators;
-- basically the `paths` functions with pattern matching factored out.
-- Some of the complication is just making sure paths don't depend on tmpdir,
-- and some is that I wrote this near the beginning, when I didn't have
-- many of the patterns worked out yet. Feel free to update...
cBop :: CutState -> CutType -> CutExpr -> (CutExpr, CutExpr)
      -> Rules (ExprPath, ExprPath, ExprPath)
cBop s@(_,cfg) t e@(CutBop _ salt _ name _ _) (n1, n2) = do
  (ExprPath p1) <- cExpr s n1
  (ExprPath p2) <- cExpr s n2
  let rel1  = makeRelative (cfgTmpDir cfg) p1
      rel2  = makeRelative (cfgTmpDir cfg) p2
      path  = exprPathExplicit cfg True t "cut_bop" [show salt, name, rel1, rel2]
      path' = debugCompiler cfg "cBop" e path
  return (ExprPath p1, ExprPath p2, path')
cBop _ _ _ _ = error "bad argument to cBop"
