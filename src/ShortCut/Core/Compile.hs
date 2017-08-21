
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
  , cList
  , addPrefixes
  , toShortCutList
  , toShortCutListStr
  , fromShortCutList
  )
  where

import Development.Shake
import ShortCut.Core.Debug
import ShortCut.Core.Types
import ShortCut.Core.Paths

import Data.List                  (find, sort)
import Data.Maybe                 (fromJust)
import Development.Shake.FilePath ((</>))
import System.FilePath            (makeRelative, takeDirectory)
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
mangleExpr fn (CutList t n vs   es   ) = CutList t n (map fn vs)   (map (mangleExpr fn) es)

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

-- TODO what happens to plain sets?
-- TODO WAIT ARE SETS REALLY NEEDED? OR CAN WE JUST REFER TO FILETYPES?
cExpr :: CutState -> CutExpr -> Rules ExprPath
cExpr s e@(CutLit  _ _ _      ) = cLit s e
cExpr s e@(CutRef  _ _ _ _    ) = cRef s e
cExpr s e@(CutList _ _ _ _    ) = cList s e
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
cAssign s (var, expr) = do
  path  <- cExpr s expr
  path' <- cVar s var expr path
  return (var, path')

-- TODO how to fail if the var doesn't exist??
--      (or, is that not possible for a typechecked AST?)
compileScript :: CutState -> Maybe String -> Rules ResPath
compileScript s@(as,_) permHash = do
  -- liftIO $ putStrLn "entering compileScript"
  -- TODO this can't be done all in parallel because they depend on each other,
  --      but can parts of it be parallelized? or maybe it doesn't matter because
  --      evaluating the code itself is always faster than the system commands
  rpaths <- mapM (cAssign s) as
  return $ (\(VarPath r) -> ResPath r) $ fromJust $ lookup (CutVar res) rpaths
  where
    -- p here is "result" + the permutation name/hash if there is one right?
    res = case permHash of
      Nothing -> "result"
      Just h  -> "result." ++ h

-- write a literal value from ShortCut source code to file
cLit :: CutState -> CutExpr -> Rules ExprPath
cLit (_,cfg) expr = do
  -- liftIO $ putStrLn "entering cLit"
  let (ExprPath path) = exprPath cfg expr []
  path %> \out -> debugWriteFile cfg out $ paths expr ++ "\n"
  return (ExprPath path)
  where
    paths :: CutExpr -> FilePath
    paths (CutLit _ _ p) = p
    paths _ = error "bad argument to paths"

-- TODO how to show the list once it's created? not just as a list of paths!
-- TODO why are lists of lists not given .list.list ext? hides a more serious bug?
--      or possibly the bug is that we're making accidental lists of lists?
cList :: CutState -> CutExpr -> Rules ExprPath
cList (_,cfg) e@(CutList EmptyList _ _ _) = do
  let (ExprPath link) = exprPath cfg e []
  link %> \out -> quietly $ wrappedCmd cfg [] "touch" [out]
  return (ExprPath link)
cList s@(_,cfg) e@(CutList _ _ _ exprs) = do
  paths <- mapM (cExpr s) exprs
  let (ExprPath path) = exprPath cfg e paths
      paths' = map (\(ExprPath p) -> makeRelative (cfgTmpDir cfg) p) paths
  path %> \out -> need (map (\(ExprPath p) -> p) paths) >> writeFileChanged out (unlines paths')
  return (ExprPath path)
cList _ _ = error "bad arguemnts to cList"

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
cRef :: CutState -> CutExpr -> Rules ExprPath
cRef (_,cfg) expr@(CutRef _ _ _ var) = return $ ePath $ varPath cfg var expr
  where
    ePath (VarPath p) = ExprPath p
cRef _ _ = error "bad argument to cRef"

-- Creates a symlink from varname to expression file.
-- TODO unify with cLink2, cLoadOne etc?
-- TODO do we need both the CutExpr and ExprPath? seems like CutExpr would do
cVar :: CutState -> CutVar -> CutExpr -> ExprPath -> Rules VarPath
cVar (_,cfg) var expr (ExprPath dest) = do
  let (VarPath link) = varPath cfg var expr
      dest' = ".." </> (makeRelative (cfgTmpDir cfg) dest)
  link %> \out -> do
    alwaysRerun
    need [dest]
    liftIO $ createDirectoryIfMissing True $ takeDirectory out
    quietly $ wrappedCmd cfg [] "ln" ["-fs", dest', out]
  return (VarPath link)

-- handles the actual rule generation for all binary operators
-- basically the `paths` functions with pattern matching factored out
cBop :: CutState -> CutType -> CutExpr -> (CutExpr, CutExpr)
      -> Rules (ExprPath, ExprPath, ExprPath)
cBop s@(_,cfg) t expr (n1, n2) = do
  p1 <- cExpr s n1
  p2 <- cExpr s n2
  return (p1, p2, exprPathExplicit cfg t expr "cut_bop" [p1, p2]) -- TODO name each one?

----------------------------------------------
-- adapters for scripts to read/write lists --
----------------------------------------------

-- gathers a list into one file so scripts don't have to worry about the
-- details of ShortCut's caching habits. note that that file might still be a
-- list of paths, if the original was a list of lists
-- TODO is there any good way to handle that?
-- TODO remove tmpDir
fromShortCutList :: CutConfig -> ExprPath -> ExprPath -> Action ()
fromShortCutList cfg (ExprPath inPath) (ExprPath outPath) = do
  litPaths <- debugReadLines cfg inPath
  let litPaths' = map (cfgTmpDir cfg </>) litPaths
  need litPaths'
  lits <- mapM (\p -> fmap init $ debugReadFile cfg p) litPaths'
  writeFileLines outPath lits

-- reverse of fromShortCutList. this is needed after calling a script that
-- writes a list of literals, because shortcut expects a list of hashed
-- filenames *pointing* to literals
-- TODO any reason to pass the full state instead of just config?
-- TODO do they not need to be lits?
toShortCutList :: CutConfig -> CutType -> ExprPath -> ExprPath -> Action ()
toShortCutList cfg litType (ExprPath inPath) (ExprPath outPath) = do
  lits <- fmap sort $ debugReadLines cfg inPath
  toShortCutListStr cfg litType (ExprPath outPath) lits

-- Like toShortCutList, but takes strings instead of a tmpfile containing strings.
toShortCutListStr :: CutConfig -> CutType -> ExprPath -> [String] -> Action ()
toShortCutListStr cfg litType (ExprPath outPath) lits = do
  let litExprs   = map (CutLit litType 0) lits
      litPaths   = map (\e -> exprPath cfg e []) litExprs
      litPaths'  = map (\(ExprPath p) -> makeRelative (cfgTmpDir cfg) p) litPaths
      litPaths'' = map (\(ExprPath p) -> p) litPaths
      litPairs   = zip lits litPaths''
  -- TODO nope have to separately use an action for each write if you want parallel
  --      (but would that even help much?)
  -- need litPaths''
  mapM_ (\(l,p) -> debugWriteFile cfg p $ l ++ "\n") litPairs
  debugWriteLines cfg outPath litPaths'
