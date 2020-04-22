{-|
This module "compiles" an expression it by translating it into a set of Shake
build rules. To actually run the rules, use 'OrthoLang.Interpreter.Eval.eval'.

TODO add more descriptive runtime error for canonicalizePath failing b/c no file

TODO see if you can avoid making more than one absolute symlink per input file

TODO make systematically sure there's only one rule for each file

TODO pass tmpDir as a config option somehow, and verbosity

TODO why doesn't turning down the verbosity actually work?
-}

module OrthoLang.Interpreter.Compile.Basic
  (

  -- * Expression compilers
    rExpr
  , rLit
  , rList
  , rListLits
  , rListPaths
  , rNamedFunction
  , rBop

  -- * Script compilers
  , rRef
  , rVar
  , rAssign
  , compileScript

  -- * Misc functions for export
  , debugC
  , debugRules
  -- , typeError

  -- * Implementation details
  , aLit
  , aListLits
  , aListPaths

  )
  where

-- TODO does turning of traces radically speed up the interpreter?

import Prelude hiding (error)

import OrthoLang.Debug
import Development.Shake
import OrthoLang.Types

import Data.List              (isPrefixOf)
import Data.Maybe             (fromJust)
import OrthoLang.Interpreter.Actions (traceA, debugA, need', readLit, writeLit, writeLits, writePaths, symlink)
import OrthoLang.Interpreter.Paths   (exprPath, toPath, fromPath, varPath, Path, prefixOf)
import OrthoLang.Util         (resolveSymlinks, stripWhiteSpace, removeIfExists)

-- import OrthoLang.Interpreter.Paths (insertNewRulesDigest)


debugC :: String -> String -> a -> a
debugC name msg rtn = trace ("core.compile." ++ name) msg rtn

-- TODO restrict to Expr?
-- TODO put in rExpr to catch everything at once? but misses which fn was called
debugRules :: (Pretty a, Show b) => String -> a -> b -> b
debugRules name input out = debugC name msg out
  where
    ren = render $ pPrint input
    msg = "\"" ++ ren ++ "' -> " ++ show out

------------------------------
-- compile the OrthoLang AST --
------------------------------

{-|
The main entry point for compiling any 'Expr'. Use this by default
unless you have a reason to delve into the more specific compilers below!

TODO remove the insert digests hack? or is this the main entry point for that too?

TODO are the extra rExpr steps needed in most cases, or only for rNamedFunction?
-}
rExpr :: RulesFn
rExpr s e@(Lit _ _      ) = rLit s e
rExpr s e@(Ref _ _ _ _    ) = rRef s e
rExpr s e@(Lst _ _   es) = mapM (rExpr s) es >> rList s e
rExpr s e@(Fun _ _ _ n es) = mapM (rExpr s) es >> rNamedFunction s e n -- TODO is the map part needed?
rExpr s e@(Bop t r ds _ e1 e2) = mapM (rExpr s) [e1, e2, Lst t ds [e1, e2]] >> rBop s e
rExpr _ (Com (CompiledExpr _ _ rules)) = rules

-- | Temporary hack to fix Bops
rBop :: RulesFn
rBop s e@(Bop t ms ds _ e1 e2) = rExpr s es >> rExpr s fn
  where
    es = Lst t ds [e1, e2] -- TODO (ListOf t)?
    fn = Fun t ms ds (prefixOf e) [es] -- TODO is the salt right?
rBop _ e = error "rBop" $ "called with non-Bop: \"" ++ render (pPrint e) ++ "\""

-- | This is in the process of being replaced with fNewRules,
--   so we ignore any function that already has that field written.
rNamedFunction :: Script -> Expr -> String -> Rules ExprPath
rNamedFunction s e@(Fun _ _ _ _ _) n = rNamedFunction' s e n -- TODO is this missing the map part above?
rNamedFunction _ _ n = error "rNamedFunction" $ "bad argument: " ++ n

rNamedFunction' :: Script -> Expr -> String -> Rules ExprPath
rNamedFunction' scr expr name = do
  cfg  <- fmap fromJust getShakeExtraRules
  mods <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let loc = "core.compile.basic.rNamedFunction'"
  case findFunction mods name of
    Nothing -> error "rNamedFunction" $ "no such function \"" ++ name ++ "\""
    Just f  -> case fNewRules f of
                 NewNotImplemented -> if "load_" `isPrefixOf` fName f
                                        then (fOldRules f) scr $ setSalt 0 expr
                                        else (fOldRules f) scr expr
                 -- note that the rules themselves should have been added by 'newRules'
                 NewRules _ -> let p   = fromPath loc cfg $ exprPath cfg dRef scr expr
                                   res = ExprPath p
                               in return $ debugRules "rNamedFunction'" expr res
                 -- TODO typecheck here to make sure the macro didn't mess anything up?
                 NewMacro _ -> fail $ "all macros should have been expanded already," ++
                                      " but rNamedFunction found " ++ name ++ " in " ++ show expr

rAssign :: Script -> Assign -> Rules (Var, VarPath)
rAssign scr (var, expr) = do
  (ExprPath path) <- rExpr scr expr
  cfg <- fmap fromJust getShakeExtraRules
  let loc = "core.compile.basic.rAssign"
  path' <- rVar var expr $ toPath loc cfg path
  let res  = (var, path')
      res' = debugRules "rAssign" (var, expr) res
  return res'

-- TODO how to fail if the var doesn't exist??
--      (or, is that not possible for a typechecked AST?)
-- TODO remove permHash
compileScript :: Script -> Rules ResPath
compileScript scr = do
  -- TODO this can't be done all in parallel because they depend on each other,
  --      but can parts of it be parallelized? or maybe it doesn't matter because
  --      evaluating the code itself is always faster than the system commands
  rpaths <- mapM (rAssign scr) scr -- TODO is having scr be both an issue? 
  res <- case lookupResult rpaths of
    Nothing -> fmap (\(ExprPath p) -> p) $ rExpr scr $ fromJust $ lookupResult $ ensureResult scr
    Just r  -> fmap (\(VarPath  p) -> p) $ return r
  return $ ResPath res
  -- where
    -- p here is "result" + the permutation name/hash if there is one right?
    -- res = case permHash of
      -- Nothing -> "result"
      -- Just h  -> "result." ++ h

-- | Write a literal value (a 'str' or 'num') from OrthoLang source code to file
rLit :: RulesFn
rLit scr expr = do
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let loc = "core.compile.basic.rLit"
  let path  = exprPath cfg dRef scr expr -- absolute paths allowed!
      path' = debugRules "rLit" expr $ fromPath loc cfg path
  path' %> \_ -> aLit expr path
  return (ExprPath path')

-- TODO take the path, not the expression?
-- TODO these actions all need to decode their dependencies from the outpath rather than the expression
aLit :: Expr -> Path -> Action ()
aLit expr out = do
  cfg <- fmap fromJust getShakeExtra
  let loc   = "core.compile.basic.aLit"
      paths :: Expr -> FilePath
      paths (Lit _ p) = p
      paths _ = fail "bad argument to paths"
      ePath = paths expr
      out'  = fromPath loc cfg out
      out'' = traceA loc out' [ePath, out']
  writeLit loc out'' ePath -- TODO too much dedup?

{-|
Lists written explicitly in the source code (not generated by function calls).
These use the 'Lst' constructor; generated lists come as 'Fun's
whose type is a @'ListOf' \<something\>@ instead.

TODO remove the insert digests hack
-}
rList :: RulesFn
rList s e@(Lst rtn _ _)
  | rtn `elem` [Empty, str, num] = rListLits  s e
  | otherwise                    = rListPaths s e
rList _ _ = error "rList" "bad arguemnt"

{-|
Special case for writing lists of literals ('str's or 'num's) in the source code.
For example:

@
nums = [1,2,3]
strs = ["one", "two", "three", "four"]
@

These have a variable number of arguments which is known at Rules-time. So we
fold over their digests to produce one main \"argument\" digest. All their
actual arguments are added to their hExprs entry at the same time.

Note this is different from how function-generated lists of literals (or paths)
are handled, because their arguments won't be known until after the function
runs.

It's also different from how source code lists of non-literals are handled, in
order to match the format of function-generated lists of literals. It turns out
to be much more efficient for those to write one big multiline file than
thousands of small literals + one list of links pointing to them.

TODO can it be mostly unified with rListPaths digest-wise?

TODO what happens when you make a list of literals in two steps using links?
-}
rListLits :: RulesFn
rListLits scr e@(Lst _ _ exprs) = do
  litPaths <- mapM (rExpr scr) exprs
  cfg <- fmap fromJust getShakeExtraRules
  let loc = "core.compile.basic.rListLits"
      litPaths' = map (\(ExprPath p) -> toPath loc cfg p) litPaths
  dRef <- fmap fromJust getShakeExtraRules
  let outPath  = exprPath cfg dRef scr e
      outPath' = debugRules loc e $ fromPath loc cfg outPath
  outPath' %> \_ -> aListLits litPaths' outPath
  return (ExprPath outPath')
rListLits _ e = error "rListLits" $ "bad argument: " ++ show e

-- TODO put this in a cache dir by content hash and link there
aListLits :: [Path] -> Path -> Action ()
aListLits paths outPath = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "core.compile.basic.aListLits"
      out'   = fromPath loc cfg outPath
      out''  = traceA "aListLits" out' (out':paths')
      paths' = map (fromPath loc cfg) paths
  need' loc paths' -- TODO remove?
  lits <- mapM (readLit loc) paths'
  let lits' = map stripWhiteSpace lits -- TODO insert <<emptylist>> here?
  debugA loc $ "lits': " ++ show lits'
  writeLits loc out'' lits'

{-|
Regular case for writing a list of links to some other file type
(not literal 'str's or 'num's) in the source code. For example:

@
chlamy    = load_faa ...
athaliana = load_faa ...
pcc7942   = load_faa ...
greens = [chlamy, athaliana, pcc7942]
@

Like lists of literals, these also have a variable number of arguments which is
known at Rules-time. So we fold over their digests to produce one main
\"argument\" digest. All their actual arguments are added to their hExprs
entry at the same time. Note this is different from how function-generated
lists of paths (or literals) are handled, because their arguments won't be
known until after the function runs.

TODO hash mismatch error here?
-}
rListPaths :: RulesFn
rListPaths scr e@(Lst _ _ exprs) = do
  paths <- mapM (rExpr scr) exprs
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let loc = "core.compile.basic.rListPaths"
      paths'   = map (\(ExprPath p) -> toPath loc cfg p) paths
      -- hash     = digest $ concat $ map digest paths'
      -- outPath  = unsafeExprPathExplicit cfg "list" (ListOf rtn) salt [hash]
      outPath  = exprPath cfg dRef scr e
      outPath' = debugRules loc e $ fromPath loc cfg outPath
  outPath' %> \_ -> aListPaths paths' outPath
  return (ExprPath outPath')
rListPaths _ _ = error "rListPaths" "bad argument"

aListPaths :: [Path] -> Path -> Action ()
aListPaths paths outPath = do
  cfg <- fmap fromJust getShakeExtra
  let out'   = fromPath loc cfg outPath
      loc    = "core.compile.basic.aListPaths"
      out''  = traceA "aListPaths" out' (out':paths')
      paths' = map (fromPath loc cfg) paths -- TODO remove this
  need' loc paths'
  paths'' <- liftIO $ mapM (resolveSymlinks $ Just $ tmpdir cfg) paths'
  need' loc paths''
  let paths''' = map (toPath loc cfg) paths'' -- TODO not working?
  writePaths loc out'' paths'''

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
rRef :: RulesFn
rRef _ e@(Ref _ _ _ var) = do
  cfg <- fmap fromJust getShakeExtraRules
  let loc = "core.compile.basic.rRef"
      ePath p = ExprPath $ debugRules loc e $ fromPath loc cfg p
  return $ ePath $ varPath cfg var e
    
rRef _ _ = fail "bad argument to rRef"

-- Creates a symlink from varname to expression file.
-- TODO unify with rLink2, rLoad etc?
-- TODO do we need both the Expr and ExprPath? seems like Expr would do
rVar :: Var -> Expr -> Path -> Rules VarPath
rVar var expr oPath = do
  cfg <- fmap fromJust getShakeExtraRules
  let vPath  = varPath cfg var expr
      loc = "core.compile.basic.rVar"
      vPath' = debugRules loc var $ fromPath loc cfg vPath
  vPath' %> \_ -> aVar vPath oPath
  return (VarPath vPath')

aVar :: Path -> Path -> Action ()
aVar vPath oPath = do
  alwaysRerun
  cfg <- fmap fromJust getShakeExtra
  let loc = "core.compile.basic.aVar"
      oPath'  = fromPath loc cfg oPath
      vPath'  = fromPath loc cfg vPath
      vPath'' = traceA "aVar" vPath [vPath', oPath']
  need' loc [oPath']
  ref <- fmap fromJust getShakeExtra
  liftIO $ removeIfExists ref vPath'
  -- TODO should it sometimes symlink and sometimes copy?
  -- TODO before copying, think about how you might have to re-hash again!
  symlink vPath'' oPath
  -- ids' <- liftIO $ readIORef ids
  -- unhashIDsFile ' oPath vPath''
