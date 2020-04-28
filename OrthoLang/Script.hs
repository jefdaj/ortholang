{-|
This module gathers together all the semi-complicated 'Script' transformations
that were littering the rest of the codebase.

TODO use the standard module hierarchy? Data.OrthoLang.Script sounds good

TODO result handling:
  remove "result" var if any when including a script
  split Parse.Basic.putAssign/assign into File and Repl versions with different result handling + errors
  rename Parse.Script.pResult -> pNaked? seems like it would only be used in the Repl version
  add repl test cases that include test scripts and check the result handling
-}

-- TODO still confusing, so split the appendStatement fns into multiple non-confusing smaller ones!

module OrthoLang.Script
  (

  -- * Used in Interpreter.Parse
    -- appendStatement -- TODO remove
    appendStatementFile
  , appendStatementRepl -- TODO will this be used in the Repl directly instead?

  -- * Used in Interpreter.Eval (between parse and compile steps)
  , expandMacros
  -- , eScript
  -- , eAssign
  -- , eExpr

  -- * Used in Modules.Replace
  , extractExprs
  , setRepIDs
  -- , mapExprVars
  -- , mapAssignVars
  -- , mapScriptVars
  -- , setRepID

  -- * Used in Interpreter.Repl
  , rDepsOf
  , depsOnly
  -- , appendOrUpdateAssign
  -- , replaceVar
  -- , removeAssignSelfReferences
  -- , rmRef

  )
  where

import Prelude hiding (error)
import OrthoLang.Debug (trace, error)

import OrthoLang.Types
import Data.List       (filter, delete)
import OrthoLang.Util  (justOrDie)


-----------
-- parse --
-----------

-- Ref Type (Maybe Salt) [Var] Var -- do refs need a salt? yes! (i think?)
-- TODO salt is Nothing rather than the expr's salt, right?
-- TODO var is added to deps, right?
-- appendStatement :: Script -> Assign -> Script
-- appendStatement scr a =

  -- let rv  = Var (RepID Nothing) "result"
      -- rr  = Ref (typeOf expr) Nothing (depsOf expr) var 
      -- ra  = Assign rv rr
  -- let as' = delVar (sAssigns scr) vName ++ [a]
  --     r'  = if null (sResult scr) || vName == "result"
  --             then Just expr 
  --             else sResult scr
  --     scr' = Script {sAssigns = as', sResult = r'}

  -- OK, so by example the behavior we actually wanted instead of this:
  --
  -- result = load_fna "https://molb7621.github.io/workshop/_downloads/sample.fa"
  -- samplefa = result
  --
  -- was:
  -- 1) substitute the result ref for its value in the script
  -- 2) strip the "result" assignment from the script
  -- 3) add the new assignment statement samplefa = load_fna ...
  -- 4) assigned a new default result = samplefa
  --
  -- How do we know that?
  --
  -- we should always do (1)
  -- if interactive we should also always do (2), but respect the earlier "result = " in written scripts?
  -- always do (3)
  -- and always do (4) unless the current assignment was explicitly "result = " already

  -- the next bug:
  --
  -- ortholang —▶ 1 # comments after naked expressions should be ignored
  -- 1
  -- 
  -- ortholang —▶ :show
  -- result = 1
  -- 
  -- ortholang —▶ test = 1 # same with comments after assignment statements
  -- ortholang —▶ :show
  -- result = 1
  -- test = 1
  -- 
  -- ## only problem is it should remove "result = 1" and insert "result = test" at the end
  -- TODO try stripping old result and see if that fixes it
  -- TODO if not, try also inserting the new one at the end

  -- Seems like the issue here is implicit vs explicit result? When assigned
  -- explicitly in a script, result should persist after more assignments. But
  -- when only assigned implicitly it should be overwritten, and when in the
  -- REPL it should always be overwritten. When saving a specific REPL var it
  -- should be set added to the end of the script file.
  --
  -- The simplest way to capture this in types, though maybe not the cleanest,
  -- seems to be to prioritize a "result" var in sAssigns over sResult, and
  -- have sResult be optional. Then parsing behavior should vary between file and repl:
  -- in repl, assign each naked expression to sResult. but in a file, naked expressions should be errors
  -- in a file, never remove previous result vars, except during includes
  -- in a file, sResult isn't set at all until it holds a ref to the result var in sAssigns
  --
  -- does only the repl version need the fancy remove-self-references logic?

--   let scr'  = scr {sAssigns = delVar (sAssigns scr) "result"}
--       scr'' = appendOrUpdateAssign scr' a
--   in trace "interpreter.parse.basic.assign"
--            ("scr:\n" ++ render (pPrint scr) ++ "\nscr'':\n" ++ render (pPrint scr''))
--            scr''

{-|
Behaviors that differ from 'appendStatementRepl':

* Does not auto-assign result; that's only done once at the end of the script if the explicit assignment was missing.
* Removes all but the last "result" var to prevent conflicts between included scripts and the main one
* Throws an error (TODO which class?) instead of overwriting any existing variable other than "result"

TODO where does the final auto-assign of result happen? Eval currently?
TODO add the error
-}
appendStatementFile :: Script -> Assign -> Script
appendStatementFile scr asn =
  let as   = sAssigns scr
      as'  = (if isResult asn then delVar as "result" else as) ++ [asn]
      r'   = if isResult asn then Just (aExpr asn) else sResult scr
      scr' = scr {sAssigns = as', sResult = r'}
  in trace "ortholang.script.appendStatementFile"
           ("scr:\n" ++ render (pPrint scr) ++ "\nscr':\n" ++ render (pPrint scr'))
           scr'

{-|
Behaviors that differ from 'appendStatementFile':

* Works on naked 'Expr's as well as 'Assign's. They're auto-assign to "result",
  overwriting the previous result if any.

* Allows overwriting an existing variable, but prompts the user to confirm it
  first. It may also have to drop the var's reverse dependencies if the type is
  being changed, and it checks that none of them appear in the new expression
  (that would create a cycle).

TODO implement the overwriting prompt thing!
-}
appendStatementRepl :: Script -> Either Expr Assign -> Script
appendStatementRepl scr (Left  e) = appendStatementRepl scr $ Right $ Assign (Var (RepID Nothing) "result") e
appendStatementRepl scr (Right asn@(Assign v e)) =
  let e' = removeResultReferences scr e
      asn'@(Assign (Var _ vName) e'') = removeAssignSelfReferences scr (Assign v e')
      as' = delVar (sAssigns scr) "result"
      as'' = if not (isResult asn') && v `elem` map aVar as'
               then replaceVar asn' as' -- update an old assignment in place (repl only)
               else as' ++ [asn'] -- append a new one (delVar only needed if it's "result")
      r' = if isResult asn'
             then Just e''
             else sResult scr
      scr' = scr {sAssigns = as'', sResult = r'}
      scr'' = if isResult asn'
                then scr'
                else appendResultRef scr' asn'
  in trace "ortholang.script.appendStatementRepl"
           ("scr:\n" ++ render (pPrint scr) ++ "\nscr'':\n" ++ render (pPrint scr''))
           scr''


appendResultRef :: Script -> Assign -> Script
appendResultRef scr (Assign v e) = scr {sAssigns = sAssigns scr ++ [resAsn], sResult = Just e}
  where
    resRef = Ref (typeOf e) (saltOf e) (depsOf e) v -- TODO vName in deps too?
    resAsn = Assign resultVar resRef

  -- Ref Type (Maybe Salt) [Var] Var -- do refs need a salt? yes! (i think?)

-- TODO general algorithm:
-- 0. make naked expr into result assign if needed (repl only)
-- 1. remove self-references to "result" in the expr if any
-- 2. remove "result" from assigns if any (repl version only)

------------
-- macros --
------------

{-|
This expands macros and checks that the resulting types make sense.
There's some basic typechecking during the 'OrthoLang.Interpreter.Parse' step too, but
it assumes every 'Function' is right about its return type. After
'MacroExpansion's have been applied we can ensure that inputs and outputs
actually match up.

After expansion there shouldn't be any functions implemented as macros left in
the script.

TODO wait, why can't macro fns be typechecked during parsing? Seems like they could!
-}
expandMacros :: [Module] -> Script -> Script
expandMacros = eScript

eScript :: [Module] -> Script -> Script
eScript mods s = s {sAssigns = map (eAssign mods s) (sAssigns s)}

eAssign :: [Module] -> Script -> Assign -> Assign
eAssign mods scr a@(Assign {aExpr = e}) = a {aExpr = eExpr mods scr e}

-- | This one is recursive in case one macro expression is hidden inside the
--   result of another
eExpr :: [Module] -> Script -> Expr -> Expr
eExpr mods scr e = if e' == e then e' else eExpr' mods scr e'
  where
    e' = eExpr' mods scr e

eExpr' :: [Module] -> Script -> Expr -> Expr
eExpr' mods scr e@(Fun r s ds name es) =
  let e' = Fun r s ds name $ map (eExpr mods scr) es
  in case findFun mods name of
       Left err -> error "script.eExpr'" err
       Right fn -> case fNewRules fn of
                     -- TODO is another typechecking step necessary? maybe doesn't add anything
                     -- (NewMacro m) -> case typecheck mods (m scr e) of
                     --                   Left err -> error err
                     --                   Right e' -> e'
                     (NewMacro m) -> let e'' = m scr e
                                     in trace "script.eExpr'"
                                              ("expanded macro: " ++ show e ++ " -> " ++ show e'') e''
                     _ -> e'
eExpr' mods scr (Bop r ms vs n e1 e2) = Bop r ms vs n (eExpr mods scr e1) (eExpr mods scr e2)
eExpr' mods scr (Lst r vs es) = Lst r vs $ map (eExpr mods scr) es
eExpr' _ _ e = e

-- typecheck :: [Module] -> Expr -> Either String Expr
-- typecheck mods expr = undefined


-------------
-- replace --
-------------

-- TODO what if it's a function call?
-- do we have to make a rule that you can't use those?
-- (uuuugly! but not a show-stopper for now)
extractExprs :: Script -> Expr -> [Expr]
extractExprs  _  (Lst _ _ es) = es
extractExprs s (Ref _ _ _ v ) = case lookupVar v (sAssigns s) of
                                       Nothing -> error "types.extractExprs" $ "no such var " ++ show v
                                       Just e  -> extractExprs s e
extractExprs _   (Fun _ _ _ _ _) = error "types.extractExprs" explainFnBug
extractExprs scr (Bop _ _ _ _ l r) = extractExprs scr l ++ extractExprs scr r
extractExprs  _   e               = error "types.extractExprs" $ "bad arg: " ++ show e

-- TODO will this get printed, or will there just be a parse error?
explainFnBug :: String
explainFnBug =
  "You've stumbled on an outstanding bug. Sorry about that! \
  \The problem is that when doing transformations involving lists \
  \like repeat or map, OrthoLang can't \"see\" through future function calls; \
  \it can only manipulate lists whose elements are known *before* running the \
  \program. If you want Jeff to consider rewriting some things to fix that, \
  \drop him a line!"

{- This does the filename mangling by setting a "replace ID" in each variable
 - in a script. If it's anything other than Nothing it gets used by
 - OrthoLang.Interpreter to set the rep dir.
 -}
mapExprVars :: (Var -> Var) -> Expr -> Expr
mapExprVars _ e@(Lit  _ _) = e
mapExprVars fn (Ref  t n vs v      ) = Ref  t n (map fn vs)   (fn v)
mapExprVars fn (Bop  t n vs s e1 e2) = Bop  t n (map fn vs) s (mapExprVars fn e1) (mapExprVars fn e2)
mapExprVars fn (Fun  t n vs s es   ) = Fun  t n (map fn vs) s (map (mapExprVars fn) es)
mapExprVars fn (Lst t vs   es   ) = Lst t (map fn vs)   (map (mapExprVars fn) es)
mapExprVars _ (Com _) = error "script.mapExprVars" "implement this!"

mapAssignVars :: (Var -> Var) -> Assign -> Assign
mapAssignVars fn a@(Assign var expr) = Assign (fn var) (mapExprVars fn expr)

mapScriptVars :: (Var -> Var) -> Script -> Script
mapScriptVars fn scr = scr
  { sAssigns = map (mapAssignVars fn) (sAssigns scr)
  , sResult  = fmap (mapExprVars fn) (sResult scr)
  }

setRepID :: RepID -> Var -> Var
setRepID newID (Var _ name) = Var newID name

setRepIDs :: RepID -> Script -> Script
setRepIDs newID = mapScriptVars (setRepID newID)


----------
-- repl --
----------

rDepsOf :: Script -> Var -> [Var]
rDepsOf s var = map aVar rDeps
  where
    rDeps = filter (isRDep . aExpr) (sAssigns s)
    isRDep expr = elem var $ depsOf expr

depsOnly :: Expr -> Script -> Script
depsOnly expr scr = scr {sAssigns = deps ++ [res]}
  where
    deps = filter (\a -> (elem (aVar a) $ depsOf expr)) (sAssigns scr)
    res  = Assign {aVar = Var (RepID Nothing) "result", aExpr = expr}

{-|
This is needed to avoid assigning a variable literally to itself, which is
especially a problem when auto-assigning "result". It takes the current script
and a new 'Assign' statement, and either updates an old assignment in the
script or appends the new one.

TODO is this where we can easily require the replacement var's type to match if it has deps?
TODO what happens if you try that in a script? it should fail i guess?
-}
-- appendOrUpdateAssign :: Script -> Assign -> Script
-- appendOrUpdateAssign scr asn@(Assign {aVar = v@(Var _ vName), aExpr = expr}) =
--   trace "ortholang.script.appendOrUpdateAssign" 
--         ("scr:\n" ++ render (pPrint scr) ++ "\nscr':\n" ++ render (pPrint scr'))
--         scr'
--   where
--     scr' = scr {sAssigns = as', sResult = r'}
--     asn' = removeAssignSelfReferences scr asn -- TODO prohibit self-references from appearing in scripts
--     as  = sAssigns scr
--     as' = if not (isResult asn') && v `elem` map aVar as 
--             then replaceVar asn' as        -- update an old assignment in place (repl only)
--             else delVar as vName ++ [asn'] -- append a new one (delVar only needed if it's "result")
--     r' = if isResult asn'
--            then Just expr
--            else sResult scr

{-|
Update the 'Expr' in a list of 'Assign's to a new value. Used to implement
'appendOrUpdateAssign'.
-}
replaceVar :: Assign -> [Assign] -> [Assign]
replaceVar a1 = map $ \a2 -> if aVar a1 == aVar a2 then a1 else a2

{-|
This makes it ok to assign a var to itself in the repl by replacing the
reference with its expression. For example:

@
ortholang —▶ myvar = 1
ortholang —▶ :show
myvar = 1
result = myvar

ortholang —▶ myvar = myvar + 1
ortholang —▶ :show
myvar = 1 + 1
result = myvar
@

That's only allowed in the Repl of course.
-}
removeAssignSelfReferences :: Script -> Assign -> Assign
removeAssignSelfReferences s a@(Assign {aVar=v, aExpr=e}) =
  if not (v `elem` depsOf e)
    then a
    else a {aExpr=rmRef s v e}

removeResultReferences :: Script -> Expr -> Expr
removeResultReferences scr expr = rmRef scr resultVar expr

{-|
Remove a particular 'Var' from an 'Expr' by substituting its value from the
'Script'.  Used to implement 'removeAssignSelfReferences'.
-}
rmRef :: Script -> Var -> Expr -> Expr
rmRef s v e = let r = rmRef' s v e
              in trace "ortholang.script.rmRef" (render (pPrint e) ++ " -> " ++ render (pPrint r)) r

rmRef' :: Script -> Var -> Expr -> Expr
rmRef' scr var e@(Ref _ _ _ v2)
  | var == v2 = justOrDie "failed to rmRef variable!" $ lookupVar var (sAssigns scr)
  | otherwise = e
rmRef' _   _   e@(Lit _ _) = e
rmRef' scr var (Bop  t ms vs s e1 e2) = Bop t ms (delete var vs) s (rmRef scr var e1) (rmRef scr var e2)
rmRef' scr var (Fun  t ms vs s es   ) = Fun t ms (delete var vs) s (map (rmRef scr var) es)
rmRef' scr var (Lst t vs       es   ) = Lst t    (delete var vs)   (map (rmRef scr var) es)
rmRef' _   _   (Com _) = error "types.rmRef" "implement this! or rethink?"
