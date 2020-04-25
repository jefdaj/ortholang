{-|
TODO move all script transformations here if possible and document them nicely
-}

module OrthoLang.Script
  (

    rDepsOf
  , extractExprs
  , updateVars
  , replaceVar
  , removeSelfReferences
  , rmRef

  )
  where

import Prelude hiding (error)
import OrthoLang.Debug (trace, error)
import OrthoLang.Types

import Data.List           (filter, delete)
import OrthoLang.Util               (absolutize, justOrDie)

rDepsOf :: Script -> Var -> [Var]
rDepsOf s var = map aVar rDeps
  where
    rDeps = filter (isRDep . aExpr) (sAssigns s)
    isRDep expr = elem var $ depsOf expr

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

-- this is needed to avoid assigning a variable literally to itself,
-- which is especially a problem when auto-assigning "result"
-- TODO is this where we can easily require the replacement var's type to match if it has deps?
-- TODO what happens if you try that in a script? it should fail i guess?
-- TODO rename because it's more like assignAndUpdateVars?
updateVars :: Script -> Assign -> Script
updateVars scr asn@(Assign {aVar = v@(Var _ vName), aExpr = expr}) = scr {sAssigns = as', sResult = r'}
  where
    res = Var (RepID Nothing) "result"
    asn' = removeSelfReferences scr asn
    as  = sAssigns scr
    as' = if v /= res && aVar asn `elem` map aVar as
            then replaceVar asn' as
            else delVar as vName ++ [asn']
    r' = case sResult scr of
           Nothing -> Just expr -- no previous result, so assign it now
           Just re -> if v == res then Just expr else sResult scr

-- replace an existing var in a script
replaceVar :: Assign -> [Assign] -> [Assign]
replaceVar a1 = map $ \a2 -> if aVar a1 == aVar a2 then a1 else a2

-- makes it ok to assign a var to itself in the repl
-- by replacing the reference with its value at that point
-- TODO forbid this in scripts though
removeSelfReferences :: Script -> Assign -> Assign
removeSelfReferences s a@(Assign {aVar=v, aExpr=e}) = if not (v `elem` depsOf e) then a else a {aExpr=rmRef s v e}

-- does the actual work of removing self-references
rmRef :: Script -> Var -> Expr -> Expr
rmRef scr var e@(Ref _ _ _ v2)
  | var == v2 = justOrDie "failed to rmRef variable!" $ lookupVar var (sAssigns scr)
  | otherwise = e
rmRef _   _   e@(Lit _ _) = e
rmRef scr var (Bop  t ms vs s e1 e2) = Bop t ms (delete var vs) s (rmRef scr var e1) (rmRef scr var e2)
rmRef scr var (Fun  t ms vs s es   ) = Fun t ms (delete var vs) s (map (rmRef scr var) es)
rmRef scr var (Lst t vs       es   ) = Lst t    (delete var vs)   (map (rmRef scr var) es)
rmRef _   _   (Com _) = error "types.rmRef" "implement this! or rethink?"
