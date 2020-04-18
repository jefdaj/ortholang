{-|
This module expands macros and checks that the resulting types make sense.
There's some basic typechecking during the 'OrthoLang.Interpreter.Parse' step too, but
it assumes every 'Function' is right about its return type. After
'MacroExpansion's have been applied we can ensure that inputs and outputs
actually match up.

After expansion there shouldn't be any functions implemented as macros left in
the script.

TODO wait, why can't macro fns be typechecked during parsing? Seems like they could!
-}

module OrthoLang.Interpreter.Expand
  (
  
  -- * Expand all macros in a script
    expandMacros

  -- * Implementation details
  , eScript
  , eExpr
  , eAssign

  )
  where

import OrthoLang.Debug (trace)
import OrthoLang.Types

expandMacros :: Config -> Script -> Script
expandMacros = eScript

eScript :: Config -> Script -> Script
eScript cfg scr = map (eAssign cfg scr) scr

eAssign :: Config -> Script -> Assign -> Assign
eAssign cfg scr (v, e) = (v, eExpr cfg scr e)

-- | This one is recursive in case one macro expression is hidden inside the
--   result of another
eExpr :: Config -> Script -> Expr -> Expr
eExpr cfg scr e = if e' == e then e' else eExpr' cfg scr e'
  where
    e' = eExpr' cfg scr e

eExpr' :: Config -> Script -> Expr -> Expr
eExpr' cfg scr e@(Fun r s ds name es) =
  let e' = Fun r s ds name $ map (eExpr cfg scr) es
  in case findFun cfg name of
       Left err -> error err
       Right fn -> case fNewRules fn of
                     -- TODO is another typechecking step necessary? maybe doesn't add anything
                     -- (NewMacro m) -> case typecheck cfg (m scr e) of
                     --                   Left err -> error err
                     --                   Right e' -> e'
                     (NewMacro m) -> let e'' = m scr e
                                     in trace "core.expand.eExpr'"
                                              ("expanded macro: " ++ show e ++ " -> " ++ show e'') e''
                     _ -> e'
eExpr' cfg scr (Bop r ms vs n e1 e2) = Bop r ms vs n (eExpr cfg scr e1) (eExpr cfg scr e2)
eExpr' cfg scr (Lst r vs es) = Lst r vs $ map (eExpr cfg scr) es
eExpr' _ _ e = e

-- typecheck :: Config -> Expr -> Either String Expr
-- typecheck cfg expr = undefined
