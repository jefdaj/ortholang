{-|
This module expands macros and checks that the resulting types make sense.
There's some basic typechecking during the 'OrthoLang.Core.Parse' step too, but
it assumes every 'Function' is right about its return type. After
'MacroExpansion's have been applied we can ensure that inputs and outputs
actually match up.

After expansion there shouldn't be any functions implemented as macros left in
the script.

TODO wait, why can't macro fns be typechecked during parsing? Seems like they could!
-}

module OrthoLang.Core.Expand
  (
  
  -- * Expand all macros in a script
    expandMacros

  -- * Implementation details
  , eScript
  , eExpr
  , eAssign

  )
  where

import OrthoLang.Core.Types

-- | Runs eScript until everything has been expanded
expandMacros :: Config -> Script -> Script
expandMacros cfg scr = if scr' == scr then scr else expandMacros cfg scr'
  where
    scr' = eScript cfg scr

eScript :: Config -> Script -> Script
eScript cfg scr = map (eAssign cfg scr) scr

eAssign :: Config -> Script -> Assign -> Assign
eAssign cfg scr (v, e) = (v, eExpr cfg scr e)

eExpr :: Config -> Script -> Expr -> Expr
eExpr cfg scr e@(Fun _ _ _ name _) =
  case findFun cfg name of
    Left err -> error err
    Right fn -> case fNewRules fn of
                  (NewMacro m) -> case typecheck cfg (m scr e) of
                                    Left err -> error err
                                    Right e' -> e'
                  _ -> e
eExpr _ _ e = e

typecheck :: Config -> Expr -> Either String Expr
typecheck cfg expr = undefined
