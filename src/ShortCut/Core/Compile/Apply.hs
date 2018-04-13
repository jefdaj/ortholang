module ShortCut.Core.Compile.Apply where

{- Attempt to compose ShortCut functions. Not working yet and may be aborted.
 -}

import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic

rApply1 :: RulesFn -> RulesFn -> RulesFn
rApply1 rules1 rules2 state expr1 = rules2 state expr2
  where
    expr2 = CutRules $ CompiledExpr expr1 $ rExpr state expr1
