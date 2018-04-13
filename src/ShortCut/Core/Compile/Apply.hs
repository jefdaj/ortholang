module ShortCut.Core.Compile.Apply where

{- Attempt to compose ShortCut functions. Not working yet and may be aborted.
 - TODO if this works, is it an Applicative?
 -}

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic
import Debug.Trace

-- apply1 :: RulesFn -> RulesFn -> RulesFn
-- apply1 rules1 rules2 state expr1 = rules2 state expr2
--   where
--     expr2 = CutRules $ CompiledExpr expr1 $ rExpr state expr1

-- rApply1 :: RulesFn -> RulesFn -> RulesFn
-- rApply1 rules1 rules2 state expr1 = rules2 state expr2
--   where
--     expr2 = CutRules $ CompiledExpr expr1 $ rExpr state expr1

-- Compiles the expr with rules1, re-wraps in a CutExpr, and compiles that with rules2
-- rApply1 :: RulesFn -> RulesFn -> RulesFn
-- rApply1 rules1 rules2 state expr = trace ("expr2: " ++ show expr2) res
--   where
--     expr2 = CutRules $ CompiledExpr expr $ rules1 state expr
--     res   = rules2 state expr2

-- rApply1 :: CutFunction -> CutFunction -> RulesFn
-- rApply1 fn1 fn2 st expr2 = rules2 st expr2
