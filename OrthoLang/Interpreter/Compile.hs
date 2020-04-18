module OrthoLang.Interpreter.Compile
  ( aSimpleScript
  , aSimpleScriptNoFix
  , aSimpleScriptPar
  -- , applyList2
  , compose1
  , compileScript
  , debugC
  , debugRules
  -- , defaultTypeCheck
  , map3of3
  , newBop
  , newFnA1
  , newFnA2
  , newFnA3
  , newFnS1
  , newFnS2
  , newFnS3
  , aNewRulesS1
  , aNewRulesS2
  , aNewRulesS3
  , newMacro
  , newRules
  , MacroExpansion
  -- , rBop
  , rExpr
  , rFun3
  , rMap
  , rMapSimpleScript
  , rMapTmps
  , rSimple
  , rSimpleScript
  , rSimpleScriptPar
  , rSimpleTmp
  )
  where

import OrthoLang.Interpreter.Compile.Basic
import OrthoLang.Interpreter.Compile.Simple
import OrthoLang.Interpreter.Compile.Map
import OrthoLang.Interpreter.Compile.Map2
-- import OrthoLang.Interpreter.Compile.Repeat
import OrthoLang.Interpreter.Compile.Compose
import OrthoLang.Interpreter.Compile.NewRules
