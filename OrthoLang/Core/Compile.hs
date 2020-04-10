module OrthoLang.Core.Compile
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

import OrthoLang.Core.Compile.Basic
import OrthoLang.Core.Compile.Simple
import OrthoLang.Core.Compile.Map
import OrthoLang.Core.Compile.Map2
-- import OrthoLang.Core.Compile.Repeat
import OrthoLang.Core.Compile.Compose
import OrthoLang.Core.Compile.NewRules
