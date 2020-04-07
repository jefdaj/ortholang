module OrthoLang.Core.Compile
  ( aSimpleScript
  , aSimpleScriptNoFix
  , aSimpleScriptPar
  -- , applyList2
  , compose1
  , compileScript
  , curl
  , debugC
  , debugRules
  , defaultTypeCheck
  , map3of3
  , mkLoad
  , mkLoadList
  , newBop
  , newFnA1
  , newFnA2
  , newFnA3
  , newFnAT1
  , newFnAT2
  , newFnAT3
  , newFnS1
  , newFnS2
  , newFnS3
  , newFnST1
  , newFnST2
  , newFnST3
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
  , typeError
  )
  where

import OrthoLang.Core.Compile.Basic
import OrthoLang.Core.Compile.Simple
import OrthoLang.Core.Compile.Map
import OrthoLang.Core.Compile.Map2
-- import OrthoLang.Core.Compile.Repeat
import OrthoLang.Core.Compile.Compose
import OrthoLang.Core.Compile.NewRules
