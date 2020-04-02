module OrthoLang.Core.Compile
  ( aSimpleScript
  , aSimpleScriptNoFix
  , aSimpleScriptPar
  , compose1
  , compileScript
  , curl
  , debug
  , debugRules
  , defaultTypeCheck
  , map3of3
  , mkLoad
  , mkLoadList
  , newBop
  , newFn1
  , newFn2
  , newFn3
  , newMacro
  , newFunctionRules
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
