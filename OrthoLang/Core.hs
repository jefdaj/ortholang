module OrthoLang.Core
  ( runRepl
  , evalFile
  , OrthoLangConfig(..)
  , OrthoLangModule(..)
  , prettyShow
  )
  where

-- TODO be systematic about what's exported here
-- TODO export Paths stuff?

import OrthoLang.Core.Pretty (prettyShow)
import OrthoLang.Core.Eval   (evalFile)
import OrthoLang.Core.Repl   (runRepl)
import OrthoLang.Core.Types  (OrthoLangConfig(..), OrthoLangModule(..))
