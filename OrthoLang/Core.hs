module OrthoLang.Core
  ( runRepl
  , evalFile
  , Config(..)
  , Module(..)
  , prettyShow
  )
  where

-- TODO be systematic about what's exported here
-- TODO export Paths stuff?

import OrthoLang.Core.Pretty (prettyShow)
import OrthoLang.Core.Eval   (evalFile)
import OrthoLang.Core.Repl   (runRepl)
import OrthoLang.Core.Types  (Config(..), Module(..))
