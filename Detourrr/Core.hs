module Detourrr.Core
  ( runRepl
  , evalFile
  , DtrConfig(..)
  , DtrModule(..)
  , prettyShow
  )
  where

-- TODO be systematic about what's exported here
-- TODO export Paths stuff?

import Detourrr.Core.Pretty (prettyShow)
import Detourrr.Core.Eval   (evalFile)
import Detourrr.Core.Repl   (runRepl)
import Detourrr.Core.Types  (DtrConfig(..), DtrModule(..)) 
