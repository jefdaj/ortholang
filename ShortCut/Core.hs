module ShortCut.Core
  ( runRepl
  , evalFile
  , CutConfig(..)
  , CutModule(..)
  , prettyShow
  )
  where

-- TODO be systematic about what's exported here

import ShortCut.Core.Pretty (prettyShow)
import ShortCut.Core.Eval   (evalFile)
import ShortCut.Core.Repl   (runRepl)
import ShortCut.Core.Types  (CutConfig(..), CutModule(..))
