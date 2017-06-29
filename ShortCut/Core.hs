module ShortCut.Core
  ( runRepl
  , evalFile
  , CutConfig(..)
  , CutModule(..)
  , prettyShow
  )
  where

import ShortCut.Core.Pretty
import ShortCut.Core.Debug (debug, debugShow)
import ShortCut.Core.Eval  (evalFile)
import ShortCut.Core.Repl  (runRepl)
import ShortCut.Core.Types (CutConfig(..), CutModule(..))
