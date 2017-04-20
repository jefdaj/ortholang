module ShortCut.Core
  ( runRepl
  , evalFile
  , CutConfig(..)
  , CutModule(..)
  )
  where

import ShortCut.Core.Eval  (evalFile)
import ShortCut.Core.Repl  (runRepl)
import ShortCut.Core.Types (CutConfig(..), CutModule(..))
