module ShortCut.Core
  ( runRepl
  , evalFile
  , CutConfig(..)
  )
  where

-- TODO add stuff for config file in core if possible

import ShortCut.Core.Repl  (runRepl)
import ShortCut.Core.Eval  (evalFile)
import ShortCut.Core.Types (CutConfig(..))
