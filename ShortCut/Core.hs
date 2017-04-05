module ShortCut.Core
  ( runRepl
  , runScript
  , CutConfig(..)
  )
  where

-- TODO add stuff for config file in core if possible

import ShortCut.Core.Repl   (runRepl)
import ShortCut.Core.Script (runScript)
import ShortCut.Core.Types  (CutConfig(..))
