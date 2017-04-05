module ShortCut.Core.Script
	( runScript
	)
	where

import Data.Maybe              (fromJust)
import ShortCut.Core.Compile   (cScript)
import ShortCut.Core.Interpret (iFile, eval)
import ShortCut.Core.Types     (CutConfig(..))

-- TODO this should be called iFile right, and the other one goes away?
runScript :: CutConfig -> IO ()
runScript cfg = do
  f <- iFile $ fromJust $ cfgScript cfg -- TODO something safer!
  case f of
    Left  e -> fail $ "oh no! " ++ show e
    Right s -> eval cfg $ cScript cfg s
