module ShortCut.Modules where

import ShortCut.Core.Types (CutModule)

import qualified ShortCut.Modules.Math  as M
import qualified ShortCut.Modules.Sets  as S
-- TODO remove: import qualified ShortCut.Modules.Files as F
import qualified ShortCut.Modules.Blast as B

modules :: [CutModule]
modules =
  [ M.cutModule
  , S.cutModule
  -- , F.cutModule
  , B.cutModule
  ]
