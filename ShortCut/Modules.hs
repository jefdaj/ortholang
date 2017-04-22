module ShortCut.Modules where

import ShortCut.Core.Types (CutModule)

import qualified ShortCut.Modules.Math   as M
import qualified ShortCut.Modules.SetOps as S
import qualified ShortCut.Modules.Blast  as B

modules :: [CutModule]
modules =
  [ M.cutModule
  , S.cutModule
  , B.cutModule
  ]
