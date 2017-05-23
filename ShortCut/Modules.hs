module ShortCut.Modules where

import ShortCut.Core.Types (CutModule(..))

import qualified ShortCut.Modules.Cheat as C
import qualified ShortCut.Modules.Math  as M
import qualified ShortCut.Modules.Sets  as S
import qualified ShortCut.Modules.Blast as B
import qualified ShortCut.Modules.BioMartR as BM

modules :: [CutModule]
modules =
  [ C.cutModule
  , M.cutModule
  , S.cutModule
  , B.cutModule
  , BM.cutModule
  ]
