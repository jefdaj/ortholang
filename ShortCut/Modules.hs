module ShortCut.Modules where

import ShortCut.Core.Types (CutModule(..))

import qualified ShortCut.Modules.BioMartR  as BioMartR
import qualified ShortCut.Modules.Blast     as Blast
import qualified ShortCut.Modules.BlastCRB  as BlastCRB
import qualified ShortCut.Modules.Cheat     as Cheat -- TODO write this
import qualified ShortCut.Modules.Math      as Math
import qualified ShortCut.Modules.PRS       as PRS
import qualified ShortCut.Modules.Sets      as Sets
import qualified ShortCut.Modules.SeqIO     as SeqIO

modules :: [CutModule]
modules =
  [ BioMartR.cutModule
  , Blast.cutModule
  , BlastCRB.cutModule
  , Cheat.cutModule
  , Math.cutModule
  , PRS.cutModule
  , Sets.cutModule
  , SeqIO.cutModule
  ]
