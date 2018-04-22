module ShortCut.Modules where

import ShortCut.Core.Types (CutModule(..))

import qualified ShortCut.Modules.BioMartR  as BioMartR
import qualified ShortCut.Modules.Blast     as Blast
import qualified ShortCut.Modules.BlastCRB  as BlastCRB
import qualified ShortCut.Modules.BlastDB   as BlastDB
import qualified ShortCut.Modules.BlastHits as BlastHits
import qualified ShortCut.Modules.BlastRBH  as BlastRBH
-- import qualified ShortCut.Modules.Cheat     as Cheat -- TODO write this
import qualified ShortCut.Modules.Load      as Load
import qualified ShortCut.Modules.Length    as Length
import qualified ShortCut.Modules.Math      as Math
import qualified ShortCut.Modules.Permute   as Permute
import qualified ShortCut.Modules.Repeat    as Repeat
import qualified ShortCut.Modules.SeqIO     as SeqIO
import qualified ShortCut.Modules.Sets      as Sets
import qualified ShortCut.Modules.Summarize as Summarize
import qualified ShortCut.Modules.Scores    as Scores
import qualified ShortCut.Modules.Plots     as Plots
-- import qualified ShortCut.Modules.PsiBlast  as PsiBlast

modules :: [CutModule]
modules =
  [ BioMartR.cutModule
  , Blast.cutModule
  , BlastCRB.cutModule
  , BlastDB.cutModule
  , BlastHits.cutModule
  , BlastRBH.cutModule
  -- , Cheat.cutModule
  , Load.cutModule
  , Length.cutModule
  , Math.cutModule
  , Permute.cutModule
  , Repeat.cutModule
  , SeqIO.cutModule
  , Sets.cutModule
  , Summarize.cutModule
  , Scores.cutModule
  , Plots.cutModule
  -- , PsiBlast.cutModule
  ]
