module Detourrr.Modules where

import Detourrr.Core.Types (DtrModule(..))

import qualified Detourrr.Modules.BioMartR  as BioMartR
import qualified Detourrr.Modules.Blast     as Blast
import qualified Detourrr.Modules.BlastCRB  as BlastCRB
import qualified Detourrr.Modules.BlastDB   as BlastDB
import qualified Detourrr.Modules.BlastHits as BlastHits
import qualified Detourrr.Modules.BlastRBH  as BlastRBH
-- import qualified Detourrr.Modules.Cheat     as Cheat -- TODO write this
import qualified Detourrr.Modules.Load      as Load
import qualified Detourrr.Modules.Length    as Length
import qualified Detourrr.Modules.Math      as Math
import qualified Detourrr.Modules.Muscle      as Muscle
import qualified Detourrr.Modules.Permute   as Permute
import qualified Detourrr.Modules.Repeat    as Repeat
import qualified Detourrr.Modules.SeqIO     as SeqIO
import qualified Detourrr.Modules.Sets      as Sets
import qualified Detourrr.Modules.Sample    as Sample
import qualified Detourrr.Modules.Summarize as Summarize
import qualified Detourrr.Modules.Scores    as Scores
import qualified Detourrr.Modules.Plots     as Plots
import qualified Detourrr.Modules.PsiBlast  as PsiBlast
import qualified Detourrr.Modules.Hmmer     as Hmmer
import qualified Detourrr.Modules.OrthoFinder as OrthoFinder
import qualified Detourrr.Modules.Diamond   as Diamond
import qualified Detourrr.Modules.MMSeqs    as MMSeqs
import qualified Detourrr.Modules.SonicParanoid as SonicParanoid
import qualified Detourrr.Modules.OrthoGroups as OrthoGroups

modules :: [DtrModule]
modules =
  [ Math.dtrModule
  , Load.dtrModule
  , Sets.dtrModule
  , SeqIO.dtrModule
  , BioMartR.dtrModule
  , BlastDB.dtrModule
  , Blast.dtrModule
  , BlastHits.dtrModule
  , Length.dtrModule
  , PsiBlast.dtrModule
  , BlastCRB.dtrModule
  , Hmmer.dtrModule
  , BlastRBH.dtrModule
  -- , Cheat.dtrModule
  , Muscle.dtrModule
  , Sample.dtrModule
  , Permute.dtrModule
  , Repeat.dtrModule
  , Summarize.dtrModule
  , Scores.dtrModule
  , Plots.dtrModule
  , OrthoFinder.dtrModule
  , Diamond.dtrModule
  , MMSeqs.dtrModule
  , SonicParanoid.dtrModule
  , OrthoGroups.dtrModule
  ]
