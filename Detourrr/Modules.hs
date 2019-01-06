module Detourrr.Modules where

import Detourrr.Core.Types (RrrModule(..))

import qualified Detourrr.Modules.BioMartR  as BioMartR
import qualified Detourrr.Modules.Blast     as Blast
import qualified Detourrr.Modules.CRBBlast  as CRBBlast
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

modules :: [RrrModule]
modules =
  [ Math.rrrModule
  , Load.rrrModule
  , Sets.rrrModule
  , SeqIO.rrrModule
  , BioMartR.rrrModule
  , BlastDB.rrrModule
  , Blast.rrrModule
  , BlastHits.rrrModule
  , Length.rrrModule
  , PsiBlast.rrrModule
  , CRBBlast.rrrModule
  , Hmmer.rrrModule
  , BlastRBH.rrrModule
  -- , Cheat.rrrModule
  , Muscle.rrrModule
  , Sample.rrrModule
  , Permute.rrrModule
  , Repeat.rrrModule
  , Summarize.rrrModule
  , Scores.rrrModule
  , Plots.rrrModule
  , OrthoFinder.rrrModule
  , Diamond.rrrModule
  , MMSeqs.rrrModule
  , SonicParanoid.rrrModule
  , OrthoGroups.rrrModule
  ]
