module ShortCut.Modules where

import ShortCut.Core.Types (CutModule(..))

-- these started as modules but ended up depending heavily on core modifications
import qualified ShortCut.Core.Compile.Replace as Replace
import qualified ShortCut.Core.Compile.Repeat  as Repeat

-- whereas these are actually modular
import qualified ShortCut.Modules.BioMartR  as BioMartR
import qualified ShortCut.Modules.Blast     as Blast
import qualified ShortCut.Modules.CRBBlast  as CRBBlast
import qualified ShortCut.Modules.BlastDB   as BlastDB
import qualified ShortCut.Modules.BlastHits as BlastHits
import qualified ShortCut.Modules.BlastRBH  as BlastRBH
-- import qualified ShortCut.Modules.Cheat     as Cheat -- TODO write this
import qualified ShortCut.Modules.Load      as Load
import qualified ShortCut.Modules.Length    as Length
import qualified ShortCut.Modules.Math      as Math
import qualified ShortCut.Modules.Muscle    as Muscle
import qualified ShortCut.Modules.Permute   as Permute
import qualified ShortCut.Modules.SeqIO     as SeqIO
import qualified ShortCut.Modules.Sets      as Sets
import qualified ShortCut.Modules.Sample    as Sample
import qualified ShortCut.Modules.Summarize as Summarize
import qualified ShortCut.Modules.Scores    as Scores
import qualified ShortCut.Modules.Plots     as Plots
import qualified ShortCut.Modules.PsiBlast  as PsiBlast
import qualified ShortCut.Modules.Hmmer     as Hmmer
import qualified ShortCut.Modules.OrthoFinder as OrthoFinder
import qualified ShortCut.Modules.Diamond   as Diamond
import qualified ShortCut.Modules.MMSeqs    as MMSeqs
import qualified ShortCut.Modules.SonicParanoid as SonicParanoid
import qualified ShortCut.Modules.OrthoGroups as OrthoGroups

modules :: [CutModule]
modules =
  [ Replace.cutModule
  , Repeat.cutModule

  , Math.cutModule
  , Load.cutModule
  , Sets.cutModule
  , SeqIO.cutModule
  , BioMartR.cutModule
  , BlastDB.cutModule
  , Blast.cutModule
  , BlastHits.cutModule
  , Length.cutModule
  , PsiBlast.cutModule
  , CRBBlast.cutModule
  , Hmmer.cutModule
  , BlastRBH.cutModule
  -- , Cheat.cutModule
  , Muscle.cutModule
  , Sample.cutModule
  , Permute.cutModule
  , Summarize.cutModule
  , Scores.cutModule
  , Plots.cutModule
  , OrthoFinder.cutModule
  , Diamond.cutModule
  , MMSeqs.cutModule
  , SonicParanoid.cutModule
  , OrthoGroups.cutModule
  ]
