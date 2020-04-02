module OrthoLang.Modules (modules)
  where

import OrthoLang.Core.Types (Module)

import qualified OrthoLang.Modules.AllVsAll      as AllVsAll
import qualified OrthoLang.Modules.BioMartR      as BioMartR
import qualified OrthoLang.Modules.Blast         as Blast
import qualified OrthoLang.Modules.BlastDB       as BlastDB
import qualified OrthoLang.Modules.BlastHits     as BlastHits
import qualified OrthoLang.Modules.BlastRBH      as BlastRBH
import qualified OrthoLang.Modules.Busco         as Busco
import qualified OrthoLang.Modules.CRBBlast      as CRBBlast
import qualified OrthoLang.Modules.Cheat         as Cheat
import qualified OrthoLang.Modules.Diamond       as Diamond
import qualified OrthoLang.Modules.GreenCut      as GreenCut
import qualified OrthoLang.Modules.Hmmer         as Hmmer
import qualified OrthoLang.Modules.ListLike      as ListLike
import qualified OrthoLang.Modules.Load          as Load
import qualified OrthoLang.Modules.MMSeqs        as MMSeqs
import qualified OrthoLang.Modules.Math          as Math
import qualified OrthoLang.Modules.Muscle        as Muscle
import qualified OrthoLang.Modules.NewRulesTest  as NewRulesTest
import qualified OrthoLang.Modules.OrthoFinder   as OrthoFinder
import qualified OrthoLang.Modules.OrthoGroups   as OrthoGroups
import qualified OrthoLang.Modules.Permute       as Permute
import qualified OrthoLang.Modules.Plots         as Plots
import qualified OrthoLang.Modules.PsiBlast      as PsiBlast
import qualified OrthoLang.Modules.Range         as Range
import qualified OrthoLang.Modules.Repeat        as Repeat
import qualified OrthoLang.Modules.Replace       as Replace
import qualified OrthoLang.Modules.Sample        as Sample
import qualified OrthoLang.Modules.Scores        as Scores
import qualified OrthoLang.Modules.SeqIO         as SeqIO
import qualified OrthoLang.Modules.Sets          as Sets
import qualified OrthoLang.Modules.SetsTable     as SetsTable
import qualified OrthoLang.Modules.SonicParanoid as SonicParanoid
import qualified OrthoLang.Modules.Summarize     as Summarize

modules :: [Module]
modules =
  [

  -- works in progress and/or tests
    AllVsAll.olModule
  , Cheat.olModule
  , GreenCut.olModule
  , NewRulesTest.olModule

  -- core language features
  , ListLike.olModule -- TODO expose Function fields in API
  , Load.olModule
  , Math.olModule
  , Permute.olModule
  , Range.olModule
  , Repeat.olModule
  , Replace.olModule
  , Sample.olModule
  , Scores.olModule
  , Sets.olModule
  , SetsTable.olModule
  , Summarize.olModule

  -- load, download, and convert sequences
  , BioMartR.olModule
  , SeqIO.olModule

  -- BLAST
  , Blast.olModule
  , BlastDB.olModule
  , BlastHits.olModule
  , PsiBlast.olModule

  -- BLAST reciprocal best hits
  , BlastRBH.olModule
  , CRBBlast.olModule

  -- other sequence search programs
  , Diamond.olModule
  , Hmmer.olModule
  , MMSeqs.olModule
  , Muscle.olModule

  -- orthogroup search programs
  , Busco.olModule
  , OrthoFinder.olModule
  , OrthoGroups.olModule
  , SonicParanoid.olModule

  -- plots and figures
  , Plots.olModule

  ]
