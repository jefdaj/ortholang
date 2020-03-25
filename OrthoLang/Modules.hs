module OrthoLang.Modules where

import OrthoLang.Core.Types

-- these started as modules but ended up depending heavily on core modifications
import qualified OrthoLang.Modules.Replace as Replace
import qualified OrthoLang.Modules.Repeat  as Repeat

-- whereas these are actually modular
import qualified OrthoLang.Modules.BioMartR  as BioMartR
import qualified OrthoLang.Modules.Blast     as Blast
import qualified OrthoLang.Modules.CRBBlast  as CRBBlast
import qualified OrthoLang.Modules.BlastDB   as BlastDB
import qualified OrthoLang.Modules.BlastHits as BlastHits
import qualified OrthoLang.Modules.BlastRBH  as BlastRBH
-- import qualified OrthoLang.Modules.Cheat     as Cheat -- TODO write this
import qualified OrthoLang.Modules.Load      as Load
import qualified OrthoLang.Modules.ListLike  as ListLike
import qualified OrthoLang.Modules.Math      as Math
import qualified OrthoLang.Modules.Muscle    as Muscle
import qualified OrthoLang.Modules.Permute   as Permute
import qualified OrthoLang.Modules.SeqIO     as SeqIO
import qualified OrthoLang.Modules.Sets      as Sets
import qualified OrthoLang.Modules.Sample    as Sample
import qualified OrthoLang.Modules.Summarize as Summarize
import qualified OrthoLang.Modules.Scores    as Scores
import qualified OrthoLang.Modules.Plots     as Plots
import qualified OrthoLang.Modules.PsiBlast  as PsiBlast
import qualified OrthoLang.Modules.Hmmer     as Hmmer
import qualified OrthoLang.Modules.OrthoFinder as OrthoFinder
import qualified OrthoLang.Modules.Diamond   as Diamond
import qualified OrthoLang.Modules.MMSeqs    as MMSeqs
import qualified OrthoLang.Modules.SonicParanoid as SonicParanoid
import qualified OrthoLang.Modules.OrthoGroups as OrthoGroups
import qualified OrthoLang.Modules.Busco as Busco
import qualified OrthoLang.Modules.Range as Range
import qualified OrthoLang.Modules.SetsTable as SetsTable
import qualified OrthoLang.Modules.AllVsAll as AllVsAll
import qualified OrthoLang.Modules.GreenCut as GreenCut
import qualified OrthoLang.Modules.NewRulesTest as NewRulesTest

modules :: [OrthoLangModule]
modules =
  [ Replace.orthoLangModule
  , Repeat.orthoLangModule

  , Math.orthoLangModule
  , Load.orthoLangModule
  , Sets.orthoLangModule
  , SeqIO.orthoLangModule
  , BioMartR.orthoLangModule
  , BlastDB.orthoLangModule
  , Blast.orthoLangModule
  , BlastHits.orthoLangModule
  , ListLike.orthoLangModule
  , PsiBlast.orthoLangModule
  , CRBBlast.orthoLangModule
  , Hmmer.orthoLangModule
  , BlastRBH.orthoLangModule
  -- , Cheat.orthoLangModule
  , Muscle.orthoLangModule
  , Sample.orthoLangModule
  , Permute.orthoLangModule
  , Summarize.orthoLangModule
  , Scores.orthoLangModule
  , Plots.orthoLangModule
  , OrthoFinder.orthoLangModule
  , Diamond.orthoLangModule
  , MMSeqs.orthoLangModule
  , SonicParanoid.orthoLangModule
  , OrthoGroups.orthoLangModule
  , Busco.orthoLangModule
  , Range.orthoLangModule
  , SetsTable.orthoLangModule
  , AllVsAll.orthoLangModule
  , GreenCut.orthoLangModule
  , NewRulesTest.orthoLangModule
  ]
