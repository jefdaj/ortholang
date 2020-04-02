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
import qualified OrthoLang.Modules.Singletons    as Singletons
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
  , Summarize.olModule
  , OrthoFinder.olModule
  , Singletons.olModule

  -- core language features
  , ListLike.olModule -- TODO expose Function fields in API
  , Load.olModule     -- TODO glob_files first, then mkGlob after Compose
  , Math.olModule
  , Permute.olModule  -- could do rPermute
  , Range.olModule    -- could do rRange
  , Repeat.olModule   -- implemented in terms of rReplaceEach
  , Replace.olModule  -- implement as Script -> Script, then normal?
  , Sample.olModule   -- could do rSample
  , Scores.olModule   -- looks semi complicated to convert
  , Sets.olModule     -- could do it same as math?

  -- load, download, and convert sequences
  , BioMartR.olModule -- rewrite without the parse searches thing
  , SeqIO.olModule    -- mix of rSimple* and rMap* functions

  -- BLAST
  , Blast.olModule      -- surprisingly, looks doable via rSimple
  , BlastDB.olModule    -- could do some, some will require expr transforms too
  , BlastHits.olModule  -- simplest case of rMap use? aCutCol etc.
  , PsiBlast.olModule   -- complicated :(

  -- BLAST reciprocal best hits
  , BlastRBH.olModule -- fairly simple but some minor rMap
  , CRBBlast.olModule -- rSimpleTmp, rMapTmps

  -- other sequence search programs
  , Diamond.olModule -- long, and uses rSimple + rMap
  , Hmmer.olModule   -- short, uses rSimple + rMap
  , MMSeqs.olModule  -- custom Actions, but Rules look reasonably easy
  , Muscle.olModule  -- very short use of rSimple

  -- orthogroup search programs
  , Busco.olModule         -- medium complicated
  , OrthoGroups.olModule   -- complicated str code, but simple Rules?
  , SonicParanoid.olModule -- short and simple Rules

  -- plots and figures
  , Plots.olModule     -- somewhat complicated Rules... Script transform?
  , SetsTable.olModule -- implemented in terms of plots

  ]
