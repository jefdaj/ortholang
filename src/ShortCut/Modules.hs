module ShortCut.Modules where

import ShortCut.Core.Types (CutModule(..))

import qualified ShortCut.Modules.BioMartR  as BioMartR
import qualified ShortCut.Modules.Blast     as Blast
import qualified ShortCut.Modules.BlastDB   as BlastDB
import qualified ShortCut.Modules.BlastCRB  as BlastCRB
import qualified ShortCut.Modules.Glob      as Glob
import qualified ShortCut.Modules.Cheat     as Cheat -- TODO write this
import qualified ShortCut.Modules.Length    as Length
import qualified ShortCut.Modules.Math      as Math
import qualified ShortCut.Modules.Permute   as Permute
import qualified ShortCut.Modules.Repeat    as Repeat
import qualified ShortCut.Modules.Summarize as Summarize
import qualified ShortCut.Modules.Sets      as Sets
import qualified ShortCut.Modules.SeqIO     as SeqIO
import qualified ShortCut.Modules.Tables    as Tables

modules :: [CutModule]
modules =
  [ BioMartR.cutModule
  , Blast.cutModule
  , BlastDB.cutModule
  , BlastCRB.cutModule
  , Glob.cutModule
  , Cheat.cutModule
  , Length.cutModule
  , Math.cutModule
  , Permute.cutModule
  , Repeat.cutModule
  , Summarize.cutModule
  , Sets.cutModule
  , SeqIO.cutModule
  , Tables.cutModule
  ]
