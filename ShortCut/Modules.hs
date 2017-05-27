module ShortCut.Modules where

import ShortCut.Core.Types (CutModule(..))

import qualified ShortCut.Modules.BioMartR  as BioMartR
import qualified ShortCut.Modules.Blast     as Blast -- TODO replace with shmlast
import qualified ShortCut.Modules.Cheat     as Cheat -- TODO write this
import qualified ShortCut.Modules.Math      as Math
import qualified ShortCut.Modules.Permute   as Permute
import qualified ShortCut.Modules.Sets      as Sets
import qualified ShortCut.Modules.Summarize as Summarize

modules :: [CutModule]
modules =
  [ BioMartR.cutModule
  , Blast.cutModule
  , Cheat.cutModule
  , Math.cutModule
  , Permute.cutModule
  , Sets.cutModule
  , Summarize.cutModule
  ]
