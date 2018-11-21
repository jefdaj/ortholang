module ShortCut.Modules.OrthoFinder
  where

-- import Development.Shake
import ShortCut.Core.Types
import ShortCut.Modules.SeqIO (faa)
-- import ShortCut.Modules.Muscle (aln)
-- import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple)
-- import ShortCut.Core.Paths (CutPath, fromCutPath)
-- import ShortCut.Core.Actions (debugA, wrappedCmdWrite, wrappedCmdOut, readLit, readLits, writeLits)
-- import Data.Scientific (formatScientific, FPFormat(..))
-- import Data.List (isPrefixOf, nub, sort)
-- import System.Directory           (createDirectoryIfMissing)
-- import System.FilePath             (takeFileName, (</>))
-- import ShortCut.Core.Compile.Map  (rMap)

cutModule :: CutModule
cutModule = CutModule
  { mName = "OrthoFinder"
  , mDesc = "Inference of orthologs, orthogroups, the rooted species, gene trees and gene duplcation events tree"
  , mTypes = [faa, ofr]
  , mFunctions = []
  }

ofr :: CutType
ofr = CutType
  { tExt  = "ofr"
  , tDesc = "OrthoFinder results"
  , tShow = \_ _ f -> return $ "OrthoFinder results '" ++ f ++ "'"
  }


