module ShortCut.Modules.SonicParanoid
  where

-- import Development.Shake
import ShortCut.Core.Types

import ShortCut.Modules.SeqIO      (fna, faa)

cutModule :: CutModule
cutModule = CutModule
  { mName = "SonicParanoid"
  , mDesc = "Very fast, accurate, and easy orthology."
  , mTypes = [faa, fna, spr]
  , mFunctions =
      [
      ]
  }

spr :: CutType
spr = CutType
  { tExt  = "spr"
  , tDesc = "SonicParanoid results"
  , tShow = undefined
  -- , tShow = \_ ref path -> do
  --     txt <- readFileStrict ref path
  --     return $ unlines $ take 17 $ lines txt
  }


