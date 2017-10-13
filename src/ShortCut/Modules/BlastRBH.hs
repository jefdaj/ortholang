module ShortCut.Modules.BlastRBH where

import Development.Shake
import ShortCut.Core.Types

import Control.Monad.Trans         (liftIO)
import ShortCut.Core.Compile.Basic (rExpr, rSimple, defaultTypeCheck)
import ShortCut.Core.Compile.Map   (rMap)
import ShortCut.Core.Config        (wrappedCmd)
import ShortCut.Core.Debug         (debugTrackWrite, debugAction)
import ShortCut.Core.Paths         (exprPath, cacheDir, CutPath, toCutPath,
                                    fromCutPath)
import ShortCut.Modules.Blast      (bht, BlastDesc, blastDescs)
import ShortCut.Modules.SeqIO      (faa, fna)
import System.Directory            (createDirectoryIfMissing)

-- TODO should the _rev functions also be moved here?
-- TODO test each one: first all the peices, then together

cutModule :: CutModule
cutModule = CutModule
  { mName = "blastrbh"
  , mFunctions =
    [ reciprocalBest
    , reciprocalBestEach
    ]
    -- TODO filter for the ones that make sense in _rbh form!
    ++ map mkBlastRbh     blastDescs
    ++ map mkBlastRbhEach blastDescs
  }

---------------------
-- reciprocal_best --
---------------------

-- TODO move to Tables.hs? And rename that to BlastHits?

reciprocalBest :: CutFunction
reciprocalBest = CutFunction
  { fName      = "reciprocal_best"
  , fTypeCheck = defaultTypeCheck [bht, bht] bht
  , fFixity    = Prefix
  , fRules     = rSimple aReciprocalBest
  }

-- TODO how are $TMPDIR paths getting through after conversion from cutpaths??
aReciprocalBest :: CutConfig -> [CutPath] -> Action ()
aReciprocalBest cfg [out, left, right] = do
  unit $ quietly $ wrappedCmd cfg [out'] [] "reciprocal_best.R" [out', left', right']
  debugTrackWrite cfg [out'']
  where
    out'   = fromCutPath cfg out
    left'  = fromCutPath cfg left
    right' = fromCutPath cfg right
    out''  = debugAction cfg "aReciprocalBest" out' [out', left', right']
aReciprocalBest _ args = error $ "bad argument to aReciprocalBest: " ++ show args

--------------------------
-- reciprocal_best_each --
--------------------------

reciprocalBestEach :: CutFunction
reciprocalBestEach = CutFunction
  { fName      = "reciprocal_best_each"
  , fTypeCheck = defaultTypeCheck [bht, ListOf bht] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rMap aReciprocalBest
  }

-----------------
-- *blast*_rbh --
-----------------

mkBlastRbh :: BlastDesc -> CutFunction
mkBlastRbh d@(bCmd, qType, sType, _) = CutFunction
  { fName      = bCmd ++ "_rbh"
  , fTypeCheck = defaultTypeCheck [num, qType, sType] bht
  , fFixity    = Prefix
  , fRules     = rMkBlastRbh d
  }

-- TODO this only works with symmetric fns so far... either fix or restrict to those!
rMkBlastRbh :: BlastDesc -> RulesFn
rMkBlastRbh (bCmd, _, _, _) s (CutFun _ salt deps _ [e, l, r]) = rExpr s main
  where
    main  = CutFun bht salt deps "reciprocal_best" [lHits, rHits]
    lHits = CutFun bht salt deps  bCmd            [e, l, r]
    rHits = CutFun bht salt deps (bCmd ++ "_rev") [e, l, r]
rMkBlastRbh _ _ _ = error "bad argument to rMkBlastRbh"

----------------------
-- *blast*_rbh_each --
----------------------

mkBlastRbhEach :: BlastDesc -> CutFunction
mkBlastRbhEach d@(bCmd, qType, sType, _) = CutFunction
  { fName      = bCmd ++ "_rbh_each"
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf sType] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rMkBlastRbhEach d
  }

rMkBlastRbhEach :: BlastDesc -> RulesFn
rMkBlastRbhEach (bCmd, _, _, _) s (CutFun _ salt deps _ [e, l, rs]) = rExpr s main
  where
    main  = CutFun (ListOf bht) salt deps "reciprocal_best_each" [lHits, rHits]
    lHits = CutFun (ListOf bht) salt deps (bCmd ++ "_each"    )  [e, l, rs]
    rHits = CutFun (ListOf bht) salt deps (bCmd ++ "_each_rev")  [e, l, rs]
rMkBlastRbhEach _ _ _ = error "bad argument to rMkBlastRbh"
