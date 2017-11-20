module ShortCut.Modules.BlastRBH where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Compile.Basic (rExpr, rSimple, defaultTypeCheck)
import ShortCut.Core.Compile.Each  (rEach)
import ShortCut.Core.Actions       (wrappedCmd, debugTrackWrite)
import ShortCut.Core.Debug         (debugAction)
import ShortCut.Core.Paths         (CutPath, fromCutPath)
import ShortCut.Modules.Blast      (bht, BlastDesc, blastDescs, mkBlastFromFa,
                                    aMkBlastFromDb)
import ShortCut.Modules.BlastDB    (ndb, pdb)
import ShortCut.Modules.SeqIO      (faa)

-- TODO should the _rev functions also be moved here?
-- TODO test each one: first all the peices, then together

cutModule :: CutModule
cutModule = CutModule
  { mName = "blastrbh"
  , mFunctions =
    -- TODO also work with the non-symmetric ones that have an obvious way to do it?
    map mkBlastFromFaRev     blastDescsRev ++
    map mkBlastFromFaRevEach blastDescsRev ++

    [reciprocalBest, reciprocalBestEach] ++

    map mkBlastRbh     blastDescsRev ++
    map mkBlastRbhEach blastDescsRev
  }

-- note that this just filters. it doesn't "reverse" the descriptions
blastDescsRev :: [BlastDesc]
blastDescsRev = filter isReversible blastDescs
  where
    isReversible (_, qType, sType, _) = qType == sType

-----------------
-- *blast*_rev --
-----------------

mkBlastFromFaRev :: BlastDesc -> CutFunction
mkBlastFromFaRev d@(bCmd, qType, sType, _) = CutFunction
  { fName      = bCmd ++ "_rev"
  , fTypeCheck = defaultTypeCheck [num, sType, qType] bht
  , fFixity    = Prefix
  , fRules     = rMkBlastFromFaRev d
  }

-- flips the query and subject arguments and reuses the regular compiler above
rMkBlastFromFaRev :: BlastDesc -> RulesFn
rMkBlastFromFaRev d st (CutFun rtn salt deps _ [e, q, s])
  = rules st (CutFun rtn salt deps name [e, s, q])
  where
    rules = fRules $ mkBlastFromFa d
    name  = fName  $ mkBlastFromFa d
rMkBlastFromFaRev _ _ _ = error "bad argument to rMkBlastFromFaRev"

----------------------
-- *blast*_rev_each --
----------------------

mkBlastFromFaRevEach :: BlastDesc -> CutFunction
mkBlastFromFaRevEach d@(bCmd, sType, qType, _) = CutFunction
  { fName      = bCmd ++ "_rev_each"
  , fTypeCheck = defaultTypeCheck [num, sType, ListOf qType] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rMkBlastFromFaRevEach d
  }

-- The most confusing one! Edits the expression to make the subject into a db,
-- and the action fn to take the query and subject flipped, then maps the new
-- expression over the new action fn.
-- TODO check if all this is right, since it's confusing!
rMkBlastFromFaRevEach :: BlastDesc -> RulesFn
rMkBlastFromFaRevEach (bCmd, qType, _, _) st (CutFun rtn salt deps _ [e, s, qs])
  = rEach revDbAct st editedExpr
  where
    revDbAct   = aMkBlastFromDbRev bCmd
    subjDbExpr = CutFun dbType salt (depsOf s) dbFnName [s]
    editedExpr = CutFun rtn salt deps editedName [e, subjDbExpr, qs]
    editedName = bCmd ++ "_db_rev_each"
    (dbFnName, dbType) = if qType == faa
                           then ("makeblastdb_prot", pdb)
                           else ("makeblastdb_nucl", ndb)
rMkBlastFromFaRevEach _ _ _ = error "bad argument to rMkBlastFromFaRevEach"

-- TODO which blast commands make sense with this?
aMkBlastFromDbRev :: String -> (CutConfig -> [CutPath] -> Action ())
aMkBlastFromDbRev bCmd cfg [oPath, eValue, dbPrefix, queryFa] =
  aMkBlastFromDb  bCmd cfg [oPath, eValue, queryFa, dbPrefix]
aMkBlastFromDbRev _ _ _ = error "bad argument to aMkBlastFromDbRev"

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
  , fRules     = rEach aReciprocalBest
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
    rHits = CutFun (ListOf bht) salt deps (bCmd ++ "_rev_each")  [e, l, rs]
rMkBlastRbhEach _ _ _ = error "bad argument to rMkBlastRbh"
