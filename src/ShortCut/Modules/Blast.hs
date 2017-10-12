module ShortCut.Modules.Blast
  ( cutModule
  , bht
  , BlastDesc
  , blastDescs
  )
  where

import Development.Shake
import ShortCut.Core.Types

import Data.Scientific             (formatScientific, FPFormat(..))
import ShortCut.Core.Compile.Basic (rSimple, defaultTypeCheck)
import ShortCut.Core.Compile.Map   (rMap)
import ShortCut.Core.Config        (wrappedCmd)
import ShortCut.Core.Debug         (debugTrackWrite, debugAction)
import ShortCut.Core.Paths         (readLit, readPath, fromCutPath, CutPath)
import ShortCut.Modules.BlastDB    (ndb, pdb)
import ShortCut.Modules.SeqIO      (faa, fna)
import System.FilePath             (takeDirectory, takeFileName, (</>))

cutModule :: CutModule
cutModule = CutModule
  { mName = "blast"
  , mFunctions =
    -- TODO remove the ones that don't apply to each fn type!
    -- TODO psiblast, dbiblast, deltablast, rpsblast, rpsblastn?
    map mkBlastFromDb        blastDescs ++
    map mkBlastFromDbEach    blastDescs ++
    map mkBlastFromFa        blastDescs ++
    map mkBlastFromFaEach    blastDescs ++
    map mkBlastFromFaRev     blastDescs ++
    map mkBlastFromFaRevEach blastDescs
  }

-- tsv with these columns:
-- qseqid sseqid pident length mismatch gapopen
-- qstart qend sstart send evalue bitscore
bht :: CutType
bht = CutType
  { tExt  = "bht"
  , tDesc = "tab-separated table of blast hits (outfmt 6)"
  , tShow  = defaultShow
  }

-------------------------------------------
-- new description-based blast functions --
-------------------------------------------

-- TODO need a separate db type for reverse fns?
type BlastDesc =
  ( String  -- name and also system command to call
  , CutType -- query fasta type
  , CutType -- subject type when starting from fasta
  , CutType -- subject type when starting from db
  )

blastDescs :: [BlastDesc]
blastDescs =
  [ ( "blastn", fna, fna, ndb)
  , ( "blastp", faa, faa, pdb)
  , ( "blastx", fna, faa, pdb)
  , ("tblastn", faa, fna, ndb)
  , ("tblastx", fna, fna, ndb)
  ]

----------------
-- *blast*_db --
----------------

mkBlastFromDb :: BlastDesc -> CutFunction
mkBlastFromDb d@(bCmd, qType, _, dbType) = CutFunction
  { fName      = bCmd ++ "_db"
  , fTypeCheck = defaultTypeCheck [num, qType, dbType] bht
  , fFixity    = Prefix
  , fRules     = rMkBlastFromDb d
  }

-- TODO remove tmp?
-- rMkBlastFromDb :: BlastDesc -> RulesFn
rMkBlastFromDb (bCmd, _, _, _) = rSimple $ aMkBlastFromDb bCmd

aMkBlastFromDb :: String -> (CutConfig -> [CutPath] -> Action ())
aMkBlastFromDb bCmd cfg [o, e, q, p] = do
  eStr   <- readLit cfg e'
  prefix <- readPath cfg p'
  let eDec    = formatScientific Fixed Nothing (read eStr) -- format as decimal
      prefix' = fromCutPath cfg prefix
      cDir    = cfgTmpDir cfg </> takeDirectory prefix' -- TODO remove?
      dbg     = if cfgDebug cfg then ["-v"] else []
      args    = [ "-c", bCmd, "-t", cDir, "-q", q', "-d", takeFileName prefix'
                , "-o", o'  , "-e", eDec, "-p"] ++ dbg
  unit $ quietly $ wrappedCmd cfg [o'] [Cwd $ takeDirectory prefix']
                     "parallelblast.py" args
  debugTrackWrite cfg [o'']
  where
    o'  = fromCutPath cfg o
    q'  = fromCutPath cfg q
    p'  = fromCutPath cfg p
    e'  = fromCutPath cfg e
    o'' = debugAction cfg "aMkBlastFromDb" o' [bCmd, e', o', q', p']
aMkBlastFromDb _ _ _ = error $ "bad argument to aMkBlastFromDb"

-------------
-- *blast* --
-------------

mkBlastFromFa :: BlastDesc -> CutFunction
mkBlastFromFa d@(bCmd, qType, sType, _) = CutFunction
  { fName      = bCmd
  , fTypeCheck = defaultTypeCheck [num, qType, sType] bht
  , fFixity    = Prefix
  , fRules     = rMkBlastFromFa d
  }

-- inserts a "makeblastdb" call and reuses the _db compiler from above
rMkBlastFromFa :: BlastDesc -> RulesFn
rMkBlastFromFa d@(_, _, _, dbType) st (CutFun rtn salt deps _ [e, q, s])
  = rules st (CutFun rtn salt deps name1 [e, q, expr])
  where
    rules = fRules $ mkBlastFromDb d
    name1 = fName  $ mkBlastFromDb d
    name2 = "makeblastdb" ++ if dbType == ndb then "_nucl" else "_prot"
    expr  = CutFun dbType salt (depsOf s) name2 [s] -- TODO deps OK?
rMkBlastFromFa _ _ _ = error "bad argument to rMkBlastFromFa"

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

---------------------
-- *blast*_db_each --
---------------------

mkBlastFromDbEach :: BlastDesc -> CutFunction
mkBlastFromDbEach d@(bCmd, qType, _, dbType) = CutFunction
  { fName      = bCmd ++ "_db_each"
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf dbType] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rMkBlastFromDbEach d
  }

rMkBlastFromDbEach :: BlastDesc -> RulesFn
rMkBlastFromDbEach d@(bCmd, _, _, _) = rMap $ aMkBlastFromDb bCmd

------------------
-- *blast*_each --
------------------

mkBlastFromFaEach :: BlastDesc -> CutFunction
mkBlastFromFaEach d@(bCmd, qType, faType, _) = CutFunction
  { fName      = bCmd ++ "_each"
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf faType] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rMkBlastFromFaEach d
  }

-- combination of the two above: insert the makeblastdbcall, then map
rMkBlastFromFaEach :: BlastDesc -> RulesFn
rMkBlastFromFaEach d@(_, _, _, dbType) st (CutFun rtn salt deps _ [e, q, ss])
  = rules st (CutFun rtn salt deps name1 [e, q, ss'])
  where
    rules = rMkBlastFromDbEach d
    ss'   = CutFun (ListOf dbType) salt (depsOf ss) name2 [ss]
    name1 = (fName $ mkBlastFromFa d) ++ "_each"
    name2 = "makeblastdb"
              ++ (if dbType == ndb then "_nucl" else "_prot")
              ++ "_each"
rMkBlastFromFaEach _ _ _ = error "bad argument to rMkBlastFromFaEach"

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
  = rMap revDbAct st editedExpr
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
