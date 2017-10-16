module ShortCut.Modules.Blast
  ( cutModule
  , bht
  -- the rest is for blastrbh, which is pretty intimately related:
  , BlastDesc
  , blastDescs
  , mkBlastFromFa
  , aMkBlastFromDb
  )
  where

import Development.Shake
import ShortCut.Core.Types

import Data.Scientific             (formatScientific, FPFormat(..))
import ShortCut.Core.Compile.Basic (rSimple, defaultTypeCheck)
import ShortCut.Core.Compile.Each   (rEach)
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
    map mkBlastFromFa     blastDescs ++
    map mkBlastFromFaEach blastDescs ++
    map mkBlastFromDb     blastDescs ++
    map mkBlastFromDbEach blastDescs
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
rMkBlastFromDb :: BlastDesc -> RulesFn
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
rMkBlastFromDbEach (bCmd, _, _, _) = rEach $ aMkBlastFromDb bCmd

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
