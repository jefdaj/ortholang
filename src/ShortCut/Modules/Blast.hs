module ShortCut.Modules.Blast
  ( cutModule
  , bht
  , BlastDesc
  , blastDescs
  )
  where

import Development.Shake
import ShortCut.Core.Types

import Data.Scientific          (formatScientific, FPFormat(..))
import ShortCut.Core.Config     (wrappedCmd)
import ShortCut.Core.Paths      (readLit, readPath, fromCutPath, CutPath)
import ShortCut.Core.Debug      (debugTrackWrite, debug, debugAction)
import ShortCut.Core.Compile.Basic      (rSimpleTmp, defaultTypeCheck)
import ShortCut.Core.Compile.Map      (rMapTmp)
import ShortCut.Modules.BlastDB (ndb, pdb)
import ShortCut.Modules.SeqIO   (faa, fna)
import System.FilePath          (takeDirectory, takeFileName, (</>))
import Text.PrettyPrint.HughesPJClass

cutModule :: CutModule
cutModule = CutModule
  { mName = "blast"
  , mFunctions =
    -- old functions to replace:
    -- [ oldMkBlastFn        "blastn" fna fna ndb -- TODO why doesn't this one work??
    -- , oldMkBlastFn        "blastp" faa faa pdb
    -- , oldMkBlastFn        "blastx" fna faa pdb
    -- , oldMkBlastFn       "tblastn" faa fna ndb
    -- , oldMkBlastFn       "tblastx" fna fna ndb
    -- , oldMkBlastDb      "blastn" fna ndb -- TODO why doesn't this one work??
    -- , oldMkBlastDb      "blastp" faa pdb
    -- , oldMkBlastDb      "blastx" fna pdb
    -- , oldMkBlastDb     "tblastn" faa ndb
    -- , oldMkBlastDb     "tblastx" fna ndb
    [ oldMkBlastEachFn    "blastn" fna fna ndb -- TODO why doesn't this one work??
    , oldMkBlastEachFn    "blastp" faa faa pdb
    , oldMkBlastEachFn    "blastx" fna faa pdb
    , oldMkBlastEachFn   "tblastn" faa fna ndb
    , oldMkBlastEachFn   "tblastx" fna fna ndb
    , oldMkBlastEachRevFn "blastn" fna fna ndb -- TODO don't expose to users?
    , oldMkBlastEachRevFn "blastp" faa faa pdb -- TODO don't expose to users?
    -- TODO use the reverse each ones?
    -- TODO psiblast, dbiblast, deltablast, rpsblast, rpsblastn?
    ] ++

    -- new description-based functions:
    -- TODO remove the ones that don't apply to each fn type!
    map mkBlastFromDb     blastDescs ++
    map mkBlastFromFa     blastDescs ++
    map mkBlastRev  blastDescs ++
    map newMkBlastEach    blastDescs ++
    map newMkBlastRevEach blastDescs
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
mkBlastFromDb (bCmd, qType, _, dbType) = CutFunction
  { fName      = bCmd ++ "_db"
  , fTypeCheck = defaultTypeCheck [qType, dbType, num] bht
  , fFixity    = Prefix
  , fRules     = rSimpleTmp (aBlastFromDb bCmd) "blast" bht
  }

aBlastFromDb :: String -> (CutConfig -> CutPath -> [CutPath] -> Action ())
aBlastFromDb bCmd cfg _ paths@[o, q, p, e] = do
  -- liftIO $ putStrLn $ "aBlastFromDb args: " ++ show paths
  eStr   <- readLit cfg e'
  -- TODO why does this have the complete dna sequence in it when using tblastn_each??
  prefix <- readPath cfg p'
  let eDec    = formatScientific Fixed Nothing (read eStr) -- format as decimal
      prefix' = fromCutPath cfg prefix
      cDir    = cfgTmpDir cfg </> takeDirectory prefix'
      dbg     = if cfgDebug cfg then ["-v"] else []
      args    = [ "-c", bCmd, "-t", cDir, "-q", q', "-d", takeFileName prefix'
                , "-o", o'  , "-e", eDec, "-p"] ++ dbg
  -- liftIO $ putStrLn $ "prefix: " ++ show prefix
  -- liftIO $ putStrLn $ "prefix': " ++ prefix'
  unit $ quietly $ wrappedCmd cfg [o'] [Cwd $ takeDirectory prefix']
                     "parallelblast.py" args
  debugTrackWrite cfg [o'']
  where
    o'  = fromCutPath cfg o
    q'  = fromCutPath cfg q
    p'  = fromCutPath cfg p
    e'  = fromCutPath cfg e
    o'' = debugAction cfg "aBlastFromDb" o' [bCmd, o', q', p', e']
aBlastFromDb _ _ _ _ = error $ "bad argument to aBlastFromDb"

-------------
-- *blast* --
-------------

mkBlastFromFa :: BlastDesc -> CutFunction
mkBlastFromFa d@(bCmd, qType, sType, _) = CutFunction
  { fName      = bCmd
  , fTypeCheck = defaultTypeCheck [qType, sType, num] bht
  , fFixity    = Prefix
  , fRules     = rMkBlastFromFa d
  }

-- inserts a "makeblastdb" call and reuses the _db compiler from above
rMkBlastFromFa :: BlastDesc -> RulesFn
rMkBlastFromFa d@(_, _, _, dbType) st (CutFun rtn salt deps name [q, s, e])
  = fRules (mkBlastFromDb d) st (CutFun rtn salt deps name [q, dbExpr, e])
  where
    dbName = "makeblastdb" ++ if dbType == ndb then "_nucl" else "_prot"
    dbExpr = CutFun dbType salt [] dbName [s] -- TODO deps OK?
rMkBlastFromFa _ _ _ = error "bad argument to rMkBlastFromFa"

-----------------
-- *blast*_rev --
-----------------

mkBlastRev :: BlastDesc -> CutFunction
mkBlastRev d@(bCmd, qType, sType, _) = CutFunction
  { fName      = bCmd ++ "_rev"
  , fTypeCheck = defaultTypeCheck [sType, qType, num] bht
  , fFixity    = Prefix
  , fRules     = rMkBlastRev d
  }

-- flips the query and subject arguments and reuses the regular compiler above
rMkBlastRev :: BlastDesc -> RulesFn
rMkBlastRev d st (CutFun rtn salt deps name [q, s, e])
  = rulesFn   st (CutFun rtn salt deps name [s, q, e])
  where
    rulesFn = fRules $ mkBlastFromFa d
rMkBlastRev _ _ _ = error "bad argument to rMkBlastRev"

-- TODO how to do rev_each? db_each? _db_rev_each/_rev_db_each?
-- *blast*_each
-- *blast*_db_each
newMkBlastEach :: BlastDesc -> CutFunction
newMkBlastEach (bCmd, qType, _, dbType) = CutFunction
  { fName      = "new_" ++ bCmd ++ "_each"
  , fTypeCheck = defaultTypeCheck [qType, dbType, num] bht
  , fFixity    = Prefix
  , fRules     = undefined
  }

-- *blast*_rev_each
newMkBlastRevEach :: BlastDesc -> CutFunction
newMkBlastRevEach (bCmd, qType, _, dbType) = CutFunction
  { fName      = "new_" ++ bCmd ++ "_each"
  , fTypeCheck = defaultTypeCheck [qType, dbType, num] bht
  , fFixity    = Prefix
  , fRules     = undefined
  }

---------------------------------
-- (old) basic blast+ commands --
---------------------------------

-- as a quick kludge, duplicated this and rearranged the args
-- TODO validation function so I can't mess up constructing these by hand? aha! write strings + parse normally!
oldAddMakeDBCall2 :: CutExpr -> CutType -> CutExpr
oldAddMakeDBCall2 (CutFun r i ds n [e, q, ss]) dbType = CutFun r i ds n [e, q, dbs]
  where
    -- dbType = if typeOf s `elem` [fna, ListOf fna] then ndb else pdb -- TODO maybe it's (ListOf fna)?
    dbs = CutFun (ListOf dbType) i (depsOf ss) name [ss]
    name = "makeblastdb" ++ (if dbType == ndb then "_nucl" else "_prot") ++ "_each"
oldAddMakeDBCall2 _ _ = error "bad argument to oldAddMakeDBCall2"

-- TODO remove the old bbtmp default tmpDir
aOldParBlast :: String -> (CutConfig -> CutPath -> [CutPath] -> Action ())
aOldParBlast bCmd cfg _ paths@[o, q, p, e] = do
  liftIO $ putStrLn $ "aOldParBlast args: " ++ show paths
  eStr   <- readLit cfg e'
  -- TODO why does this have the complete dna sequence in it when using tblastn_each??
  prefix <- readPath cfg p'
  let eDec    = formatScientific Fixed Nothing (read eStr) -- format as decimal
      prefix' = fromCutPath cfg prefix
      cDir    = cfgTmpDir cfg </> takeDirectory prefix'
      dbg     = if cfgDebug cfg then ["-v"] else []
      args    = [ "-c", bCmd, "-t", cDir, "-q", q', "-d", takeFileName prefix'
                , "-o", o'  , "-e", eDec, "-p"] ++ dbg
  liftIO $ putStrLn $ "prefix: " ++ show prefix
  liftIO $ putStrLn $ "prefix': " ++ prefix'
  unit $ quietly $ wrappedCmd cfg [o'] [Cwd $ takeDirectory prefix']
                     "parallelblast.py" args
  debugTrackWrite cfg [o'']
  where
    o'  = fromCutPath cfg o
    q'  = fromCutPath cfg q
    p'  = fromCutPath cfg p
    e'  = fromCutPath cfg e
    o'' = debugAction cfg "aOldParBlast" o' [bCmd, o', q', p', e']
aOldParBlast _ _ _ _ = error $ "bad argument to aOldParBlast"

---------------------------
-- (old) mapped versions --
---------------------------

-- TODO gotta have a variation for "not the last arg"
oldMkBlastEachFn :: String -> CutType -> CutType -> CutType -> CutFunction
oldMkBlastEachFn bCmd qType sType dbType = CutFunction
  { fName      = bCmd ++ "_each"
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf sType] (ListOf bht)
  , fFixity    = Prefix
  , fRules  = rOldMkBlastEach bCmd dbType aOldParBlast
  }

-- TODO need to apply oldAddMakeDBCall2 *after* mapping over the last arg
-- TODO more specific tmpDir?
rOldMkBlastEach :: String -> CutType -> (String -> (CutConfig -> CutPath -> [CutPath] -> Action ())) -> RulesFn
rOldMkBlastEach bCmd dbType bActFn st@(_,cfg) expr = mapFn st $ oldAddMakeDBCall2 expr' dbType
  where
    mapFn = rMapTmp (bActFn' bCmd) (bCmd ++ "_each") bCmd
    expr' = debug cfg ("rOldMkBlastEach expr: '" ++ render (pPrint expr) ++ "'") expr
    -- kludge to allow easy mapping over the subject rather than evalue:
    -- TODO is this right?
    -- TODO can it be changed to keep the evalues at the end like expected?
    -- TODO are the e and q getting reversed? they look OK here
    bActFn' b c d [o, e, q, s] = let args = [o, q, s, e]; args' = debug cfg ("bActFn args: " ++ show args) args in bActFn b c d args'
    bActFn' _ _ _ _ = error "bad argument to bActFn'"

-----------------------------------------------------------------
-- (old) "reverse" versions to help write reciprocal best hits --
-----------------------------------------------------------------

-- TODO move to BlastRBH module?

-- note: only works on symmetric blast fns (take two of the same type)
-- oldMkBlastRevFn :: String -> CutType -> CutType -> CutType -> CutFunction
-- oldMkBlastRevFn bCmd qType sType dbType = CutFunction
--   { fName      = bCmd ++ "_rev"
--   , fTypeCheck = defaultTypeCheck [qType, sType, num] bht
--   , fFixity    = Prefix
--   , fRules  = rOldMkBlastFn bCmd dbType aOldParBlastRev
--   }

-- just switches the query and subject, which won't work for asymmetric blast fns!
-- TODO write specific ones for that, or a fn + mapping
-- TODO debug transformations too!
aOldParBlastRev :: String -> (CutConfig -> CutPath -> [CutPath] -> Action ())
aOldParBlastRev b c d [o, q, s, e] = aOldParBlast b c d [o, s, q, e]
aOldParBlastRev _ _ _ args = error $ "bad argument to aOldParBlast: " ++ show args

-- TODO gotta have a variation for "not the last arg"
oldMkBlastEachRevFn :: String -> CutType -> CutType -> CutType -> CutFunction
oldMkBlastEachRevFn bCmd qType sType dbType = CutFunction
  { fName      = bCmd ++ "_each_rev"
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf sType] bht
  , fFixity    = Prefix
  , fRules  = rOldMkBlastEach bCmd dbType aOldParBlastRev
  }
