module ShortCut.Modules.Blast where

-- TODO make a separate module for the reciprocal stuff?
-- TODO remove the rest of the evalue machinery from rBlast once filter_evalue works!
-- TODO write/find filter_evalue.R

import Development.Shake
import ShortCut.Core.Types

import Data.Scientific          (formatScientific, FPFormat(..))
import ShortCut.Core.Config     (wrappedCmd)
import ShortCut.Core.Debug      (debugReadFile, debugTrackWrite)
import ShortCut.Core.ModuleAPI  (rSimpleTmp, rMapLastTmp, defaultTypeCheck)
import ShortCut.Modules.BlastDB (bdb)
import ShortCut.Modules.SeqIO   (faa, fna)

cutModule :: CutModule
cutModule = CutModule
  { mName = "blast"
  , mFunctions =
    [ mkBlastFn  "blastn" fna fna -- TODO why doesn't this one work??
    , mkBlastFn  "blastp" faa faa
    , mkBlastFn  "blastx" fna faa
    , mkBlastFn "tblastn" faa fna
    , mkBlastFn "tblastx" fna fna
    , filterEvalue
    , bestHits
    , mkBlastEachFn  "blastn" fna fna -- TODO why doesn't this one work??
    , mkBlastEachFn  "blastp" faa faa
    , mkBlastEachFn  "blastx" fna faa
    , mkBlastEachFn "tblastn" faa fna
    , mkBlastEachFn "tblastx" fna fna
    , mkBlastEachRevFn "blastn" fna fna -- TODO don't expose to users?
    , mkBlastEachRevFn "blastp" faa faa -- TODO don't expose to users?
    -- TODO psiblast, dbiblast, deltablast, rpsblast, rpsblastn?
    ]
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

---------------------------
-- basic blast+ commands --
---------------------------

mkBlastFn :: String -> CutType -> CutType -> CutFunction
mkBlastFn bCmd qType sType = CutFunction
  { fName      = bCmd
  , fTypeCheck = defaultTypeCheck [qType, sType, num] bht
  , fFixity    = Prefix
  -- , fCompiler  = rSimpleTmp (aParBlast bCmd sType) "blast" bht
  , fCompiler  = cMkBlast2 bCmd aParBlast
  }

-- TODO wait, can this be done as just a transformation of the AST + an action fn?
--      (that is, no Rules monad needed?)
--      if so, can also write the _each version the same way!!
-- cMkBlast :: String
--          -> (String -> ActionFn)
--          -> RulesFn
-- cMkBlast bCmd bActFn st@(_,cfg) e@(CutFun _ _ _ _ [q, s, n]) = do
--   qPath  <- cExpr st q
--   dbPath <- cExpr st $ CutFun bdb (saltOf s) (depsOf s) "makeblastdb" [s]
--   nPath  <- cExpr st n
--   let (ExprPath htPath) = exprPath cfg e []
--       tmpDir = cacheDir cfg "blast"
--   htPath %> \_ -> bActFn bCmd cfg tmpDir [ExprPath htPath, qPath, dbPath, nPath]
--   return (ExprPath htPath)
-- cMkBlast _ _ _ _ = error "bad argument to cMkBlast"

cMkBlast2 :: String -> (String -> ActionFn) -> RulesFn
cMkBlast2 bCmd bActFn st expr = -- (CutFun rtn salt deps name [q, s, n]) = -- TODO is arg order right?
  rSimpleTmp (bActFn bCmd) "blast" bht st $ addMakeDBCall expr
  -- where
    -- db = CutFun bdb salt (depsOf s) "makeblastdb" [s]
    -- e' = CutFun rtn salt deps name [q, db, n] -- TODO is it confusing to keep the same name?
-- cMkBlast2 _ _ _ _ = error "bad argument to cMkBlast2"

addMakeDBCall :: CutExpr -> CutExpr
addMakeDBCall (CutFun r i ds n [q, s, e]) = CutFun r i ds n [q, db, e]
  where
    db = CutFun bdb i (depsOf s) "makeblastdb" [s]
addMakeDBCall _ = error "bad argument to addMakeDBCall"

cMkBlastEach :: String -> (String -> ActionFn) -> RulesFn
cMkBlastEach bCmd bActFn st expr = mapFn st $ addMakeDBCall expr
  where
    mapFn = rMapLastTmp (bActFn bCmd) "blast" bht

-- parallelblast.py also has a function for this, but after I wrote that I
-- discovered the race condition where more than one instance of it tries to
-- create the db at once. Creating it first with Shake first prevents it.
-- aBlastDB sType cfg [ExprPath dbPath, ExprPath sPath] = do
-- aBlastDB _ _ _ = error "bad argument to aBlastDB"
-- aBlastDB :: CutType -> CutConfig -> ExprPath -> Action ExprPath
-- aBlastDB sType cfg (ExprPath sPath) = do
--   need [sPath]
--   sDigest <- fmap digest $ debugReadFile cfg sPath
--   -- this should match find_db in parallelblast.py:
--   let dbPath = cDir </> (sDigest ++ "_" ++ dbType)
--   unit $ quietly $ wrappedCmd cfg [Cwd cDir] "makeblastdb"
--     [ "-in"    , sPath
--     , "-out"   , dbPath
--     , "-title" , takeBaseName dbPath
--     , "-dbtype", dbType
--     ]
--   debugTrackWrite cfg [dbPath]
--   return (ExprPath dbPath)
--   where
--     (CacheDir cDir) = cacheDir cfg "blast"
--     dbType = if      sType == fna then "nucl"
--              else if sType == faa then "prot"
--              else    error $ "invalid FASTA type for aBlastDB: " ++ show sType

aParBlast :: String -> ActionFn
aParBlast bCmd cfg (CacheDir cDir)
          [ExprPath out, ExprPath query, ExprPath db, ExprPath evalue] = do
  -- TODO is this automatic? need [query, subject, ePath]
  eStr <- fmap init $ debugReadFile cfg evalue
  let eDec = formatScientific Fixed Nothing (read eStr) -- format as decimal
  unit $ quietly $ wrappedCmd cfg [] "parallelblast.py" -- TODO Cwd cDir?
    [ "-c", bCmd, "-t", cDir, "-q", query, "-d", db, "-o", out, "-e", eDec, "-p"]
  debugTrackWrite cfg [out]
aParBlast _ _ _ args = error $ "bad argument to aParBlast: " ++ show args

-----------------------------------------------------------
-- "reverse" versions to help write reciprocal best hits --
-----------------------------------------------------------

-- TODO remove these if you don't end up using them

-- note: only works on symmetric blast fns (take two of the same type)
mkBlastRevFn :: String -> CutType -> CutType -> CutFunction
mkBlastRevFn bCmd qType sType = CutFunction
  { fName      = bCmd ++ "_rev"
  , fTypeCheck = defaultTypeCheck [qType, sType, num] bht
  , fFixity    = Prefix
  -- , fCompiler  = rSimpleTmp (aParBlastRev bCmd sType) "blast" bht
  , fCompiler  = cMkBlast2 bCmd aParBlastRev
  }

-- just switches the query and subject, which won't work for asymmetric blast fns!
-- TODO write specific ones for that, or a fn + mapping
aParBlastRev :: String -> ActionFn
aParBlastRev b c d [o, q, s, e] = aParBlast b c d [o, s, q, e]
aParBlastRev _ _ _ args = error $ "bad argument to aParBlast: " ++ show args

---------------------------
-- filter hits by evalue --
---------------------------

filterEvalue :: CutFunction
filterEvalue = CutFunction
  { fName      = "filter_evalue"
  , fTypeCheck = defaultTypeCheck [num, bht] bht
  , fFixity    = Prefix
  , fCompiler  = rSimpleTmp aFilterEvalue "blast" bht
  }

aFilterEvalue :: ActionFn
aFilterEvalue cfg (CacheDir tmp) [ExprPath out, ExprPath evalue, ExprPath hits] = do
  unit $ quietly $ wrappedCmd cfg [Cwd tmp] "filter_evalue.R" [out, evalue, hits]
aFilterEvalue _ _ args = error $ "bad argument to aFilterEvalue: " ++ show args

-------------------------------
-- get the best hit per gene --
-------------------------------

bestHits :: CutFunction
bestHits = CutFunction
  { fName      = "best_hits"
  , fTypeCheck = defaultTypeCheck [bht] bht
  , fFixity    = Prefix
  , fCompiler  = rSimpleTmp aBestHits "blast" bht
  }

aBestHits :: ActionFn
aBestHits cfg (CacheDir tmp) [ExprPath out, ExprPath hits] = do
  unit $ quietly $ wrappedCmd cfg [Cwd tmp] "best_hits.R" [out, hits]
aBestHits _ _ args = error $ "bad argument to aBestHits: " ++ show args

----------------------------------
-- mapped versions of blast fns --
----------------------------------

-- TODO gotta have a variation for "not the last arg"
mkBlastEachFn :: String -> CutType -> CutType -> CutFunction
mkBlastEachFn bCmd qType sType = CutFunction
  { fName      = bCmd ++ "_each"
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf sType] (ListOf bht)
  , fFixity    = Prefix
  -- , fCompiler  = rMapLastTmp (aParBlast' bCmd sType) "blast" bht
  , fCompiler  = cMkBlastEach bCmd aParBlast'
  }

-- kludge to allow easy mapping over the subject rather than evalue
-- TODO have a version of the map functions that does this on its own
aParBlast' :: String -> ActionFn
aParBlast' bCmd cfg (CacheDir cDir) [ExprPath o, ExprPath e, ExprPath q, ExprPath s] =
 aParBlast bCmd cfg (CacheDir cDir) [ExprPath o, ExprPath q, ExprPath s, ExprPath e]
aParBlast' _ _ _ args = error $ "bad argument to aParBlast': " ++ show args

---------------------------------------------------------------------
-- reverse mapped versions (needed for mapped reciprocal versions) --
---------------------------------------------------------------------

-- TODO move to BlastRBH module?

-- TODO gotta have a variation for "not the last arg"
mkBlastEachRevFn :: String -> CutType -> CutType -> CutFunction
mkBlastEachRevFn bCmd qType sType = CutFunction
  { fName      = bCmd ++ "_each_rev"
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf sType] bht
  , fFixity    = Prefix
  -- , fCompiler  = rMapLastTmp (aParBlastRev' bCmd sType) "blast" bht
  , fCompiler  = cMkBlastEach bCmd aParBlastRev'
  }

-- kludge to allow easy mapping over the subject rather than evalue
aParBlastRev' :: String -> ActionFn
aParBlastRev' bCmd cfg cDir [o,e,q,s] = aParBlast' bCmd cfg cDir [o,e,s,q]
aParBlastRev' _ _ _ args = error $ "bad argument to aParBlast': " ++ show args
