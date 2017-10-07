module ShortCut.Modules.Blast where

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
    [ mkBlastFn        "blastn" fna fna ndb -- TODO why doesn't this one work??
    , mkBlastFn        "blastp" faa faa pdb
    , mkBlastFn        "blastx" fna faa pdb
    , mkBlastFn       "tblastn" faa fna ndb
    , mkBlastFn       "tblastx" fna fna ndb
    , mkBlastDbFn      "blastn" fna ndb -- TODO why doesn't this one work??
    , mkBlastDbFn      "blastp" faa pdb
    , mkBlastDbFn      "blastx" fna pdb
    , mkBlastDbFn     "tblastn" faa ndb
    , mkBlastDbFn     "tblastx" fna ndb
    , mkBlastEachFn    "blastn" fna fna ndb -- TODO why doesn't this one work??
    , mkBlastEachFn    "blastp" faa faa pdb
    , mkBlastEachFn    "blastx" fna faa pdb
    , mkBlastEachFn   "tblastn" faa fna ndb
    , mkBlastEachFn   "tblastx" fna fna ndb
    , mkBlastEachRevFn "blastn" fna fna ndb -- TODO don't expose to users?
    , mkBlastEachRevFn "blastp" faa faa pdb -- TODO don't expose to users?
    -- TODO use the reverse each ones?
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

-- the "regular" function that requires an existing blast db
mkBlastDbFn :: String -> CutType -> CutType -> CutFunction
mkBlastDbFn bCmd qType dbType = CutFunction
  { fName      = bCmd ++ "_db"
  , fTypeCheck = defaultTypeCheck [qType, dbType, num] bht
  , fFixity    = Prefix
  , fRules  = rMkBlastDbFn bCmd aParBlast
  }

-- the "fancy" one that makes the db from a fasta file
-- (this is what i imagine users will usually want)
mkBlastFn :: String -> CutType -> CutType -> CutType -> CutFunction
mkBlastFn bCmd qType sType dbType = CutFunction
  { fName      = bCmd
  , fTypeCheck = defaultTypeCheck [qType, sType, num] bht
  , fFixity    = Prefix
  , fRules  = rMkBlastFn bCmd dbType aParBlast
  }

rMkBlastDbFn :: String -> (String -> (CutConfig -> CutPath -> [CutPath] -> Action ())) -> RulesFn
rMkBlastDbFn bCmd bActFn = rSimpleTmp (bActFn bCmd) "blast" bht

-- convert the fasta file to a db and pass to the db version (above)
rMkBlastFn :: String -> CutType -> (String -> (CutConfig -> CutPath -> [CutPath] -> Action ())) -> RulesFn
rMkBlastFn c dbType a s e = rMkBlastDbFn c a s $ addMakeDBCall1 e dbType

-- TODO why doesn't this `need` the db out path?
addMakeDBCall1 :: CutExpr -> CutType -> CutExpr
addMakeDBCall1 (CutFun r i ds n [q, s, e]) dbType = CutFun r i ds n [q, db, e]
  where
    -- dbType = if typeOf s `elem` [fna, ListOf fna] then ndb else pdb -- TODO maybe it's (ListOf fna)?
    db = CutFun dbType i (depsOf s) name [s]
    name = "makeblastdb" ++ if dbType == ndb then "_nucl" else "_prot"
addMakeDBCall1 _ _ = error "bad argument to addMakeDBCall1"

-- as a quick kludge, duplicated this and rearranged the args
addMakeDBCall2 :: CutExpr -> CutType -> CutExpr
addMakeDBCall2 (CutFun r i ds n [e, q, ss]) dbType = CutFun r i ds n [e, q, dbs]
  where
    -- dbType = if typeOf s `elem` [fna, ListOf fna] then ndb else pdb -- TODO maybe it's (ListOf fna)?
    dbs = CutFun dbType i (depsOf ss) name [ss]
    name = "makeblastdb" ++ (if dbType == ndb then "_nucl" else "_prot") ++ "_each"
addMakeDBCall2 _ _ = error "bad argument to addMakeDBCall2"

-- TODO remove the old bbtmp default tmpDir
aParBlast :: String -> (CutConfig -> CutPath -> [CutPath] -> Action ())
aParBlast bCmd cfg _ paths@[o, q, p, e] = do
  liftIO $ putStrLn $ "aParBlast args: " ++ show paths
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
    o'' = debugAction cfg "aParBlast" o' [bCmd, o', q', p', e']
aParBlast _ _ _ _ = error $ "bad argument to aParBlast"

---------------------
-- mapped versions --
---------------------

-- TODO gotta have a variation for "not the last arg"
mkBlastEachFn :: String -> CutType -> CutType -> CutType -> CutFunction
mkBlastEachFn bCmd qType sType dbType = CutFunction
  { fName      = bCmd ++ "_each"
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf sType] (ListOf bht)
  , fFixity    = Prefix
  , fRules  = rMkBlastEach bCmd dbType aParBlast
  }

-- TODO need to apply addMakeDBCall2 *after* mapping over the last arg
-- TODO more specific tmpDir?
rMkBlastEach :: String -> CutType -> (String -> (CutConfig -> CutPath -> [CutPath] -> Action ())) -> RulesFn
rMkBlastEach bCmd dbType bActFn st@(_,cfg) expr = mapFn st $ addMakeDBCall2 expr' dbType
  where
    mapFn = rMapTmp (bActFn' bCmd) (bCmd ++ "_each") bCmd
    expr' = debug cfg ("rMkBlastEach expr: '" ++ render (pPrint expr) ++ "'") expr
    -- kludge to allow easy mapping over the subject rather than evalue:
    -- TODO is this right?
    -- TODO can it be changed to keep the evalues at the end like expected?
    -- TODO are the e and q getting reversed? they look OK here
    bActFn' b c d [o, e, q, s] = let args = [o, q, s, e]; args' = debug cfg ("bActFn args: " ++ show args) args in bActFn b c d args'
    bActFn' _ _ _ _ = error "bad argument to bActFn'"

-----------------------------------------------------------
-- "reverse" versions to help write reciprocal best hits --
-----------------------------------------------------------

-- TODO move to BlastRBH module?

-- note: only works on symmetric blast fns (take two of the same type)
mkBlastRevFn :: String -> CutType -> CutType -> CutType -> CutFunction
mkBlastRevFn bCmd qType sType dbType = CutFunction
  { fName      = bCmd ++ "_rev"
  , fTypeCheck = defaultTypeCheck [qType, sType, num] bht
  , fFixity    = Prefix
  , fRules  = rMkBlastFn bCmd dbType aParBlastRev
  }

-- just switches the query and subject, which won't work for asymmetric blast fns!
-- TODO write specific ones for that, or a fn + mapping
-- TODO debug transformations too!
aParBlastRev :: String -> (CutConfig -> CutPath -> [CutPath] -> Action ())
aParBlastRev b c d [o, q, s, e] = aParBlast b c d [o, s, q, e]
aParBlastRev _ _ _ args = error $ "bad argument to aParBlast: " ++ show args

-- TODO gotta have a variation for "not the last arg"
mkBlastEachRevFn :: String -> CutType -> CutType -> CutType -> CutFunction
mkBlastEachRevFn bCmd qType sType dbType = CutFunction
  { fName      = bCmd ++ "_each_rev"
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf sType] bht
  , fFixity    = Prefix
  , fRules  = rMkBlastEach bCmd dbType aParBlastRev
  }
