module ShortCut.Modules.Blast where

import Development.Shake
import ShortCut.Core.Types

import Data.Scientific          (formatScientific, FPFormat(..))
import ShortCut.Core.Config     (wrappedCmd)
import ShortCut.Core.Debug      (debugReadFile, debugTrackWrite)
import ShortCut.Core.ModuleAPI  (rSimpleTmp, rMapLastTmp, defaultTypeCheck)
import ShortCut.Modules.BlastDB (ndb, pdb)
import ShortCut.Modules.SeqIO   (faa, fna)
import System.FilePath          (takeDirectory, takeFileName, (</>))
import ShortCut.Core.Util      (stripWhiteSpace)

cutModule :: CutModule
cutModule = CutModule
  { mName = "blast"
  , mFunctions =
    [ mkBlastFn        "blastn" fna fna -- TODO why doesn't this one work??
    , mkBlastFn        "blastp" faa faa
    , mkBlastFn        "blastx" fna faa
    , mkBlastFn       "tblastn" faa fna
    , mkBlastFn       "tblastx" fna fna
    , mkBlastDbFn      "blastn" fna ndb -- TODO why doesn't this one work??
    , mkBlastDbFn      "blastp" faa pdb
    , mkBlastDbFn      "blastx" fna pdb
    , mkBlastDbFn     "tblastn" faa ndb
    , mkBlastDbFn     "tblastx" fna ndb
    , mkBlastEachFn    "blastn" fna fna -- TODO why doesn't this one work??
    , mkBlastEachFn    "blastp" faa faa
    , mkBlastEachFn    "blastx" fna faa
    , mkBlastEachFn   "tblastn" faa fna
    , mkBlastEachFn   "tblastx" fna fna
    , mkBlastEachRevFn "blastn" fna fna -- TODO don't expose to users?
    , mkBlastEachRevFn "blastp" faa faa -- TODO don't expose to users?
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
  , fCompiler  = cMkBlastDbFn bCmd aParBlast
  }

-- the "fancy" one that makes the db from a fasta file
-- (this is what i imagine users will usually want)
mkBlastFn :: String -> CutType -> CutType -> CutFunction
mkBlastFn bCmd qType sType = CutFunction
  { fName      = bCmd
  , fTypeCheck = defaultTypeCheck [qType, sType, num] bht
  , fFixity    = Prefix
  , fCompiler  = cMkBlastFn bCmd aParBlast
  }

cMkBlastDbFn :: String -> (String -> ActionFn) -> RulesFn
cMkBlastDbFn bCmd bActFn = rSimpleTmp (bActFn bCmd) "blast" bht

-- convert the fasta file to a db and pass to the db version (above)
cMkBlastFn :: String -> (String -> ActionFn) -> RulesFn
cMkBlastFn c a s e = cMkBlastDbFn c a s $ addMakeDBCall e

addMakeDBCall :: CutExpr -> CutExpr
addMakeDBCall (CutFun r i ds n [q, s, e]) = CutFun r i ds n [q, db, e]
  where
    dbType = if typeOf s == fna then ndb else pdb
    db = CutFun dbType i (depsOf s) "makeblastdb" [s]
addMakeDBCall _ = error "bad argument to addMakeDBCall"

aParBlast :: String -> ActionFn
aParBlast bCmd cfg _ [ExprPath o, ExprPath q, ExprPath p, ExprPath e] = do
  eStr   <- fmap init $ debugReadFile cfg e
  prefix <- fmap (cfgTmpDir cfg </>)
          $ fmap stripWhiteSpace
          $ debugReadFile cfg p
  let eDec = formatScientific Fixed Nothing (read eStr) -- format as decimal
      cDir = cfgTmpDir cfg </> takeDirectory prefix
      dbg  = if cfgDebug cfg then ["-v"] else []
      args = [ "-c", bCmd, "-t", cDir, "-q", q, "-d", takeFileName prefix
             , "-o", o   , "-e", eDec, "-p"] ++ dbg
  unit $ quietly $ wrappedCmd cfg [Cwd $ takeDirectory prefix] "parallelblast.py" args -- TODO Cwd?
  debugTrackWrite cfg [o]
aParBlast _ _ _ _ = error $ "bad argument to aParBlast"

---------------------
-- mapped versions --
---------------------

-- TODO gotta have a variation for "not the last arg"
mkBlastEachFn :: String -> CutType -> CutType -> CutFunction
mkBlastEachFn bCmd qType sType = CutFunction
  { fName      = bCmd ++ "_each"
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf sType] (ListOf bht)
  , fFixity    = Prefix
  , fCompiler  = cMkBlastEach bCmd aParBlast
  }

cMkBlastEach :: String -> (String -> ActionFn) -> RulesFn
cMkBlastEach bCmd bActFn st expr = mapFn st $ addMakeDBCall expr
  where
    mapFn = rMapLastTmp (bActFn' bCmd) "blast" bht
    -- kludge to allow easy mapping over the subject rather than evalue:
    -- TODO is this right?
    -- TODO can it be changed to keep the evalues at the end like expected?
    bActFn' b c d [o, e, q, s] = bActFn b c d [o, q, s, e]
    bActFn' _ _ _ _ = error "bad argument to bActFn'"

-----------------------------------------------------------
-- "reverse" versions to help write reciprocal best hits --
-----------------------------------------------------------

-- TODO move to BlastRBH module?

-- note: only works on symmetric blast fns (take two of the same type)
mkBlastRevFn :: String -> CutType -> CutType -> CutFunction
mkBlastRevFn bCmd qType sType = CutFunction
  { fName      = bCmd ++ "_rev"
  , fTypeCheck = defaultTypeCheck [qType, sType, num] bht
  , fFixity    = Prefix
  , fCompiler  = cMkBlastFn bCmd aParBlastRev
  }

-- just switches the query and subject, which won't work for asymmetric blast fns!
-- TODO write specific ones for that, or a fn + mapping
aParBlastRev :: String -> ActionFn
aParBlastRev b c d [o, q, s, e] = aParBlast b c d [o, s, q, e]
aParBlastRev _ _ _ args = error $ "bad argument to aParBlast: " ++ show args

-- TODO gotta have a variation for "not the last arg"
mkBlastEachRevFn :: String -> CutType -> CutType -> CutFunction
mkBlastEachRevFn bCmd qType sType = CutFunction
  { fName      = bCmd ++ "_each_rev"
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf sType] bht
  , fFixity    = Prefix
  , fCompiler  = cMkBlastEach bCmd aParBlastRev
  }
