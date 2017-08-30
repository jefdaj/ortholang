module ShortCut.Modules.Blast where

-- TODO remove the rest of the evalue machinery from rBlast once filter_evalue works!
-- TODO write/find filter_evalue.R

import Development.Shake
import ShortCut.Core.Types

import Data.Scientific            (formatScientific, FPFormat(..))
import ShortCut.Core.Config       (wrappedCmd)
import ShortCut.Core.Debug        (debugReadFile)
import ShortCut.Core.ModuleAPI    (rSimpleTmp, rMapLastTmp, defaultTypeCheck)
import ShortCut.Modules.SeqIO     (faa, fna)

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
    , reciprocal
    -- TODO vectorized versions
    -- TODO psiblast, dbiblast, deltablast, rpsblast, rpsblastn?
    ]
  }

{- The most straightforward way I can now think to do this is having a
 - top-level db dir cache/blastdb, and in there is a folder for each database.
 - The folder can be named by the hash of the fasta file it's made from, and
 - inside are whatever files blast wants to make.
 -
 - I don't think this needs to be exposed to ShortCut users as an actual
 - CutType yet, but that could happen at some point. Maybe it will seem more
 - like the thing to do once I get folder-based types figured out in a standard
 - way? (They'll probably come up elsewhere, like with tree-making programs) It
 - also might be needed to use the NCBI nr database; not sure yet.
 -}

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
mkBlastFn wrappedCmdFn qType sType = CutFunction
  { fName      = wrappedCmdFn
  , fTypeCheck = defaultTypeCheck [qType, sType, num] bht
  , fFixity    = Prefix
  , fCompiler  = rSimpleTmp (aParBlast wrappedCmdFn) "blast" bht
  }

-- TODO move to Util?
-- listFiles :: FilePath -> Action [FilePath]
-- listFiles dir = fmap (map (dir </>)) (getDirectoryFiles dir ["*"])

aParBlast :: String -> CutConfig -> CacheDir -> [ExprPath] -> Action ()
aParBlast bCmd cfg (CacheDir cDir) [ExprPath out, ExprPath query, ExprPath subject, ExprPath evalue] = do
  -- TODO is this automatic? need [query, subject, ePath]
  eStr <- fmap init $ debugReadFile cfg evalue
  let eDec = formatScientific Fixed Nothing (read eStr) -- format as decimal
  unit $ quietly $ wrappedCmd cfg [] "parallelblast.py" -- TODO Cwd cDir?
    [ "-c", bCmd, "-t", cDir, "-q", query, "-s", subject, "-o", out, "-e", eDec, "-p"]
aParBlast _ _ _ args = error $ "bad argument to aParBlast: " ++ show args

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

aFilterEvalue :: CutConfig -> CacheDir -> [ExprPath] -> Action ()
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

aBestHits :: CutConfig -> CacheDir -> [ExprPath] -> Action ()
aBestHits cfg (CacheDir tmp) [ExprPath out, ExprPath hits] = do
  unit $ quietly $ wrappedCmd cfg [Cwd tmp] "best_hits.R" [out, hits]
aBestHits _ _ args = error $ "bad argument to aBestHits: " ++ show args

--------------------------
-- reciprocal best hits --
--------------------------

-- TODO once this works, what should the actual fn people call look like?

reciprocal :: CutFunction
reciprocal = CutFunction
  { fName      = "reciprocal"
  , fTypeCheck = defaultTypeCheck [bht, bht] bht
  , fFixity    = Prefix
  , fCompiler  = rSimpleTmp aRecip "blast" bht
  }

aRecip :: CutConfig -> CacheDir -> [ExprPath] -> Action ()
aRecip cfg (CacheDir tmp) [ExprPath out, ExprPath left, ExprPath right] = do
  unit $ quietly $ wrappedCmd cfg [Cwd tmp] "reciprocal.R" [out, left, right]
aRecip _ _ args = error $ "bad argument to aRecip: " ++ show args

-----------------------------------
-- mapped versions of everything --
-----------------------------------

-- TODO gotta have a variation for "not the last arg"
mkBlastEachFn :: String -> CutType -> CutType -> CutFunction
mkBlastEachFn wrappedCmdFn qType sType = CutFunction
  { fName      = wrappedCmdFn ++ "_each"
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf sType] bht
  , fFixity    = Prefix
  , fCompiler  = rMapLastTmp (aParBlast' wrappedCmdFn) "blast" bht
  }

-- kludge to allow easy mapping over the subject rather than evalue
aParBlast' :: String -> CutConfig -> CacheDir -> [ExprPath] -> Action ()
aParBlast' bCmd cfg (CacheDir cDir) [ExprPath out, ExprPath evalue, ExprPath query, ExprPath subject] =
 aParBlast bCmd cfg (CacheDir cDir) [ExprPath out, ExprPath query, ExprPath subject, ExprPath evalue]
aParBlast' _ _ _ args = error $ "bad argument to aParBlast': " ++ show args

-- blastCRBAll :: CutFunction
-- blastCRBAll = CutFunction
--   { fName      = "crb_blast_all"
--   , fTypeCheck = defaultTypeCheck [faa, ListOf faa] (ListOf crb)
--   , fFixity    = Prefix
--   , fCompiler  = rMapLastTmps aBlastCRB "crbblast" crb
--   }
