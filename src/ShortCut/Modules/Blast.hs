module ShortCut.Modules.Blast where

-- TODO remove the rest of the evalue machinery from rBlast once filter_evalue works!
-- TODO write/find filter_evalue.R

import Development.Shake
import ShortCut.Core.Types

import Data.Scientific            (formatScientific, FPFormat(..))
import ShortCut.Core.Paths        (exprPath, cacheDir)
import ShortCut.Core.Compile      (cExpr)
import ShortCut.Core.Config       (wrappedCmd)
import ShortCut.Core.Debug        (debugReadFile, debugTrackWrite)
import ShortCut.Core.ModuleAPI    (defaultTypeCheck)
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
mkBlastFn wrappedCmdFn qType tType = CutFunction
  { fName      = wrappedCmdFn
  , fTypeCheck = defaultTypeCheck [qType, tType, num] bht
  , fFixity    = Prefix
  , fCompiler  = rParallelBlast wrappedCmdFn
  }

-- TODO move to Util?
-- listFiles :: FilePath -> Action [FilePath]
-- listFiles dir = fmap (map (dir </>)) (getDirectoryFiles dir ["*"])

rParallelBlast :: String -> (CutState -> CutExpr -> Rules ExprPath)
rParallelBlast bCmd s@(_,cfg) e@(CutFun _ _ _ _ [query, subject, evalue]) = do
  (ExprPath qPath) <- cExpr s query
  (ExprPath sPath) <- cExpr s subject
  (ExprPath ePath) <- cExpr s evalue
  let (CacheDir cDir ) = cacheDir cfg "blast"
      (ExprPath oPath) = exprPath cfg e []
  oPath %> \_ -> do
    need [qPath, sPath, ePath]
    eStr <- fmap init $ debugReadFile cfg ePath
    let eDec = formatScientific Fixed Nothing (read eStr) -- format as decimal
    unit $ quietly $ wrappedCmd cfg [] "parallelblast.py" -- TODO Cwd cDir?
      [ "-c", bCmd
      , "-t", cDir
      , "-q", qPath
      , "-s", sPath
      , "-o", oPath
      , "-e", eDec
      , "-p"
      ]
    debugTrackWrite cfg [oPath]
  return (ExprPath oPath)
rParallelBlast _ _ _ = error "bad argument to rParallelBlast"

---------------------------
-- filter hits by evalue --
---------------------------

filterEvalue :: CutFunction
filterEvalue = CutFunction
  { fName      = "filter_evalue"
  , fTypeCheck = defaultTypeCheck [num, bht] bht
  , fFixity    = Prefix
  , fCompiler  = cFilterEvalue
  }

cFilterEvalue :: CutState -> CutExpr -> Rules ExprPath
cFilterEvalue s@(_,cfg) e@(CutFun _ _ _ _ [evalue, hits]) = do
  (ExprPath ePath) <- cExpr s evalue
  (ExprPath hPath) <- cExpr s hits
  let (ExprPath oPath) = exprPath cfg e []
  oPath %> \_ -> do
    need [ePath, hPath]
    unit$ quietly $ wrappedCmd cfg [] "filter_evalue.R" [oPath, ePath, hPath]
    debugTrackWrite cfg [oPath]
  return (ExprPath oPath)
cFilterEvalue _ _ = error "bad argument to cFilterEvalue"
