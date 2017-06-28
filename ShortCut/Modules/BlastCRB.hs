module ShortCut.Modules.BlastCRB where

-- TODO any way to prevent it having to remake the mini6803 or whatever
--      query database each time? seems kind of inefficient! but can't put
--      everything in the same folder or it trips over itself in parallel

-- TODO what to do about the e-value cutoff?

import ShortCut.Core.Types
import Development.Shake

import Development.Shake.FilePath ((</>), (<.>))
import ShortCut.Core.Compile      (cExpr, scriptTmpDir, hashedTmp', toShortCutList)
import ShortCut.Core.Parse        (defaultTypeCheck)
import ShortCut.Modules.Fasta     (faa, fna)
import ShortCut.Modules.Vectorize (vectorize)
import ShortCut.Modules.Repeat    (extractExprs)
import System.Directory           (createDirectoryIfMissing)
import System.FilePath            (makeRelative)

---------------
-- interface --
---------------

cutModule :: CutModule
cutModule = CutModule
  { mName = "crb-blast"
  , mFunctions =
    [ blastCRB
    , blastCRBAll
    , extractCrbQueries
    , extractCrbTargets
    ]
  }

crb :: CutType
crb = CutType
  { tExt  = "crb"
  , tDesc = "tab-separated table of conditional reciprocal blast best hits"
  , tCat  = defaultCat
  }

----------------------
-- basic crb search --
----------------------

blastCRB :: CutFunction
blastCRB = CutFunction
  { fName      = "crb_blast" -- TODO match the other no-underscore blast binaries?
  , fTypeCheck = defaultTypeCheck [faa, faa] crb
  , fFixity    = Prefix
  , fCompiler  = rSimple aBlastCRB "crbblast" crb
  }

blastCRBAll :: CutFunction
blastCRBAll = vectorize blastCRB "crb_blast_all"

-- output columns:
-- query - the name of the transcript from the 'query' fasta file
-- target - the name of the transcript from the 'target' fasta file
-- id - the percent sequence identity
-- alnlen - the alignment length
-- evalue - the blast evalue
-- bitscore - the blast bitscore
-- qstart..qend - the coordinates of the alignment in the query from start to end
-- tstart..tend - the coordinates of the alignment in the target from start to end 
-- qlen - the length of the query transcript
-- tlen - the length of the target transcript

rSimple :: ([FilePath] -> Action ()) -> String -> CutType
        -> (CutState -> CutExpr -> Rules FilePath)
rSimple actFn tmpPrefix rtnType s@(scr,cfg) e@(CutFun _ _ _ exprs) = do
  argPaths <- mapM (cExpr s) exprs
  let outPath = hashedTmp' cfg rtnType e []
      tmpDir  = scriptTmpDir (cfgTmpDir cfg </> "cache" </> tmpPrefix) e
  outPath %> \_ -> do
    need argPaths
    liftIO $ createDirectoryIfMissing True tmpDir
    actFn $ [tmpDir, outPath] ++ argPaths
    trackWrite [outPath]
  return outPath
rSimple _ _ _ _ _ = error "bad argument to cSimple"

-- TODO move to another module
-- takes an action fn and vectorizes the last arg (calls the fn with each of a
-- list of last args). returns a list of results. uses a new tmpDir each call.
rMapLastTmpEach :: ([FilePath] -> Action ()) -> String -> CutType
                -> (CutState -> CutExpr -> Rules FilePath)
rMapLastTmpEach actFn tmpPrefix rtnType s@(scr,cfg) e@(CutFun _ _ _ exprs) = do
  initPaths <- mapM (cExpr s) (init exprs)
  lastsPath <- cExpr s (last exprs)
  let outPath    = hashedTmp' cfg rtnType e []
      tmpPrefix' = cfgTmpDir cfg </> "cache" </> tmpPrefix
  outPath %> \_ -> do
    lastPaths <- readFileLines lastsPath
    let dirs = map (\p -> scriptTmpDir tmpPrefix' [show e, show p]) lastPaths
        outs = map (\d -> d </> "out" <.> tExt rtnType) dirs
        rels = map (makeRelative $ cfgTmpDir cfg) outs
    (flip mapM)
      (zip3 lastPaths dirs outs)
      (\(last, dir, out) -> do
        need (initPaths ++ [last])
        liftIO $ createDirectoryIfMissing True dir
        actFn $ [dir, out] ++ initPaths ++ [last]
        trackWrite [out]
      )
    need outs
    writeFileLines outPath rels
  return outPath
rMapLastTmpEach _ _ _ _ _ = error "bad argument to rMapLastTmpEach"

aBlastCRB :: [FilePath] -> Action ()
aBlastCRB [tmpDir, oPath, qPath, tPath] =
  quietly $ cmd (Cwd tmpDir) "crb-blast"
    [ "--query"  , qPath
    , "--target" , tPath
    , "--output" , oPath
    , "--threads", "8" -- TODO how to pick this?
    , "--split"
    ]

-------------------------------
-- list query or target hits --
-------------------------------

extractCrbQueries :: CutFunction
extractCrbQueries = CutFunction
  { fName      = "extract_crb_queries"
  , fTypeCheck = defaultTypeCheck [crb] (ListOf str)
  , fFixity    = Prefix
  , fCompiler  = rTsvColumn 1
  }

extractCrbTargets :: CutFunction
extractCrbTargets = CutFunction
  { fName      = "extract_crb_targets"
  , fTypeCheck = defaultTypeCheck [crb] (ListOf str)
  , fFixity    = Prefix
  , fCompiler  = rTsvColumn 2
  }

-- TODO move to another module
rTsvColumn :: Int -> (CutState -> CutExpr -> Rules FilePath)
rTsvColumn n s@(_,cfg) e@(CutFun _ _ _ [tsvExpr]) = do
  tsvPath <- cExpr s tsvExpr
  let tmpPath = hashedTmp' cfg str e []
      outPath = hashedTmp' cfg (ListOf str) e []
  tmpPath %> \out -> do
    need [tsvPath]
    let awkCmd = "awk '{print $" ++ show n ++ "}'"
    Stdout strs <- quietly $ cmd Shell awkCmd tsvPath
    writeFile' out strs
  outPath %> \out -> toShortCutList s str tmpPath out
  return outPath
extractCrbColumn _ _ _ = error "bad argument to extractCrbColumn"
