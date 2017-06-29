module ShortCut.Modules.BlastCRB where

-- TODO any way to prevent it having to remake the mini6803 or whatever
--      query database each time? seems kind of inefficient! but can't put
--      everything in the same folder or it trips over itself in parallel

-- TODO what to do about the e-value cutoff?

import ShortCut.Core.Types
import Development.Shake

import Development.Shake.FilePath ((</>), (<.>))
import ShortCut.Core.Compile      (cExpr, scriptTmpDir, scriptTmpFile,
                                   hashedTmp', toShortCutList, exprDir)
import ShortCut.Core.Parse        (defaultTypeCheck)
import ShortCut.Modules.Fasta     (faa, fna)
import ShortCut.Modules.Vectorize (rMapLastTmp)
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
    , extractAllCrbQueries
    , extractCrbTargets
    , extractAllCrbTargets
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
  , fCompiler  = rSimpleTmp aBlastCRB "crbblast" crb
  }

blastCRBAll :: CutFunction
blastCRBAll = CutFunction
  { fName      = "crb_blast_all"
  , fTypeCheck = defaultTypeCheck [faa, ListOf faa] (ListOf crb)
  , fFixity    = Prefix
  , fCompiler  = rMapLastTmps aBlastCRB "crbblast" crb
  }

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

-- TODO move to another module
-- takes an action fn with any number of args and calls it with a tmpdir.
rSimpleTmp :: (CutConfig -> [FilePath] -> Action ()) -> String -> CutType
           -> (CutState -> CutExpr -> Rules FilePath)
rSimpleTmp actFn tmpPrefix rtnType s@(scr,cfg) e@(CutFun _ _ _ exprs) = do
  argPaths <- mapM (cExpr s) exprs
  let outPath = hashedTmp' cfg rtnType e []
      tmpDir  = scriptTmpDir cfg (cfgTmpDir cfg </> "cache" </> tmpPrefix) e
  outPath %> \_ -> do
    need argPaths
    liftIO $ createDirectoryIfMissing True tmpDir
    actFn cfg $ [tmpDir, outPath] ++ argPaths
    trackWrite [outPath]
  return outPath
rSimpleTmp _ _ _ _ _ = error "bad argument to rSimpleTmp"

-- TODO move to another module
-- takes an action fn and vectorizes the last arg (calls the fn with each of a
-- list of last args). returns a list of results. uses a new tmpDir each call.
rMapLastTmps :: (CutConfig -> [FilePath] -> Action ()) -> String -> CutType
             -> (CutState -> CutExpr -> Rules FilePath)
rMapLastTmps actFn tmpPrefix rtnType s@(_,cfg) e@(CutFun _ _ _ exprs) = do
  initPaths <- mapM (cExpr s) (init exprs)
  lastsPath <- cExpr s (last exprs)
  let outPath    = hashedTmp' cfg (ListOf rtnType) e []
      tmpPrefix' = cfgTmpDir cfg </> "cache" </> tmpPrefix
  outPath %> \_ -> do
    lastPaths <- readFileLines lastsPath
    let lasts = map (cfgTmpDir cfg </>) lastPaths
        dirs  = map (\p -> scriptTmpDir cfg tmpPrefix' [show e, show p]) lasts
        outs  = map (\d -> d </> "out" <.> extOf rtnType) dirs
        rels  = map (makeRelative $ cfgTmpDir cfg) outs -- TODO standardize this stuff
    (flip mapM)
      (zip3 lasts dirs outs)
      (\(last, dir, out) -> do
        need $ initPaths ++ [last]
        liftIO $ createDirectoryIfMissing True dir
        actFn cfg $ [dir, out] ++ initPaths ++ [last]
        trackWrite [out]
      )
    need outs
    writeFileLines outPath rels
  return outPath
rMapLastTmps _ _ _ _ _ = error "bad argument to rMapLastTmps"

aBlastCRB :: CutConfig -> [FilePath] -> Action ()
aBlastCRB cfg args@[tmpDir, oPath, qPath, tPath] =
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
  , fCompiler  = rSimpleTmp (aTsvColumn 1) "crbblast" (ListOf str)
  }

extractAllCrbQueries :: CutFunction
extractAllCrbQueries = CutFunction
  { fName      = "extract_all_crb_queries"
  , fTypeCheck = defaultTypeCheck [(ListOf crb)] (ListOf $ ListOf str)
  , fFixity    = Prefix
  , fCompiler  = rMapLastTmp (aTsvColumn 1) "crbblast" (ListOf str)
  }

extractCrbTargets :: CutFunction
extractCrbTargets = CutFunction
  { fName      = "extract_crb_targets"
  , fTypeCheck = defaultTypeCheck [crb] (ListOf str)
  , fFixity    = Prefix
  , fCompiler  = rSimpleTmp (aTsvColumn 2) "crbblast" (ListOf str)
  }

extractAllCrbTargets :: CutFunction
extractAllCrbTargets = CutFunction
  { fName      = "extract_all_crb_targets"
  , fTypeCheck = defaultTypeCheck [(ListOf crb)] (ListOf $ ListOf str)
  , fFixity    = Prefix
  , fCompiler  = rMapLastTmp (aTsvColumn 2) "crbblast" (ListOf str)
  }

-- TODO move to another module
aTsvColumn :: Int -> CutConfig -> [FilePath] -> Action ()
aTsvColumn n cfg as@[_, outPath, tsvPath] = do
  let awkCmd = "awk '{print $" ++ show n ++ "}'"
      tmpOut = scriptTmpFile cfg (exprDir cfg) ["aTsvColumn", show n, tsvPath] (extOf str)
  Stdout strs <- quietly $ cmd Shell awkCmd tsvPath
  writeFile' tmpOut strs
  toShortCutList cfg str tmpOut outPath
aTsvColumn _ _ as = error "bad arguments to aTsvColumn"
