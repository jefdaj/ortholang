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
    , mkBlastEachRevFn "blastn" fna fna -- TODO don't expose to users?
    , mkBlastEachRevFn "blastp" faa faa -- TODO don't expose to users?
    , reciprocal
    , blastpRBH
    -- , blastpRBHEach -- TODO broken :(
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

-----------------------------------------------------------
-- "reverse" versions to help write reciprocal best hits --
-----------------------------------------------------------

-- TODO remove these if you don't end up using them

-- note: only works on symmetric blast fns (take two of the same type)
mkBlastRevFn :: String -> CutType -> CutType -> CutFunction
mkBlastRevFn wrappedCmdFn qType sType = CutFunction
  { fName      = wrappedCmdFn ++ "_rev"
  , fTypeCheck = defaultTypeCheck [qType, sType, num] bht
  , fFixity    = Prefix
  , fCompiler  = rSimpleTmp (aParBlastRev wrappedCmdFn) "blast" bht
  }

-- just switches the query and subject, which won't work for asymmetric blast fns!
aParBlastRev :: String -> CutConfig -> CacheDir -> [ExprPath] -> Action ()
aParBlastRev bCmd cfg cDir [o, q, s, e] = aParBlast bCmd cfg cDir [o, s, q, e]
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

-- TODO make the rest of them... but not until after lab meeting?
blastpRBH :: CutFunction
blastpRBH = CutFunction
  { fName = "blastp_rbh"
  , fTypeCheck = defaultTypeCheck [num, faa, faa] bht
  , fFixity = Prefix
  , fCompiler = cBlastpRBH
  }

-- it this works I'll be modeling new versions of the map ones after it
cBlastpRBH :: CutState -> CutExpr -> Rules ExprPath
cBlastpRBH s@(_,cfg) e@(CutFun _ salt deps _ [evalue, lfaa, rfaa]) = do
  let lhits = CutFun bht salt deps "blastp"     [lfaa , rfaa , evalue]
      rhits = CutFun bht salt deps "blastp"     [rfaa , lfaa , evalue]
      lbest = CutFun bht salt deps "best_hits"  [lhits]
      rbest = CutFun bht salt deps "best_hits"  [rhits]
      rbh   = CutFun bht salt deps "reciprocal" [lbest, rbest]
      (ExprPath out) = exprPath cfg e []
  (ExprPath rbhPath) <- cExpr s rbh -- TODO this is the sticking point right?
  out %> \_ -> do
    need [rbhPath]
    aBlastpRBH cfg (cacheDir cfg "blast") [ExprPath out, ExprPath rbhPath]
    debugTrackWrite cfg [out]
  return (ExprPath out)
cBlastpRBH _ _ = error "bad argument to cBlastRBH"

-- this is an attempt to convert cBlastpRBH into a form usable with rMapLastTmp
aBlastpRBH :: CutConfig -> CacheDir -> [ExprPath] -> Action ()
aBlastpRBH cfg _ [ExprPath out, ExprPath rbhPath] =
  unit $ quietly $ wrappedCmd cfg [] "ln" ["-fs", rbhPath, out]
aBlastpRBH _ _ args = error $ "bad arguments to aBlastpRBH: " ++ show args

---------------------------------------------
-- kludge for mapping reciprocal best hits --
---------------------------------------------

reciprocalEach :: CutFunction
reciprocalEach = CutFunction
  { fName      = "reciprocal_each"
  , fTypeCheck = defaultTypeCheck [bht, ListOf bht] (ListOf bht)
  , fFixity    = Prefix
  , fCompiler  = cRecipEach
  }

-- TODO how to hook this up to blastp_each?
cRecipEach :: CutState -> CutExpr -> Rules ExprPath
cRecipEach s@(_,cfg) e@(CutFun _ _ _ _ [lbhts, rbhts]) = do
  (ExprPath lsPath) <- cExpr s lbhts
  (ExprPath rsPath) <- cExpr s rbhts
  let (ExprPath oPath) = exprPath cfg e []
      (CacheDir cDir ) = cacheDir cfg "reciprocal_each"
  oPath %> \_ -> do
    need [lsPath, rsPath]
    unit $ quietly $ wrappedCmd cfg [Cwd cDir] "reciprocal_each.py"
      [oPath, lsPath, rsPath]
    debugTrackWrite cfg [oPath]
  return (ExprPath oPath)
cRecipEach _ _ = error "bad argument to cRecipEach"

----------------------------------
-- mapped versions of blast fns --
----------------------------------

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
aParBlast' bCmd cfg (CacheDir cDir) [ExprPath o, ExprPath e, ExprPath q, ExprPath s] =
 aParBlast bCmd cfg (CacheDir cDir) [ExprPath o, ExprPath q, ExprPath s, ExprPath e]
aParBlast' _ _ _ args = error $ "bad argument to aParBlast': " ++ show args

---------------------------------------------------------------------
-- reverse mapped versions (needed for mapped reciprocal versions) --
---------------------------------------------------------------------

-- TODO gotta have a variation for "not the last arg"
mkBlastEachRevFn :: String -> CutType -> CutType -> CutFunction
mkBlastEachRevFn wrappedCmdFn qType sType = CutFunction
  { fName      = wrappedCmdFn ++ "_each_rev"
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf sType] bht
  , fFixity    = Prefix
  , fCompiler  = rMapLastTmp (aParBlastRev' wrappedCmdFn) "blast" bht
  }

-- kludge to allow easy mapping over the subject rather than evalue
aParBlastRev' :: String -> CutConfig -> CacheDir -> [ExprPath] -> Action ()
aParBlastRev' bCmd cfg cDir [o,e,q,s] = aParBlast' bCmd cfg cDir [o,e,s,q]
aParBlastRev' _ _ _ args = error $ "bad argument to aParBlast': " ++ show args


-- TODO find a way to fix this or replace it...

-- blastpRBHEach :: CutFunction
-- blastpRBHEach = CutFunction
--   { fName      = "blastp_rbh_each"
--   , fTypeCheck = defaultTypeCheck [num, faa, ListOf faa] (ListOf bht)
--   , fFixity    = Prefix
--   , fCompiler  = cBlastpRBHEach
--   }
-- 
-- -- TODO oh right, that might not be directly a list!
-- --      can this be done a cleaner way??
-- cBlastpRBHEach :: CutState -> CutExpr -> Rules ExprPath
-- cBlastpRBHEach st@(_,cfg) expr@(CutFun _ salt _ _ [e, q, CutList _ _ _ ss]) = do
-- -- cBlastpRBHEach st@(scr,cfg) expr@(CutFun _ salt _ _ [e, q, ss]) = do
--   -- let subjects = extractExprs scr ss
--   let exprs = map (\s -> CutFun bht salt (concatMap depsOf [e, q, s]) "blastp_rbh" [e, q, s]) ss
--   paths <- mapM (cExpr st) exprs
--   let (ExprPath out) = exprPath cfg expr []
--       paths' = map (\(ExprPath p) -> p) paths
--   out %> \_ -> need paths' >> debugWriteLines cfg out paths' >> debugTrackWrite cfg [out]
--   return (ExprPath out)
-- cBlastpRBHEach _ _ = error "bad argument to cBlastpRBHEach"
