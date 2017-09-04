module ShortCut.Modules.Blast where

-- TODO make a separate module for the reciprocal stuff?
-- TODO remove the rest of the evalue machinery from rBlast once filter_evalue works!
-- TODO write/find filter_evalue.R

import Development.Shake
import ShortCut.Core.Types

import Data.Scientific            (formatScientific, FPFormat(..))
import ShortCut.Core.Paths        (exprPath, cacheDir)
import ShortCut.Core.Util         (digest)
import ShortCut.Core.Compile      (cExpr)
import ShortCut.Core.Config       (wrappedCmd)
import ShortCut.Core.Debug        (debugReadFile, debugTrackWrite)
import ShortCut.Core.ModuleAPI    (rSimpleTmp, rMapLast, defaultTypeCheck)
import ShortCut.Modules.SeqIO     (faa, fna)
import System.Directory           (createDirectoryIfMissing)
import Control.Monad.Trans        (liftIO)
import System.FilePath            (takeBaseName, (</>))

cutModule :: CutModule
cutModule = CutModule
  { mName = "blast"
  , mFunctions =
    [ mkBlastDB

    , mkBlastFn  "blastn" fna fna -- TODO why doesn't this one work??
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

    -- TODO move to a new module:
    , reciprocal
    , blastpRBH
    , blastpRBHEach
    -- , blastpRBHEach -- TODO broken :(

    -- TODO psiblast, dbiblast, deltablast, rpsblast, rpsblastn?
    ]
  }

-- Users shouldn't need to work with these directly, but I discovered it's
-- muuuch easier to write the BLAST function if I include it here.
bdb :: CutType
bdb = CutType
  { tExt  = "bdb"
  , tDesc = "blast database"
  , tShow  = defaultShow -- TODO will this work? maybe use a dummy one
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

-------------------
-- make blast db --
-------------------

mkBlastDB :: CutFunction
mkBlastDB = CutFunction
  { fName      = "makeblastdb"
  , fTypeCheck = tMkBlastDB
  , fFixity    = Prefix
  , fCompiler  = cMkBlastDB
  }

tMkBlastDB :: [CutType] -> Either String CutType
tMkBlastDB [x] | x `elem` [faa, fna] = Right bdb
tMkBlastDB _ = error "makeblastdb requires a fasta file"

cMkBlastDB :: CutState -> CutExpr -> Rules ExprPath
cMkBlastDB s@(_,cfg) e@(CutFun _ _ _ _ [fa]) = do
  (ExprPath faPath) <- cExpr s fa
  let (CacheDir tmpDir) = cacheDir cfg "makeblastdb"
      (ExprPath dbPath) = exprPath cfg e []
      dbType = if      typeOf fa == fna then "nucl"
               else if typeOf fa == faa then "prot"
               else    error $ "invalid FASTA type for aBlastDB: " ++ show (typeOf fa)
  dbPath %> \_ -> do
    need [faPath]
    unit $ quietly $ wrappedCmd cfg [Cwd tmpDir] "makeblastdb"
      [ "-in"    , faPath
      , "-out"   , dbPath
      , "-title" , takeBaseName dbPath -- TODO does this make sense?
      , "-dbtype", dbType
      ]
    debugTrackWrite cfg [faPath]
  return (ExprPath dbPath)
cMkBlastDB _ _ = error "bad argument to mkBlastDB"

---------------------------
-- basic blast+ commands --
---------------------------

mkBlastFn :: String -> CutType -> CutType -> CutFunction
mkBlastFn bCmd qType sType = CutFunction
  { fName      = bCmd
  , fTypeCheck = defaultTypeCheck [qType, sType, num] bht
  , fFixity    = Prefix
  -- , fCompiler  = rSimpleTmp (aParBlast bCmd sType) "blast" bht
  , fCompiler  = cMkBlast bCmd aParBlast
  }

-- TODO wait, can this be done as just a transformation of the AST + an action fn?
--      (that is, no Rules monad needed?)
--      if so, can also write the _each version the same way!!
cMkBlast :: String
         -> (String -> CutConfig -> CacheDir -> [ExprPath] -> Action ())
         -> (CutState -> CutExpr -> Rules ExprPath)
cMkBlast bCmd bActFn st@(_,cfg) e@(CutFun _ _ _ _ [q, s, n]) = do
  qPath  <- cExpr st q
  dbPath <- cExpr st $ CutFun bdb (saltOf s) (depsOf s) "makeblastdb" [s]
  nPath  <- cExpr st n
  let (ExprPath htPath) = exprPath cfg e []
      tmpDir = cacheDir cfg "blast"
  htPath %> \_ -> bActFn bCmd cfg tmpDir [ExprPath htPath, qPath, dbPath, nPath]
  return (ExprPath htPath)
cMkBlast _ _ _ _ = error "bad argument to cMkBlast"

-- TODO this typechecks but WILL NOT WORK! need to make the blast db first
-- rMapLast takes an action function, which needs to be vectorized on its last argument
-- so we have the prime versions of each blast action function to work around that
-- TODO map functions that do that automatically
cMkBlastEach :: String
             -> (String -> CutConfig -> CacheDir -> [ExprPath] -> Action ())
             -> (CutState -> CutExpr -> Rules ExprPath)
cMkBlastEach bCmd bActFn s@(_,cfg) = rMapLast tmpFn (bActFn bCmd) "blast" bht s
  where
    tmpFn = const $ cacheDir cfg "blast"

-- parallelblast.py also has a function for this, but after I wrote that I
-- discovered the race condition where more than one instance of it tries to
-- create the db at once. Creating it first with Shake first prevents it.
-- aBlastDB sType cfg [ExprPath dbPath, ExprPath sPath] = do
-- aBlastDB _ _ _ = error "bad argument to aBlastDB"
aBlastDB :: CutType -> CutConfig -> ExprPath -> Action ExprPath
aBlastDB sType cfg (ExprPath sPath) = do
  need [sPath]
  sDigest <- fmap digest $ debugReadFile cfg sPath
  -- this should match find_db in parallelblast.py:
  let dbPath = cDir </> (sDigest ++ "_" ++ dbType)
  unit $ quietly $ wrappedCmd cfg [Cwd cDir] "makeblastdb"
    [ "-in"    , sPath
    , "-out"   , dbPath
    , "-title" , takeBaseName dbPath
    , "-dbtype", dbType
    ]
  debugTrackWrite cfg [dbPath]
  return (ExprPath dbPath)
  where
    (CacheDir cDir) = cacheDir cfg "blast"
    dbType = if      sType == fna then "nucl"
             else if sType == faa then "prot"
             else    error $ "invalid FASTA type for aBlastDB: " ++ show sType

aParBlast :: String -> CutConfig -> CacheDir -> [ExprPath] -> Action ()
aParBlast bCmd cfg (CacheDir cDir)
          [ExprPath out, ExprPath query, ExprPath db, ExprPath evalue] = do
  -- TODO is this automatic? need [query, subject, ePath]
  eStr <- fmap init $ debugReadFile cfg evalue
  -- (ExprPath dbPath) <- aBlastDB cfg sType subject -- TODO have to make this a pattern!
  let eDec = formatScientific Fixed Nothing (read eStr) -- format as decimal
  unit $ quietly $ wrappedCmd cfg [] "parallelblast.py" -- TODO Cwd cDir?
    -- [ "-c", bCmd, "-t", cDir, "-q", query, "-s", subject, "-o", out, "-e", eDec, "-p"]
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
  , fCompiler  = cMkBlast bCmd aParBlastRev
  }

-- just switches the query and subject, which won't work for asymmetric blast fns!
-- TODO write specific ones for that, or a fn + mapping
aParBlastRev :: String -> CutConfig -> CacheDir -> [ExprPath] -> Action ()
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

-- TODO remove?
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
      [cDir, oPath, lsPath, rsPath]
    debugTrackWrite cfg [oPath]
  return (ExprPath oPath)
cRecipEach _ _ = error "bad argument to cRecipEach"

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
  , fCompiler  = cMkBlastEach bCmd aParBlast
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
mkBlastEachRevFn bCmd qType sType = CutFunction
  { fName      = bCmd ++ "_each_rev"
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf sType] bht
  , fFixity    = Prefix
  -- , fCompiler  = rMapLastTmp (aParBlastRev' bCmd sType) "blast" bht
  , fCompiler  = cMkBlastEach bCmd aParBlastRev
  }

-- kludge to allow easy mapping over the subject rather than evalue
-- aParBlastRev' :: String -> CutConfig -> CacheDir -> [ExprPath] -> Action ()
-- aParBlastRev' bCmd cfg cDir [o,e,q,s] = aParBlast' bCmd sType cfg cDir [o,e,s,q]
-- aParBlastRev' _ _ _ args = error $ "bad argument to aParBlast': " ++ show args

-----------------------------------------------
-- the hard part: mapped reciprocal versions --
-----------------------------------------------


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


blastpRBHEach :: CutFunction
blastpRBHEach = CutFunction
  { fName      = "blastp_rbh_each"
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf faa] (ListOf bht)
  , fFixity    = Prefix
  , fCompiler  = cBlastpRBHEach
  }

cBlastpRBHEach :: CutState -> CutExpr -> Rules ExprPath
cBlastpRBHEach s@(_,cfg) e@(CutFun rtn salt deps _ [evalue, query, subjects]) = do
  -- TODO need to get best_hits on each of the subjects before calling it, or duplicate the code inside?
  -- let mkExpr name = CutFun bht salt deps "best_hits" [CutFun bht salt deps name [evalue, query, subjects]]
  let mkExpr name = CutFun rtn salt deps name [evalue, query, subjects]
      (ExprPath oPath)  = exprPath cfg e []
      (CacheDir cDir )  = cacheDir cfg "reciprocal_each"
  (ExprPath fwdsPath) <- cExpr s $ mkExpr "blastp_each"
  (ExprPath revsPath) <- cExpr s $ mkExpr "blastp_each_rev"
  oPath %> \_ -> do
    need [fwdsPath, revsPath]
    liftIO $ createDirectoryIfMissing True cDir
    unit $ quietly $ wrappedCmd cfg [Cwd cDir] "reciprocal_each.py" [cDir, oPath, fwdsPath, revsPath] -- TODO how is it failing? seems fine
    debugTrackWrite cfg [oPath]
  return (ExprPath oPath)
cBlastpRBHEach _ _ = error "bad argument to cRecipEach"
