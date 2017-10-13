module ShortCut.Modules.BlastRBH where

import Development.Shake
import ShortCut.Core.Types

import Control.Monad.Trans         (liftIO)
import ShortCut.Core.Compile.Basic (rExpr, rSimple, defaultTypeCheck)
import ShortCut.Core.Compile.Map   (rMap)
import ShortCut.Core.Config        (wrappedCmd)
import ShortCut.Core.Debug         (debugTrackWrite, debugAction)
import ShortCut.Core.Paths         (exprPath, cacheDir, CutPath, toCutPath,
                                    fromCutPath)
import ShortCut.Modules.Blast      (bht, BlastDesc, blastDescs)
import ShortCut.Modules.SeqIO      (faa, fna)
import System.Directory            (createDirectoryIfMissing)

-- TODO this module should provide:
--
-- reciprocal_best      :: bht -> bht -> bht
-- reciprocal_best_each :: bht -> [bht] -> [bht]
-- *blast*_rbh          :: fa -> fa -> bht     (for the ones that make sense)
-- *blast*_rbh_each     :: fa -> [fa] -> [bht] (for the ones with an _rbh)
--
-- TODO should the _rev functions also be moved here?
-- TODO ideally, remove a lot of this crap and the second python script
-- TODO test each one as you go: first all the peices, then together

cutModule :: CutModule
cutModule = CutModule
  { mName = "blastrbh"
  , mFunctions =
    [ reciprocalBest
    , reciprocalBestEach
    -- old fns to remove:
    -- , mkBlastSymRBH     "blastn" fna
    -- , mkBlastSymRBHEach "blastn" fna
    -- , mkBlastSymRBH     "blastp" faa
    -- , mkBlastSymRBHEach "blastp" faa
    ] ++

    -- new fns:
    -- TODO filter for the ones that make sense in _rbh form!
    map mkBlastRbh     blastDescs ++
    map mkBlastRbhEach blastDescs
  }

---------------------
-- reciprocal_best --
---------------------

-- TODO move to Tables.hs? And rename that to BlastHits?

reciprocalBest :: CutFunction
reciprocalBest = CutFunction
  { fName      = "reciprocal_best"
  , fTypeCheck = defaultTypeCheck [bht, bht] bht
  , fFixity    = Prefix
  , fRules     = rSimple aReciprocalBest
  }

-- TODO how are $TMPDIR paths getting through after conversion from cutpaths??
aReciprocalBest :: CutConfig -> [CutPath] -> Action ()
aReciprocalBest cfg [out, left, right] = do
  unit $ quietly $ wrappedCmd cfg [out'] [] "reciprocal_best.R" [out', left', right']
  debugTrackWrite cfg [out'']
  where
    out'   = fromCutPath cfg out
    left'  = fromCutPath cfg left
    right' = fromCutPath cfg right
    out''  = debugAction cfg "aReciprocalBest" out' [out', left', right']
aReciprocalBest _ args = error $ "bad argument to aReciprocalBest: " ++ show args

--------------------------
-- reciprocal_best_each --
--------------------------

reciprocalBestEach :: CutFunction
reciprocalBestEach = CutFunction
  { fName      = "reciprocal_best_each"
  , fTypeCheck = defaultTypeCheck [bht, ListOf bht] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rMap aReciprocalBest
  }

-----------------
-- *blast*_rbh --
-----------------

mkBlastRbh :: BlastDesc -> CutFunction
mkBlastRbh (bCmd, qType, sType, _) = CutFunction
  { fName      = bCmd ++ "_rbh"
  , fTypeCheck = defaultTypeCheck [num, qType, sType] bht
  , fFixity    = Prefix
  , fRules     = undefined
  }

-- TODO make the rest of them... but not until after lab meeting?
-- mkBlastSymRBH :: String -> CutType -> CutFunction
-- mkBlastSymRBH name faType = CutFunction
--   { fName = name ++ "_rbh"
--   , fTypeCheck = defaultTypeCheck [num, faType, faType] bht
--   , fFixity = Prefix
--   , fRules = rBlastSymRBH name
--   }

-- it this works I'll be modeling new versions of the map ones after it
-- rBlastSymRBH :: String -> RulesFn
-- rBlastSymRBH bCmd s@(_,cfg) e@(CutFun _ salt deps _ [evalue, lfa, rfa]) = do
--   let lhits = CutFun bht salt deps bCmd [lfa , rfa , evalue]
--       rhits = CutFun bht salt deps bCmd [rfa , lfa , evalue]
--       lbest = CutFun bht salt deps "best_hits"  [lhits] -- TODO remove?
--       rbest = CutFun bht salt deps "best_hits"  [rhits] -- TODO remove?
--       rbh   = CutFun bht salt deps "reciprocal_best" [lbest, rbest]
--       out   = fromCutPath cfg $ exprPath s e
--   (ExprPath rbhPath) <- rExpr s rbh -- TODO this is the sticking point right?
--   out %> \_ -> aBlastSymRBH cfg (CacheDir $ fromCutPath cfg $ cacheDir cfg "blast") [ExprPath out, ExprPath rbhPath]
--   return (ExprPath out)
-- rBlastSymRBH _ _ _ = error "bad argument to cBlastSymRBH"

-- this is an attempt to convert rBlastpRBH into a form usable with rMapTmp
-- TODO is it just cLink? seems pretty similar!
-- aBlastSymRBH :: ActionFn
-- aBlastSymRBH cfg _ [ExprPath out, ExprPath rbhPath] = do
--   need [rbhPath]
--   let out' = debugAction cfg "aBlastSymRBH" out [out, rbhPath]
--   unit $ quietly $ wrappedCmd cfg [out'] [] "ln" ["-fs", rbhPath, out']
--   debugTrackWrite cfg [out']
-- aBlastSymRBH _ _ args = error $ "bad arguments to aBlastSymRBH: " ++ show args

----------------------
-- *blast*_rbh_each --
----------------------

mkBlastRbhEach :: BlastDesc -> CutFunction
mkBlastRbhEach (bCmd, qType, sType, _) = CutFunction
  { fName      = bCmd ++ "_rbh_each"
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf sType] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = undefined
  }

-- TODO remove?
-- reciprocalBestEach :: CutFunction
-- reciprocalBestEach = CutFunction
--   { fName      = "reciprocal_best_each"
--   , fTypeCheck = defaultTypeCheck [bht, ListOf bht] (ListOf bht)
--   , fFixity    = Prefix
--   , fRules  = rRecipEach
--   }

-- TODO how to hook this up to blastp_each?
-- rRecipEach :: RulesFn
-- rRecipEach s@(_,cfg) e@(CutFun _ _ _ _ [lbhts, rbhts]) = do
--   (ExprPath lsPath) <- rExpr s lbhts
--   (ExprPath rsPath) <- rExpr s rbhts
--   let lsPath' = toCutPath cfg lsPath
--       rsPath' = toCutPath cfg rsPath
--   oPath' %> \_ -> aReciprocalBestEach cfg oPath lsPath' rsPath' cDir
--   return (ExprPath oPath')
--   where
--     cDir   = cacheDir cfg "reciprocal_best_each"
--     oPath  = exprPath s e
--     oPath' = fromCutPath cfg oPath
-- rRecipEach _ _ = error "bad argument to rRecipEach"

-- TODO ARE THE CUTPATHS GETTING THROUGH HERE? YEAH SUBPROCESS.WHATEVER...
-- NEED TO MAKE SOMETHING TO REPLACE ALL THROUGH BEFORE PASSING TO OUTSIDE
-- CODE, KIND OF LIKE FROMSHORTCUTLIST BUT HOPEFULLY A LOT SIMPLER?
--
-- THAT'S TOTALLY DOABLE, BUT WOULD REWRITING THIS SCRIPT IN HASKELL BE
-- EASIER? CERTAINLY SEEMS ARBITRARY TO HAVE JUST THIS ONE PART IN PYTHON.
-- TRY REWRITING IT FIRST I GUESS IN THE MORNING
-- aReciprocalBestEach :: CutConfig -> CutPath -> CutPath -> CutPath -> CutPath -> Action ()
-- aReciprocalBestEach cfg oPath lsPath rsPath cDir = do
--   need [lsPath', rsPath']
--   unit $ quietly $ wrappedCmd cfg [oPath'] [Cwd cDir'] "reciprocal_best_each.py"
--     [cDir', oPath', lsPath', rsPath']
--   debugTrackWrite cfg [oPath'']
--   where
--     oPath'  = fromCutPath cfg oPath
--     lsPath' = fromCutPath cfg lsPath
--     rsPath' = fromCutPath cfg rsPath
--     cDir'   = fromCutPath cfg cDir
--     oPath'' = debugAction cfg "aReciprocalBestEach" oPath' [oPath', rsPath', rsPath', cDir']

-----------------------------------------------
-- the hard part: mapped reciprocal versions --
-----------------------------------------------


-- TODO find a way to fix this or replace it...

-- mkBlastSymRBHEach :: CutFunction
-- mkBlastSymRBHEach = CutFunction
--   { fName      = "blastp_rbh_each"
--   , fTypeCheck = defaultTypeCheck [num, faa, ListOf faa] (ListOf bht)
--   , fFixity    = Prefix
--   , fRules  = rBlastSymRBHEach
--   }
-- 
-- -- TODO oh right, that might not be directly a list!
-- --      can this be done a cleaner way??
-- rBlastSymRBHEach :: RulesFn
-- rBlastSymRBHEach st@(_,cfg) expr@(CutFun _ salt _ _ [e, q, CutList _ _ _ ss]) = do
-- -- rBlastSymRBHEach st@(scr,cfg) expr@(CutFun _ salt _ _ [e, q, ss]) = do
--   -- let subjects = extractExprs scr ss
--   let exprs = map (\s -> CutFun bht salt (concatMap depsOf [e, q, s]) "blastp_rbh" [e, q, s]) ss
--   paths <- mapM (rExpr st) exprs
--   let (ExprPath out) = exprPath cfg True expr []
--       paths' = map (\(ExprPath p) -> p) paths
--   out %> \_ -> need paths' >> debugWriteLines cfg out paths' >> debugTrackWrite cfg [out]
--   return (ExprPath out)
-- rBlastSymRBHEach _ _ = error "bad argument to rBlastSymRBHEach"


-- mkBlastSymRBHEach :: String -> CutType -> CutFunction
-- mkBlastSymRBHEach name faType = CutFunction
--   { fName      = name ++ "_rbh_each"
--   , fTypeCheck = defaultTypeCheck [num, faType, ListOf faType] (ListOf bht)
--   , fFixity    = Prefix
--   , fRules  = rBlastSymRBHEach name
--   }


-- TODO how to remove all these files? will their mkBlast... take care of it?
-- rBlastSymRBHEach :: String -> RulesFn
-- rBlastSymRBHEach bCmd s@(_,cfg) e@(CutFun rtn salt deps _ [evalue, query, subjects]) = do
--   -- TODO need to get best_hits on each of the subjects before calling it, or duplicate the code inside?
--   -- let mkExpr name = CutFun bht salt deps "best_hits" [CutFun bht salt deps name [evalue, query, subjects]]
--   let mkExpr name = CutFun rtn salt deps name [evalue, query, subjects]
--       oPath  = exprPath s e
--       oPath' = fromCutPath cfg oPath
--       cDir   = cacheDir cfg "reciprocal_each"
--   (ExprPath fwdsPath) <- rExpr s $ mkExpr $ bCmd ++ "_each"
--   (ExprPath revsPath) <- rExpr s $ mkExpr $ bCmd ++ "_each_rev"
--   let fwdsPath' = toCutPath cfg fwdsPath
--       revsPath' = toCutPath cfg revsPath
--   oPath' %> \_ -> aBlastSymRBHEach cfg oPath cDir fwdsPath' revsPath'
--   return (ExprPath oPath')
-- rBlastSymRBHEach _ _ _ = error "bad argument to rBlastSymRBHEach"

-- aBlastSymRBHEach :: CutConfig -> CutPath -> CutPath
--                  -> CutPath -> CutPath -> Action ()
-- aBlastSymRBHEach cfg oPath cDir fwdsPath revsPath = do
--   need [fwdsPath', revsPath']
--   liftIO $ createDirectoryIfMissing True cDir'
--   unit $ quietly $ wrappedCmd cfg [oPath'] [Cwd cDir']
--                        "reciprocal_each.py" [cDir', oPath', fwdsPath', revsPath']
--   debugTrackWrite cfg [oPath'']
--   where
--     oPath'    = fromCutPath cfg oPath
--     cDir'     = fromCutPath cfg cDir
--     fwdsPath' = fromCutPath cfg fwdsPath
--     revsPath' = fromCutPath cfg revsPath
--     oPath'' = debugAction cfg "aBlastSymRBHEach" oPath' [oPath', cDir', fwdsPath', revsPath']
