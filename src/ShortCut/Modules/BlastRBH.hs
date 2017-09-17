module ShortCut.Modules.BlastRBH where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Compile.Paths     (exprPath, cacheDir)
import ShortCut.Core.Config    (wrappedCmd)
import ShortCut.Core.Debug     (debugTrackWrite, debugAction)
import ShortCut.Core.Compile.Rules     (rExpr, rSimpleTmp, defaultTypeCheck)
import ShortCut.Modules.SeqIO  (faa)
import ShortCut.Modules.Blast  (bht)
import System.Directory        (createDirectoryIfMissing)
import Control.Monad.Trans     (liftIO)

cutModule :: CutModule
cutModule = CutModule
  { mName = "blastrbh"
  , mFunctions =
    [ reciprocal
    , blastpRBH
    , blastpRBHEach
    ]
  }

--------------------------
-- reciprocal best hits --
--------------------------

-- TODO once this works, what should the actual fn people call look like?

reciprocal :: CutFunction
reciprocal = CutFunction
  { fName      = "reciprocal"
  , fTypeCheck = defaultTypeCheck [bht, bht] bht
  , fFixity    = Prefix
  , fRules  = rSimpleTmp aRecip "blast" bht
  }

aRecip :: ActionFn
aRecip cfg (CacheDir tmp) [ExprPath out, ExprPath left, ExprPath right] = do
  let out' = debugAction cfg "aRecip" out [tmp, out, left, right]
  unit $ quietly $ wrappedCmd cfg [out'] [Cwd tmp] "reciprocal.R" [out', left, right]
  debugTrackWrite cfg [out']
aRecip _ _ args = error $ "bad argument to aRecip: " ++ show args

-- TODO make the rest of them... but not until after lab meeting?
blastpRBH :: CutFunction
blastpRBH = CutFunction
  { fName = "blastp_rbh"
  , fTypeCheck = defaultTypeCheck [num, faa, faa] bht
  , fFixity = Prefix
  , fRules = rBlastpRBH
  }

-- it this works I'll be modeling new versions of the map ones after it
rBlastpRBH :: RulesFn
rBlastpRBH s@(_,cfg) e@(CutFun _ salt deps _ [evalue, lfaa, rfaa]) = do
  let lhits = CutFun bht salt deps "blastp"     [lfaa , rfaa , evalue]
      rhits = CutFun bht salt deps "blastp"     [rfaa , lfaa , evalue]
      lbest = CutFun bht salt deps "best_hits"  [lhits]
      rbest = CutFun bht salt deps "best_hits"  [rhits]
      rbh   = CutFun bht salt deps "reciprocal" [lbest, rbest]
      (ExprPath out) = exprPath cfg True e []
  (ExprPath rbhPath) <- rExpr s rbh -- TODO this is the sticking point right?
  out %> \_ -> aBlastpRBH cfg (cacheDir cfg "blast") [ExprPath out, ExprPath rbhPath]
  return (ExprPath out)
rBlastpRBH _ _ = error "bad argument to cBlastRBH"

-- this is an attempt to convert rBlastpRBH into a form usable with rMapLastTmp
aBlastpRBH :: ActionFn
aBlastpRBH cfg _ [ExprPath out, ExprPath rbhPath] = do
  need [rbhPath]
  let out' = debugAction cfg "aBlastpRBH" out [out, rbhPath]
  unit $ quietly $ wrappedCmd cfg [out'] [] "ln" ["-fs", rbhPath, out']
  debugTrackWrite cfg [out']
aBlastpRBH _ _ args = error $ "bad arguments to aBlastpRBH: " ++ show args

---------------------------------------------
-- kludge for mapping reciprocal best hits --
---------------------------------------------

-- TODO remove?
reciprocalEach :: CutFunction
reciprocalEach = CutFunction
  { fName      = "reciprocal_each"
  , fTypeCheck = defaultTypeCheck [bht, SetOf bht] (SetOf bht)
  , fFixity    = Prefix
  , fRules  = rRecipEach
  }

-- TODO how to hook this up to blastp_each?
rRecipEach :: RulesFn
rRecipEach s@(_,cfg) e@(CutFun _ _ _ _ [lbhts, rbhts]) = do
  (ExprPath lsPath) <- rExpr s lbhts
  (ExprPath rsPath) <- rExpr s rbhts
  let (ExprPath oPath) = exprPath cfg True e []
      (CacheDir cDir ) = cacheDir cfg "reciprocal_each"
  oPath %> \_ -> aRecipEach cfg oPath lsPath rsPath cDir
  return (ExprPath oPath)
rRecipEach _ _ = error "bad argument to rRecipEach"

aRecipEach :: CutConfig -> FilePath -> FilePath -> FilePath -> FilePath -> Action ()
aRecipEach cfg oPath lsPath rsPath cDir = do
  need [lsPath, rsPath]
  let oPath' = debugAction cfg "aRecipEach" oPath [oPath, lsPath, rsPath, cDir]
  unit $ quietly $ wrappedCmd cfg [oPath'] [Cwd cDir] "reciprocal_each.py"
    [cDir, oPath', lsPath, rsPath]
  debugTrackWrite cfg [oPath']

-----------------------------------------------
-- the hard part: mapped reciprocal versions --
-----------------------------------------------


-- TODO find a way to fix this or replace it...

-- blastpRBHEach :: CutFunction
-- blastpRBHEach = CutFunction
--   { fName      = "blastp_rbh_each"
--   , fTypeCheck = defaultTypeCheck [num, faa, SetOf faa] (SetOf bht)
--   , fFixity    = Prefix
--   , fRules  = rBlastpRBHEach
--   }
-- 
-- -- TODO oh right, that might not be directly a list!
-- --      can this be done a cleaner way??
-- rBlastpRBHEach :: RulesFn
-- rBlastpRBHEach st@(_,cfg) expr@(CutFun _ salt _ _ [e, q, CutSet _ _ _ ss]) = do
-- -- rBlastpRBHEach st@(scr,cfg) expr@(CutFun _ salt _ _ [e, q, ss]) = do
--   -- let subjects = extractExprs scr ss
--   let exprs = map (\s -> CutFun bht salt (concatMap depsOf [e, q, s]) "blastp_rbh" [e, q, s]) ss
--   paths <- mapM (rExpr st) exprs
--   let (ExprPath out) = exprPath cfg True expr []
--       paths' = map (\(ExprPath p) -> p) paths
--   out %> \_ -> need paths' >> debugWriteLines cfg out paths' >> debugTrackWrite cfg [out]
--   return (ExprPath out)
-- rBlastpRBHEach _ _ = error "bad argument to rBlastpRBHEach"


blastpRBHEach :: CutFunction
blastpRBHEach = CutFunction
  { fName      = "blastp_rbh_each"
  , fTypeCheck = defaultTypeCheck [num, faa, SetOf faa] (SetOf bht)
  , fFixity    = Prefix
  , fRules  = rBlastpRBHEach
  }


-- TODO how to remove all these files? will their mkBlast... take care of it?
rBlastpRBHEach :: RulesFn
rBlastpRBHEach s@(_,cfg) e@(CutFun rtn salt deps _ [evalue, query, subjects]) = do
  -- TODO need to get best_hits on each of the subjects before calling it, or duplicate the code inside?
  -- let mkExpr name = CutFun bht salt deps "best_hits" [CutFun bht salt deps name [evalue, query, subjects]]
  let mkExpr name = CutFun rtn salt deps name [evalue, query, subjects]
      (ExprPath oPath)  = exprPath cfg True e []
      (CacheDir cDir )  = cacheDir cfg "reciprocal_each"
  (ExprPath fwdsPath) <- rExpr s $ mkExpr "blastp_each"
  (ExprPath revsPath) <- rExpr s $ mkExpr "blastp_each_rev"
  oPath %> \_ -> aBlastpRBHEach cfg oPath cDir fwdsPath revsPath
  return (ExprPath oPath)
rBlastpRBHEach _ _ = error "bad argument to rRecipEach"

aBlastpRBHEach :: CutConfig -> FilePath -> FilePath -> FilePath -> FilePath -> Action ()
aBlastpRBHEach cfg oPath cDir fwdsPath revsPath = do
  need [fwdsPath, revsPath]
  liftIO $ createDirectoryIfMissing True cDir
  let oPath' = debugAction cfg "aBlastpRBHEach" oPath [oPath, cDir, fwdsPath, revsPath]
  unit $ quietly $ wrappedCmd cfg [oPath'] [Cwd cDir]
                       "reciprocal_each.py" [cDir, oPath', fwdsPath, revsPath]
  debugTrackWrite cfg [oPath']
