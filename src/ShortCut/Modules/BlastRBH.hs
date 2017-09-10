module ShortCut.Modules.BlastRBH where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Paths     (exprPath, cacheDir)
import ShortCut.Core.Compile   (cExpr)
import ShortCut.Core.Config    (wrappedCmd)
import ShortCut.Core.Debug     (debugTrackWrite)
import ShortCut.Core.ModuleAPI (rSimpleTmp, defaultTypeCheck)
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
  , fCompiler  = rSimpleTmp aRecip "blast" bht
  }

aRecip :: ActionFn
aRecip cfg (CacheDir tmp) [ExprPath out, ExprPath left, ExprPath right] = do
  unit $ quietly $ wrappedCmd cfg [out] [Cwd tmp] "reciprocal.R" [out, left, right]
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
cBlastpRBH :: RulesFn
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
aBlastpRBH :: ActionFn
aBlastpRBH cfg _ [ExprPath out, ExprPath rbhPath] =
  unit $ quietly $ wrappedCmd cfg [out] [] "ln" ["-fs", rbhPath, out]
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
  , fCompiler  = cRecipEach
  }

-- TODO how to hook this up to blastp_each?
cRecipEach :: RulesFn
cRecipEach s@(_,cfg) e@(CutFun _ _ _ _ [lbhts, rbhts]) = do
  (ExprPath lsPath) <- cExpr s lbhts
  (ExprPath rsPath) <- cExpr s rbhts
  let (ExprPath oPath) = exprPath cfg e []
      (CacheDir cDir ) = cacheDir cfg "reciprocal_each"
  oPath %> \_ -> do
    need [lsPath, rsPath]
    unit $ quietly $ wrappedCmd cfg [oPath] [Cwd cDir] "reciprocal_each.py"
      [cDir, oPath, lsPath, rsPath]
    debugTrackWrite cfg [oPath]
  return (ExprPath oPath)
cRecipEach _ _ = error "bad argument to cRecipEach"

-----------------------------------------------
-- the hard part: mapped reciprocal versions --
-----------------------------------------------


-- TODO find a way to fix this or replace it...

-- blastpRBHEach :: CutFunction
-- blastpRBHEach = CutFunction
--   { fName      = "blastp_rbh_each"
--   , fTypeCheck = defaultTypeCheck [num, faa, SetOf faa] (SetOf bht)
--   , fFixity    = Prefix
--   , fCompiler  = cBlastpRBHEach
--   }
-- 
-- -- TODO oh right, that might not be directly a list!
-- --      can this be done a cleaner way??
-- cBlastpRBHEach :: RulesFn
-- cBlastpRBHEach st@(_,cfg) expr@(CutFun _ salt _ _ [e, q, CutSet _ _ _ ss]) = do
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
  , fTypeCheck = defaultTypeCheck [num, faa, SetOf faa] (SetOf bht)
  , fFixity    = Prefix
  , fCompiler  = cBlastpRBHEach
  }


-- TODO how to remove all these files? will their mkBlast... take care of it?
cBlastpRBHEach :: RulesFn
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
    unit $ quietly $ wrappedCmd cfg [oPath] [Cwd cDir]
                       "reciprocal_each.py" [cDir, oPath, fwdsPath, revsPath]
    debugTrackWrite cfg [oPath]
  return (ExprPath oPath)
cBlastpRBHEach _ _ = error "bad argument to cRecipEach"
