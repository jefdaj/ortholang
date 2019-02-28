module ShortCut.Modules.Length where

-- TODO what should happen with length of a bht? currently it just prints itself!
-- TODO make this the first typeclass

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Actions  (readPaths, writeLit, debugA)
-- import ShortCut.Core.Debug    (debugA)
import ShortCut.Core.Paths    (exprPath, fromCutPath,
                               toCutPath, CutPath)
import ShortCut.Core.Compile.Basic     (rExpr)
import ShortCut.Core.Compile.Map     (rMap)
import ShortCut.Modules.Blast    (bht)
import ShortCut.Modules.MMSeqs   (mms)
import ShortCut.Modules.CRBBlast (crb)
import Data.Scientific (Scientific())

cutModule :: CutModule
cutModule = CutModule
  { mName = "Length"
  , mDesc = "Get the lengths of lists and tables without printing them"
  , mTypes = [bht, crb, mms]
  , mFunctions = [len, lenEach]
  }

-- can't name it length because that's a standard Haskell function
len :: CutFunction
len = CutFunction
  { fName      = "length"
  , fTypeCheck = tLen
  , fDesc = Nothing, fTypeDesc  = "length : X.list -> num"
  , fFixity    = Prefix
  , fRules  = rLen
  }

lenEach :: CutFunction
lenEach = CutFunction
  { fName      = "length_each"
  , fTypeDesc  = "length : X.list.list -> num.list"
  , fTypeCheck = tLenEach
  , fDesc      = Nothing
  , fFixity    = Prefix
  , fRules     = rMap 1 aLen -- TODO is 1 wrong?
  }

tLen :: [CutType] -> Either String CutType
tLen [Empty ] = Right num
tLen [(ListOf _)] = Right num
tLen [x] | x `elem` [bht, mms] = Right num
tLen _ = Left $ "length requires a list"

rLen :: CutState -> CutExpr -> Rules ExprPath
rLen s@(_, cfg, ref, ids) e@(CutFun _ _ _ _ [l]) = do
  (ExprPath lPath) <- rExpr s l
  -- TODO once all modules are converted, add back phantom types!
  -- let relPath = makeRelative (cfgTmpDir cfg) lPath
  -- (ExprPath outPath) = exprPathExplicit cfg True num "length" [relPath]
  let outPath = exprPath s e
      out'    = fromCutPath cfg outPath
      lPath'  = toCutPath   cfg lPath
  out' %> \_ -> aLen cfg ref ids [outPath, lPath']
  return (ExprPath out')
rLen _ _ = fail "bad arguments to rLen"

tLenEach :: [CutType] -> Either String CutType
tLenEach [ ListOf  Empty     ] = Right (ListOf num) -- specifically, []
tLenEach [(ListOf (ListOf _))] = Right (ListOf num)
tLenEach [ListOf  x] | x `elem` [bht, crb, mms] = Right (ListOf num)
tLenEach _ = Left $ "length_each requires a list of things with lengths"

-- TODO if given a list with empty lists, should return zeros!
-- TODO account for the last empty line in mms files! (currently returns length + 1)
aLen :: CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()
aLen cfg ref _ [out, lst] = do
  let count ls = read (show $ length ls) :: Scientific
  n <- fmap count $ readPaths cfg ref lst'
  writeLit cfg ref out'' $ show n
  where
    out'  = fromCutPath cfg out
    lst'  = fromCutPath cfg lst
    out'' = debugA cfg "aLen" out' [out', lst']
aLen _ _ _ args = error $ "bad arguments to aLen: " ++ show args
