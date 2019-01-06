module Detourrr.Modules.Length where

-- TODO what should happen with length of a bht? currently it just prints itself!

import Development.Shake
import Detourrr.Core.Types

import Detourrr.Core.Actions  (readPaths, writeLit, debugA)
-- import Detourrr.Core.Debug    (debugA)
import Detourrr.Core.Paths    (exprPath, fromDtrPath,
                               toDtrPath, DtrPath)
import Detourrr.Core.Compile.Basic     (rExpr)
import Detourrr.Core.Compile.Map     (rMap)
import Detourrr.Modules.Blast  (bht)
import Detourrr.Modules.CRBBlast (crb)
import Data.Scientific (Scientific())

dtrModule :: DtrModule
dtrModule = DtrModule
  { mName = "Length"
  , mDesc = "Get the lengths of lists and tables without printing them"
  , mTypes = [bht, crb]
  , mFunctions = [len, lenEach]
  }

-- can't name it length because that's a standard Haskell function
len :: DtrFunction
len = DtrFunction
  { fName      = "length"
  , fTypeCheck = tLen
  , fDesc = Nothing, fTypeDesc  = "length : X.list -> num"
  , fFixity    = Prefix
  , fRules  = rLen
  }

lenEach :: DtrFunction
lenEach = DtrFunction
  { fName      = "length_each"
  , fTypeDesc  = "length : X.list.list -> num.list"
  , fTypeCheck = tLenEach
  , fDesc      = Nothing
  , fFixity    = Prefix
  , fRules     = rMap 1 aLen -- TODO is 1 wrong?
  }

tLen :: [DtrType] -> Either String DtrType
tLen [Empty ] = Right num
tLen [(ListOf _)] = Right num
tLen [x] | x == bht = Right num
tLen _ = Left $ "length requires a list"

rLen :: DtrState -> DtrExpr -> Rules ExprPath
rLen s@(_, cfg, ref, ids) e@(DtrFun _ _ _ _ [l]) = do
  (ExprPath lPath) <- rExpr s l
  -- TODO once all modules are converted, add back phantom types!
  -- let relPath = makeRelative (cfgTmpDir cfg) lPath
  -- (ExprPath outPath) = exprPathExplicit cfg True num "length" [relPath]
  let outPath = exprPath s e
      out'    = fromDtrPath cfg outPath
      lPath'  = toDtrPath   cfg lPath
  out' %> \_ -> aLen cfg ref ids [outPath, lPath']
  return (ExprPath out')
rLen _ _ = error "bad arguments to rLen"

tLenEach :: [DtrType] -> Either String DtrType
tLenEach [ ListOf  Empty     ] = Right (ListOf num) -- specifically, []
tLenEach [(ListOf (ListOf _))] = Right (ListOf num)
tLenEach [ListOf  x] | x `elem` [bht, crb] = Right (ListOf num)
tLenEach _ = Left $ "length_each requires a list of things with lengths"

-- TODO if given a list with empty lists, should return zeros!
aLen :: DtrConfig -> Locks -> HashedSeqIDsRef -> [DtrPath] -> Action ()
aLen cfg ref _ [out, lst] = do
  let count ls = read (show $ length ls) :: Scientific
  n <- fmap count $ readPaths cfg ref lst'
  writeLit cfg ref out'' $ show n
  where
    out'  = fromDtrPath cfg out
    lst'  = fromDtrPath cfg lst
    out'' = debugA cfg "aLen" out' [out', lst']
aLen _ _ _ args = error $ "bad arguments to aLen: " ++ show args
