module Detourrr.Modules.Length where

-- TODO what should happen with length of a bht? currently it just prints itself!
-- TODO make this the first typeclass

import Development.Shake
import Detourrr.Core.Types

import Detourrr.Core.Actions  (readPaths, writeLit, debugA)
-- import Detourrr.Core.Debug    (debugA)
import Detourrr.Core.Paths    (exprPath, fromRrrPath,
                               toRrrPath, RrrPath)
import Detourrr.Core.Compile.Basic     (rExpr)
import Detourrr.Core.Compile.Map     (rMap)
import Detourrr.Modules.Blast    (bht)
import Detourrr.Modules.MMSeqs   (mms)
import Detourrr.Modules.CRBBlast (crb)
import Data.Scientific (Scientific())

rrrModule :: RrrModule
rrrModule = RrrModule
  { mName = "Length"
  , mDesc = "Get the lengths of lists and tables without printing them"
  , mTypes = [bht, crb, mms]
  , mFunctions = [len, lenEach]
  }

-- can't name it length because that's a standard Haskell function
len :: RrrFunction
len = RrrFunction
  { fName      = "length"
  , fTypeCheck = tLen
  , fDesc = Nothing, fTypeDesc  = "length : X.list -> num"
  , fFixity    = Prefix
  , fRules  = rLen
  }

lenEach :: RrrFunction
lenEach = RrrFunction
  { fName      = "length_each"
  , fTypeDesc  = "length : X.list.list -> num.list"
  , fTypeCheck = tLenEach
  , fDesc      = Nothing
  , fFixity    = Prefix
  , fRules     = rMap 1 aLen -- TODO is 1 wrong?
  }

tLen :: [RrrType] -> Either String RrrType
tLen [Empty ] = Right num
tLen [(ListOf _)] = Right num
tLen [x] | x `elem` [bht, mms] = Right num
tLen _ = Left $ "length requires a list"

rLen :: RrrState -> RrrExpr -> Rules ExprPath
rLen s@(_, cfg, ref, ids) e@(RrrFun _ _ _ _ [l]) = do
  (ExprPath lPath) <- rExpr s l
  -- TODO once all modules are converted, add back phantom types!
  -- let relPath = makeRelative (cfgTmpDir cfg) lPath
  -- (ExprPath outPath) = exprPathExplicit cfg True num "length" [relPath]
  let outPath = exprPath s e
      out'    = fromRrrPath cfg outPath
      lPath'  = toRrrPath   cfg lPath
  out' %> \_ -> aLen cfg ref ids [outPath, lPath']
  return (ExprPath out')
rLen _ _ = error "bad arguments to rLen"

tLenEach :: [RrrType] -> Either String RrrType
tLenEach [ ListOf  Empty     ] = Right (ListOf num) -- specifically, []
tLenEach [(ListOf (ListOf _))] = Right (ListOf num)
tLenEach [ListOf  x] | x `elem` [bht, crb, mms] = Right (ListOf num)
tLenEach _ = Left $ "length_each requires a list of things with lengths"

-- TODO if given a list with empty lists, should return zeros!
-- TODO account for the last empty line in mms files! (currently returns length + 1)
aLen :: RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ()
aLen cfg ref _ [out, lst] = do
  let count ls = read (show $ length ls) :: Scientific
  n <- fmap count $ readPaths cfg ref lst'
  writeLit cfg ref out'' $ show n
  where
    out'  = fromRrrPath cfg out
    lst'  = fromRrrPath cfg lst
    out'' = debugA cfg "aLen" out' [out', lst']
aLen _ _ _ args = error $ "bad arguments to aLen: " ++ show args
