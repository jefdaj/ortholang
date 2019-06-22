module ShortCut.Modules.ListLike where

-- TODO rename back to Length? or incorporate the ability to sample?
-- TODO what should happen with length of a bht? currently it just prints itself!
-- TODO make this the first typeclass

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Actions  (readPaths, writeLit, debugA)
-- import ShortCut.Core.Debug    (debugA)
import ShortCut.Core.Paths    (exprPath, fromCutPath,
                               toCutPath, CutPath)
import ShortCut.Core.Compile.Basic     (rExpr, defaultTypeCheck)
import ShortCut.Core.Compile.Map     (rMap)
import ShortCut.Modules.Blast    (bht)
import ShortCut.Modules.CRBBlast (crb)
import ShortCut.Modules.MMSeqs   (mms)
import Data.Scientific (Scientific())

cutModule :: CutModule
cutModule = CutModule
  { mName = "ListLike"
  , mDesc = "Operations on files that can be treated like lists"
  , mTypes = [bht, crb, mms, listlike]
  , mFunctions = [len, lenEach]
  }

listlike :: CutType
listlike = CutTypeGroup
  { tgExt  = "listlike"
  , tgDesc   = "files that can be treated like lists"
  , tgMember = tListLike
  }

tListLike :: CutType -> Bool
tListLike Empty      = True
tListLike (ListOf _) = True
tListLike x = x `elem` [bht, crb, mms]

-- can't name it length because that's a standard Haskell function
len :: CutFunction
len = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [listlike] num
  , fTypeDesc  = mkTypeDesc name  [listlike] num
  , fDesc      = Nothing
  , fFixity    = Prefix
  , fRules     = rLen
  }
  where
    name = "length"

lenEach :: CutFunction
lenEach = CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc name [(ListOf listlike)] (ListOf num)
  , fTypeCheck = defaultTypeCheck [ListOf listlike] (ListOf num)
  , fDesc      = Nothing
  , fFixity    = Prefix
  , fRules     = rMap 1 aLen -- TODO is 1 wrong?
  }
  where
    name = "length_each"

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
