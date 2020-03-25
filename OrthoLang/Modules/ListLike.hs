module OrthoLang.Modules.ListLike where

-- TODO rename back to Length? or incorporate the ability to sample?
-- TODO what should happen with length of a bht? currently it just prints itself!
-- TODO make this the first typeclass

import Development.Shake
import OrthoLang.Core.Types

import OrthoLang.Core.Actions  (readPaths, writeLit, traceA)
-- import OrthoLang.Core.Debug    (traceA)
import OrthoLang.Core.Paths    (exprPath, fromOrthoLangPath,
                               toOrthoLangPath, OrthoLangPath)
import OrthoLang.Core.Compile     (rExpr, defaultTypeCheck)
import OrthoLang.Core.Compile     (rMap)
import OrthoLang.Modules.Blast    (bht)
import OrthoLang.Modules.CRBBlast (crb)
import OrthoLang.Modules.MMSeqs   (mms)
import Data.Scientific (Scientific())

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "ListLike"
  , mDesc = "Operations on files that can be treated like lists"
  , mTypes = [bht, crb, mms, listlike]
  , mFunctions = [len, lenEach]
  }

listlike :: OrthoLangType
listlike = OrthoLangTypeGroup
  { tgExt  = "listlike"
  , tgDesc   = "files that can be treated like lists"
  , tgMember = tListLike
  }

tListLike :: OrthoLangType -> Bool
tListLike Empty      = True
tListLike (ListOf _) = True
tListLike x = x `elem` [bht, crb, mms]

-- can't name it length because that's a standard Haskell function
len :: OrthoLangFunction
len = OrthoLangFunction
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [listlike] num
  , fTypeDesc  = mkTypeDesc name  [listlike] num
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rLen
  }
  where
    name = "length"

lenEach :: OrthoLangFunction
lenEach = OrthoLangFunction
  { fOpChar = Nothing, fName = name
  , fTypeDesc  = mkTypeDesc name [(ListOf listlike)] (ListOf num)
  , fTypeCheck = defaultTypeCheck [ListOf listlike] (ListOf num)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMap 1 aLen -- TODO is 1 wrong?
  }
  where
    name = "length_each"

rLen :: RulesFn
rLen s@(_, cfg, ref, ids) e@(OrthoLangFun _ _ _ _ [l]) = do
  (ExprPath lPath) <- rExpr s l
  -- TODO once all modules are converted, add back phantom types!
  -- let relPath = makeRelative (cfgTmpDir cfg) lPath
  -- (ExprPath outPath) = exprPathExplicit cfg True num "length" [relPath]
  let outPath = exprPath s e
      out'    = fromOrthoLangPath cfg outPath
      lPath'  = toOrthoLangPath   cfg lPath
  out' %> \_ -> aLen cfg ref ids [outPath, lPath']
  return (ExprPath out')
rLen _ _ = fail "bad arguments to rLen"

-- TODO if given a list with empty lists, should return zeros!
-- TODO account for the last empty line in mms files! (currently returns length + 1)
aLen :: OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ()
aLen cfg ref _ [out, lst] = do
  let count ls = read (show $ length ls) :: Scientific
  n <- fmap count $ readPaths cfg ref lst'
  writeLit cfg ref out'' $ show n
  where
    out'  = fromOrthoLangPath cfg out
    lst'  = fromOrthoLangPath cfg lst
    out'' = traceA "aLen" out' [out', lst']
aLen _ _ _ args = error $ "bad arguments to aLen: " ++ show args
