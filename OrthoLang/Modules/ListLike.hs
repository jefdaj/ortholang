module OrthoLang.Modules.ListLike where

-- TODO rename back to Length? or incorporate the ability to sample?
-- TODO what should happen with length of a bht? currently it just prints itself!
-- TODO make this the first typeclass

import Prelude hiding (length)
import qualified Prelude as P

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter

import OrthoLang.Modules.Blast    (bht)
import OrthoLang.Modules.CRBBlast (crb)
import OrthoLang.Modules.MMSeqs   (mms)
import Data.Scientific            (Scientific())
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "ListLike"
  , mDesc = "Operations on files that can be treated like lists"
  , mTypes = [bht, crb, mms]
  , mGroups = [ll]
  , mEncodings = []
  , mFunctions = [length, lengthEach]
  }

ll :: TypeGroup
ll = TypeGroup
  { tgExt  = "ll"
  , tgDesc  = "files that can be treated like lists"
  , tgMembers = [ListSigs (AnyType "any type"), Exactly bht, Exactly crb, Exactly mms]
  }

length :: Function
length = Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [Some ll "something list-like"] num
  -- , fTypeDesc  = mkTypeDesc       name [Some ll "something list-like"] num
  , fInputs = [Some ll "something list-like"]
  , fOutput = Exactly num
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rLength
  }
  where
    name = "length"

-- tLength is (Some listlike "any list-like file") num
-- shown as "ll -> num, where ll is any list-like file"

-- tLengthEach is (ListOf (Some listlike "any list-like file")) (ListOf num)
-- shown as "ll.list -> num.list, where ll is any list-like file"

lengthEach :: Function
lengthEach = Function
  { fOpChar = Nothing, fName = name
  -- , fTypeDesc  = mkTypeDesc       name [ListOf (Some ll "something list-like")] (ListOf num)
  -- , fTypeCheck = defaultTypeCheck name [ListOf (Some ll "something list-like")] (ListOf num)
  , fInputs = [ListSigs (Some ll "something list-like")]
  , fOutput = Exactly (ListOf num)
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMap 1 aLength -- TODO is 1 wrong?
  }
  where
    name = "length_each"

rLength :: RulesFn
rLength scr e@(Fun _ _ _ _ [l]) = do
  (ExprPath lPath) <- rExpr scr l
  -- TODO once all modules are converted, add back phantom types!
  -- let relPath = makeRelative (cfgTmpDir cfg) lPath
  -- (ExprPath outPath) = unsafeExprPathExplicit cfg True num "length" [relPath]
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let loc = "modules.listlike.rLength"
      outPath = exprPath cfg dRef scr e
      out'    = fromPath loc cfg outPath
      lPath'  = toPath loc cfg lPath
  out' %> \_ -> aLength [outPath, lPath']
  return (ExprPath out')
rLength _ _ = fail "bad arguments to rLength"

-- TODO if given a list with empty lists, should return zeros!
-- TODO account for the last empty line in mms files! (currently returns length + 1)
aLength :: [Path] -> Action ()
aLength [out, lst] = do
  cfg  <- fmap fromJust getShakeExtra
  let out'  = fromPath loc cfg out
      lst'  = fromPath loc cfg lst
      loc = "modules.listlike.aLength"
      out'' = traceA loc out' [out', lst']
  n <- countLines lst'
  writeLit loc out'' $ show n
aLength args = error $ "bad arguments to aLength: " ++ show args
