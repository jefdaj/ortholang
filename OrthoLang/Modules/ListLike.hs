module OrthoLang.Modules.ListLike where

-- TODO rename back to Length? or incorporate the ability to sample?
-- TODO what should happen with length of a bht? currently it just prints itself!
-- TODO make this the first typeclass
-- TODO if given a list with empty lists, should return zeros!
-- TODO account for the last empty line in mms files! (currently returns length + 1)

import Prelude hiding (length)

import OrthoLang.Types
import OrthoLang.Interpreter

import OrthoLang.Modules.Blast    (bht)
import OrthoLang.Modules.CRBBlast (crb)
import OrthoLang.Modules.MMSeqs   (mms)

olModule :: Module
olModule = Module
  { mName = "ListLike"
  , mDesc = "Operations on files that can be treated like lists"
  , mTypes = [bht, crb, mms]
  , mGroups = [ll]
  , mEncodings = []
  , mRules = []
  , mFunctions = [length, lengthEach]
  }

ll :: TypeGroup
ll = TypeGroup
  { tgExt  = "ll"
  , tgDesc  = "something list-like"
  , tgMembers = [ListSigs (AnyType "any type"), Exactly bht, Exactly crb, Exactly mms]
  }

length :: Function
length = newFnA1
  "length"
  (Some ll "a list-like type")
  (Exactly num)
  aLength
  []

lengthEach :: Function
lengthEach = newFnA1
  "length_each"
  (ListSigs $ Some ll "a list-like type")
  (Exactly $ ListOf num)
  (newMap1of1 "length")
  []

aLength :: NewAction1
aLength (ExprPath out') lst' = do
  let loc = "modules.listlike.aLength"
      out'' = traceA loc out' [out', lst']
  n <- countLines lst'
  writeLit loc out'' $ show n
