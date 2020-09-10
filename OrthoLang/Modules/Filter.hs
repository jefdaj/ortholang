module OrthoLang.Modules.Filter where

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter

-- import Data.Maybe              (fromJust)
import Data.Char               (toLower)
import Data.List               (isInfixOf)

---------------
-- debugging --
---------------

debugA' :: String -> String -> Action ()
debugA' name = debugA ("modules.filter." ++ name)

debugR' :: (Pretty a, Show b) => Config -> String -> a -> b -> b
debugR' _ name = debugRules ("modules.filter." ++ name)

------------
-- module --
------------

olModule :: Module
olModule = Module
  { mName = "Filter"
  , mDesc = "Filter lists"
  , mTypes = []
  , mGroups = []
  , mEncodings = []
  , mRules = []
  , mFunctions = [filterList]
  }

-- TODO flip the args?
filterList :: Function
filterList = newFnA2
  "filter_list"
  (Exactly $ ListOf str, Exactly str)
  (Exactly $ ListOf str)
  aFilterList
  []

filterNames :: String -> [String] -> [String]
filterNames s cs = filter matchFn cs
  where
    matchFn c = (map toLower s) `isInfixOf` (map toLower c)

aFilterList :: NewAction2
aFilterList (ExprPath oPath') listTmp' fPath' = do
  let loc = "modules.blastdb.aFilterList"
      oPath''  = traceA loc oPath' [oPath', listTmp', fPath']
  filterStr <- readLit  loc fPath'
  out       <- readLits loc listTmp'
  let elems  = if null out then [] else tail out
      elems' = if null filterStr then elems else filterNames filterStr elems
  debugA' loc $ "elems': " ++ show elems'
  writeLits loc oPath'' elems'
