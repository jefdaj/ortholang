module OrthoLang.Modules.Zip
  where

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter

-- import OrthoLang.Debug (error)
import Prelude hiding (zip)
import Data.Maybe (fromJust)
-- import OrthoLang.Modules.Load (mkLoad)
import OrthoLang.Modules.Plots (listVarNames)

------------
-- module --
------------

olModule :: Module
olModule = Module
  { mName = "Zip"
  , mDesc = "Archive lists of variables"
  , mTypes = [zip]
  , mGroups = []
  , mEncodings = []
  , mFunctions = [zipArchive]
  }

zip :: Type
zip = Type
  { tExt  = "zip"
  , tDesc = "zip archives"
  , tShow = defaultShow -- TODO use zipinfo
  }

-----------------
-- zip_archive --
-----------------

-- | Hidden version of `zipArchive` that takes an explicit pre-loaded script and varnames file.
-- TODO with bin hash?
-- TODO also pass types explicitly?
-- TODO https://medium.com/@pat_wilson/building-deterministic-zip-files-with-built-in-commands-741275116a19
zipArchiveExplicit :: Function
zipArchiveExplicit = newFnA2
  "zip_archive_explicit"
  (ListSigs (Exactly str), ListSigs (Exactly Untyped))
  (Exactly zip)
  aZipArchiveExplicit
  [Hidden, Nondeterministic]

-- TODO have to delete the script first if it exists? why doesn't ortholang do that?
aZipArchiveExplicit :: NewAction2
aZipArchiveExplicit (ExprPath out) inNames inList = do
  cfg <- fmap fromJust $ getShakeExtra
  let loc  = "modules.customscript.aZipArchiveExplicit"
      out' = toPath loc cfg out
  withBinHash out out' $ \tmpPath -> do
    let tmp' = fromPath loc cfg tmpPath
    aNewRulesS2 "zip_archive.sh" id (ExprPath tmp') inNames inList

-----------------
-- zip_archive --
-----------------

-- | User-facing version that auto-loads the script and captures any varnames in the untyped list.
zipArchive :: Function
zipArchive = newExprExpansion
  "zip_archive"
  [ListSigs (Exactly Untyped)]
  (Exactly zip)
  mZipArchive
  [ReadsFile, Nondeterministic]

-- TODO rewrite Plots.hs functions to use expr expansions with varNames, like this
mZipArchive :: ExprExpansion
mZipArchive _ scr (Fun r _ ds _ [iList]) =
  let ns = listVarNames scr [iList]
  in Fun r Nothing ds "zip_archive_explicit" [ns, iList]
mZipArchive _ _ e = error "modules.zip.mZipArchive" $ "bad argument: " ++ show e
