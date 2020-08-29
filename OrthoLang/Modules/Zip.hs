module OrthoLang.Modules.Zip
  where

-- import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter

import Prelude hiding (zip)
-- import Data.Maybe (fromJust)
import OrthoLang.Modules.Plots (listVarNames)
import OrthoLang.Locks (withReadLock)
import System.Process          (readProcess)

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
  , mFunctions = [zipArchive, zipArchiveExplicit]
  }

zip :: Type
zip = Type
  { tExt  = "zip"
  , tDesc = "zip archives"
  , tShow = \cfg ref path -> do
      path' <- resolveSymlinks Nothing path
      out <- withReadLock ref path' $ readProcess "zipinfo" [path'] []
      return $ toGeneric cfg out
  }

--------------------------
-- zip_archive_explicit --
--------------------------

-- | Hidden version of `zipArchive` that takes an explicit pre-loaded script and varnames file.
zipArchiveExplicit :: Function
zipArchiveExplicit = newFnS2
  "zip_archive_explicit"
  (ListSigs (Exactly str), ListSigs (Exactly Untyped))
  (Exactly zip)
  "zip_archive.py"
  [Hidden]
  id

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
  []

-- TODO rewrite Plots.hs functions to use expr expansions with varNames, like this
mZipArchive :: ExprExpansion
mZipArchive _ scr (Fun r ms ds _ [iList]) =
  let ns = listVarNames scr [iList]
  in Fun r ms ds "zip_archive_explicit" [ns, iList]
mZipArchive _ _ e = error "modules.zip.mZipArchive" $ "bad argument: " ++ show e
