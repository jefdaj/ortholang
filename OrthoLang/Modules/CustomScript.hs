module OrthoLang.Modules.CustomScript where

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter

import OrthoLang.Debug (error)
import Prelude hiding (error)
import Data.Maybe (fromJust)
import OrthoLang.Modules.Load (mkLoad)


olModule :: Module
olModule = Module
  { mName = "CustomScript"
  , mDesc = "Run your own script and OrthoLang will assume the types are valid"
  , mTypes = [scr]
  , mGroups = []
  , mEncodings = []
  , mFunctions = [loadScript, runScriptRaw, runScript]
  }

scr :: Type
scr = Type
  { tExt  = "scr"
  , tDesc = "custom scripts"
  , tShow = defaultShow -- TODO what if it's binary? maybe use file command to show?
  }

-----------------
-- load_script --
-----------------

loadScript :: Function
loadScript = mkLoad False "load_script" (Exactly scr)

--------------------
-- run_script_raw --
--------------------

runScriptRaw :: Function
runScriptRaw = hidden $ newFnA2
  "run_script_raw"
  (Exactly scr, ListSigs (Exactly Untyped))
  (Exactly Untyped)
  aRunScriptRaw
  [Hidden]

aRunScriptRaw :: NewAction2
aRunScriptRaw (ExprPath out) inScr inList = do
  cfg <- fmap fromJust $ getShakeExtra
  let loc  = "modules.customscript.aRunScriptRaw"
      out' = toPath loc cfg out
  withBinHash out out' $ \tmpPath -> do
    let tmp' = fromPath loc cfg tmpPath
    aNewRulesS1 inScr id (ExprPath tmp') inList

----------------
-- run_script --
----------------

runScript :: Function
runScript = newExprExpansion
  "run_script"
  [Exactly str, ListSigs (Exactly Untyped)]
  (Exactly Untyped)
  mRunScript
  [ReadsFile]

mRunScript :: ExprExpansion
mRunScript _ _ (Fun r _ ds _ [sStr, iList]) =
  let f = Fun scr Nothing ds "load_script"    [sStr]
  in      Fun r   Nothing ds "run_script_raw" [f, iList]
mRunScript _ _ e = error "modules.customscript.mRunScript" $ "bad argument: " ++ show e
