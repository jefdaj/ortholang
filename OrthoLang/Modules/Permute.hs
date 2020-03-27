module OrthoLang.Modules.Permute where

-- TODO put this in Core because with the salts it's not separable

import Development.Shake
import OrthoLang.Core.Types
import OrthoLang.Core.Util (trace)
-- import OrthoLang.Core.Config (debug)

import Development.Shake.FilePath   (makeRelative)
import OrthoLang.Core.Compile  (rExpr)
import OrthoLang.Core.Actions (readStrings, writeStrings, traceA, need')
import OrthoLang.Core.Paths (exprPath, exprPathExplicit, fromPath)
-- import OrthoLang.Core.Debug          (traceA, debug)
import OrthoLang.Core.Util           (digest)

orthoLangModule :: Module
orthoLangModule = Module
  { mName = "Permute"
  , mDesc = "Generate random permutations of lists"
  , mTypes = []
  , mFunctions =
    [ leaveEachOut
    -- TODO sample n elements n times
    -- TODO partition into training + test data
    -- TODO partition into trainint + test data for cross-validation 
    ]
  }

--------------------------------------
-- generic plumbing for permute fns --
--------------------------------------

-- splits a list into a list of lists using the provided function
-- TODO produce each output list in a separate Shake monad section?
-- TODO are paths hashes unique now??
--      (if it turns out to be re-running stuff unneccesarily)
rPermute :: ([String] -> [[String]])
         -> RulesFn
rPermute comboFn s@(_, cfg, _, _) expr@(Fun _ salt _ _ [iList]) = do
  (ExprPath iPath) <- rExpr s iList
  let oList      = fromPath cfg $ exprPath s expr
      (ListOf t) = typeOf iList
  oList %> aPermute s comboFn iPath t salt
  return (ExprPath oList)
rPermute _ _ _ = fail "bad argument to rCombos"

-- TODO once back-compilation or whatever works, also use it here?
-- TODO do something more obvious than writing to the "list" prefix??
aPermute :: GlobalEnv
         -> ([String] -> [[String]])
         -> FilePath -> Type -> Salt
         -> FilePath -> Action ()
aPermute (_, cfg, ref, _) comboFn iPath eType salt out = do
  need' cfg ref "ortholang.modules.permute.aPermute" [iPath]
  elements <- readStrings eType cfg ref iPath
  -- TODO these aren't digesting properly! elements need to be compiled first?
  --      (digesting the elements themselves rather than the path to them)
  -- TODO will this match other files?
  let mkOut p = exprPathExplicit cfg "list" (ListOf eType) salt [digest $ makeRelative (cfgTmpDir cfg) p]
      oPaths  = map mkOut elements
      oPaths' = map (fromPath cfg) oPaths
      combos  = comboFn elements
  -- TODO traceA instead here?
  mapM_ (\(p,ps) -> writeStrings eType cfg ref p $
                      trace "modules.permute.aPermute"
                               ("combo: " ++ show ps) ps)
                               (zip oPaths' combos)
  let out' = traceA "aPermute" out [iPath, extOf eType, out]
  writeStrings (ListOf eType) cfg ref out' oPaths'

--------------------
-- leave_each_out --
--------------------

-- TODO rename actual function to drop_each?

leaveEachOut :: Function
leaveEachOut = let name = "leave_each_out" in Function
  { fOpChar = Nothing, fName = name 
  ,fTags = []
  , fTypeCheck = combosTypeCheck
  , fTypeDesc  = name ++ " : X.list -> X.list.list"
  , fNewRules = Nothing, fOldRules = rPermute dropEach
  }

combosTypeCheck :: [Type] -> Either String Type
combosTypeCheck [ListOf t] = Right $ ListOf $ ListOf t
combosTypeCheck _ = Left "type error in leave_each_out!"

-- drops the element at index n from a list
-- (for some reason this isn't a built-in function?)
-- TODO make sure this actually covers all cases!
dropFromList :: [a] -> Int -> [a]
dropFromList xs n
  | n < 1 || n > length xs = error "attmpt to drop nonexistent list element"
  | otherwise = before ++ after
  where
    (before, (_:after)) = splitAt (n-1) xs

-- returns a list of lists where each element is left out once
-- TODO should it be an error to call this with only one element?
dropEach :: [a] -> [[a]]
dropEach xs
  | length xs < 2 = []
  | otherwise = map (dropFromList xs) [1..length xs]
