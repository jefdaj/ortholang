module OrthoLang.Modules.Permute where

-- TODO put this in Core because with the salts it's not separable

import Development.Shake
import OrthoLang.Core
import Development.Shake.FilePath (makeRelative)
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "Permute"
  , mDesc = "Generate random permutations of lists"
  , mTypes = []
  , mGroups = []
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
rPermute :: ([String] -> [[String]]) -> RulesFn
rPermute comboFn scr expr@(Fun _ salt _ _ [iList]) = do
  (ExprPath iPath) <- rExpr scr iList
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let oList      = fromPath cfg $ exprPath cfg dRef scr expr
      (ListOf t) = typeOf iList
  oList %> aPermute comboFn iPath t salt
  return (ExprPath oList)
rPermute _ _ _ = fail "bad argument to rCombos"

-- TODO once back-compilation or whatever works, also use it here?
-- TODO do something more obvious than writing to the "list" prefix??
aPermute :: ([String] -> [[String]])
         -> FilePath -> Type -> Salt
         -> FilePath -> Action ()
aPermute comboFn iPath eType salt out = do
  need' "ortholang.modules.permute.aPermute" [iPath]
  cfg  <- fmap fromJust getShakeExtra
  elements <- readStrings eType iPath
  dRef <- fmap fromJust getShakeExtra
  let mkOut p = unsafeExprPathExplicit cfg dRef "list" (ListOf eType) salt [digest $ makeRelative (cfgTmpDir cfg) p]
      oPaths  = map mkOut elements
      oPaths' = map (fromPath cfg) oPaths
      combos  = comboFn elements
  -- TODO traceA instead here?
  mapM_ (\(p,ps) -> writeStrings eType p $
                      trace "modules.permute.aPermute"
                               ("combo: " ++ show ps) ps)
                               (zip oPaths' combos)
  let out' = traceA "aPermute" out [iPath, tExtOf eType, out]
  writeStrings (ListOf eType) out' oPaths'

--------------------
-- leave_each_out --
--------------------

-- TODO rename actual function to drop_each?

leaveEachOut :: Function
leaveEachOut = let name = "leave_each_out" in Function
  { fOpChar = Nothing, fName = name 
  , fTags = []
  -- , fTypeCheck = combosTypeCheck
  -- , fTypeDesc  = name ++ " : X.list -> X.list.list"
  , fInputs = [ListSigs (AnyType "any type")]
  , fOutput =  ListSigs (AnyType "any type")
  , fNewRules = NewNotImplemented
  , fOldRules = rPermute dropEach
  }

-- combosTypeCheck :: [Type] -> Either String Type
-- combosTypeCheck [ListOf t] = Right $ ListOf $ ListOf t
-- combosTypeCheck _ = Left "type error in leave_each_out!"

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
