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
-- rPermute :: ([String] -> [[String]]) -> RulesFn
-- rPermute comboFn scr expr@(Fun _ salt _ _ [iList]) = do
--   (ExprPath iPath) <- rExpr scr iList
--   cfg  <- fmap fromJust getShakeExtraRules
--   dRef <- fmap fromJust getShakeExtraRules
--   let oList = fromPath loc cfg $ exprPath cfg dRef scr expr
--   oList %> aPermute comboFn iPath salt
--   return (ExprPath oList)
-- rPermute _ _ _ = fail "bad argument to rCombos"

-- TODO once back-compilation or whatever works, also use it here?
-- TODO do something more obvious than writing to the "list" prefix??
aPermute :: ([String] -> [[String]]) -> NewAction1
aPermute comboFn (ExprPath out) iPath = do
  -- need' "ortholang.modules.permute.aPermute" [iPath]
  cfg  <- fmap fromJust getShakeExtra
  dRef <- fmap fromJust getShakeExtra
  let loc = "modules.permute.aPermute"
  (ListOf lt@(ListOf et)) <- liftIO $ decodeNewRulesType cfg dRef (ExprPath out)
  elements <- readStrings loc et iPath
  let mkOut p = unsafeExprPathExplicit cfg dRef "list" lt Nothing [digest $ makeRelative (cfgTmpDir cfg) p]
      oPaths  = map mkOut elements
      oPaths' = map (fromPath loc cfg) oPaths
      combos  = comboFn elements
  -- TODO traceA instead here?
  mapM_ (\(p,ps) -> writeStrings loc et p $
                      trace loc
                               ("combo: " ++ show ps) ps)
                               (zip oPaths' combos)
  let out' = traceA loc out [iPath, show lt, out]
  writeStrings loc lt out' oPaths'

--------------------
-- leave_each_out --
--------------------

-- TODO rename actual function to drop_each?

-- leaveEachOut :: Function
-- leaveEachOut = let name = "leave_each_out" in Function
--   { fOpChar = Nothing, fName = name 
--   , fTags = []
--   -- , fTypeCheck = combosTypeCheck
--   -- , fTypeDesc  = name ++ " : X.list -> X.list.list"
--   , fInputs = [ListSigs (AnyType "any type")]
--   , fOutput =  ListSigs (AnyType "any type")
--   , fNewRules = NewNotImplemented
--   , fOldRules = rPermute dropEach
--   }

leaveEachOut = newFnA1 "leave_each_out"
                 (ListSigs $            AnyType "any type")
                 (ListSigs $ ListSigs $ AnyType "any type")
                 (aPermute dropEach)
                 []

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
