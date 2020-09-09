module OrthoLang.Modules.Permute where

-- TODO put this in Core because with the seeds it's not separable

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "Permute"
  , mDesc = "Generate random permutations of lists"
  , mTypes = []
  , mGroups = []
  , mEncodings = []
  , mRules = []
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
-- TODO once back-compilation or whatever works, also use it here?
-- TODO do something more obvious than writing to the "list" prefix??
aPermute :: ([String] -> [[String]]) -> NewAction1
aPermute comboFn (ExprPath out) iPath = do
  cfg  <- fmap fromJust getShakeExtra
  dRef <- fmap fromJust getShakeExtra
  let loc = "modules.permute.aPermute"
  (ListOf lType@(ListOf eType)) <- liftIO $ decodeNewRulesType cfg dRef (ExprPath out)
  elements <- readStrings loc eType iPath
  let combos  = comboFn elements
  oPaths <- mapM (aPermuteElem eType) combos
  let oPaths' = map (fromPath loc cfg) oPaths
  let out' = traceA loc out [iPath, show lType, out]
  writeStrings loc lType out' oPaths' -- then finally a list of (list of element type)

-- takes a single comboFn result (which is a list of something), writes the
-- list, and returns the expr path to add to the main output list. also adds
-- the expr path to the digest map
aPermuteElem :: Type -> [String] -> Action Path
aPermuteElem eType elements = do
  cfg  <- fmap fromJust getShakeExtra
  dRef <- fmap fromJust getShakeExtra
  let loc = "modules.permute.aPermuteElem"
      -- this hashing has to match the Lst case in Interpreter.Paths.exprPath
      -- TODO export a utility function for that, since it keeps coming up
      hs  = map (digest $ loc ++ ".inner") elements
      h2  = digest (loc ++ ".outer") hs
      out = unsafeExprPathExplicit cfg dRef "list" (ListOf eType) Nothing [h2]
      out' = fromPath loc cfg out
  writeStrings loc eType out' elements
  return out

--------------------
-- leave_each_out --
--------------------

-- TODO rename actual function to drop_each?

leaveEachOut :: Function
leaveEachOut = newFnA1 "leave_each_out"
                 (ListSigs $            AnyType "any type")
                 (ListSigs $ ListSigs $ AnyType "any type")
                 (aPermute dropEach)
                 []

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
