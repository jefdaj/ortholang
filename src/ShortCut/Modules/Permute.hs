module ShortCut.Modules.Permute where

import Development.Shake
import ShortCut.Core.Types

import Development.Shake.FilePath   (makeRelative)
import ShortCut.Core.Compile.Basic  (rExpr)
import ShortCut.Core.Paths (exprPath, exprPathExplicit, fromCutPath, readStrings, writeStrings)
import ShortCut.Core.Debug          (debugAction, debug)
import ShortCut.Core.Util           (digest)

cutModule :: CutModule
cutModule = CutModule
  { mName = "permute"
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
-- TODO use writeFileChanged instead of writeFileLines?
--      (if it turns out to be re-running stuff unneccesarily)
rPermute :: ([String] -> [[String]])
         -> CutState -> CutExpr -> Rules ExprPath
rPermute comboFn s@(_, cfg) expr@(CutFun _ salt _ _ [iList]) = do
  (ExprPath iPath) <- rExpr s iList
  let oList      = fromCutPath cfg $ exprPath s expr
      (ListOf t) = typeOf iList
  oList %> aPermute s comboFn iPath t salt
  return (ExprPath oList)
rPermute _ _ _ = error "bad argument to rCombos"

-- TODO once back-compilation or whatever works, also use it here?
-- TODO do something more obvious than writing to the "list" prefix??
aPermute :: CutState
         -> ([String] -> [[String]])
         -> FilePath -> CutType -> Int
         -> FilePath -> Action ()
aPermute s@(_,cfg) comboFn iPath eType salt out = do
  need [iPath]
  elements <- readStrings eType cfg iPath
  -- TODO these aren't digesting properly! elements need to be compiled first?
  --      (digesting the elements themselves rather than the path to them)
  -- TODO will this match other files?
  let mkOut p = exprPathExplicit s "list" (ListOf eType) salt [digest $ makeRelative (cfgTmpDir cfg) p]
      oPaths  = map mkOut elements
      oPaths' = map (fromCutPath cfg) oPaths
      combos  = comboFn elements
  mapM_ (\(p,ps) -> writeStrings eType cfg p $ debug cfg ("combo: " ++ show ps) ps) (zip oPaths' combos)
  let out' = debugAction cfg "aPermute" out [iPath, extOf eType, out]
  writeStrings (ListOf eType) cfg out' oPaths'

--------------------
-- leave_each_out --
--------------------

-- TODO rename actual function to drop_each?

leaveEachOut :: CutFunction
leaveEachOut = CutFunction
  { fName      = "leave_each_out"
  , fFixity    = Prefix
  , fTypeCheck = combosTypeCheck
  , fRules     = rPermute dropEach
  }

combosTypeCheck :: [CutType] -> Either String CutType
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
