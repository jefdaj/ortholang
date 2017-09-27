module ShortCut.Modules.Permute where

import Development.Shake
import ShortCut.Core.Types

import Development.Shake.FilePath   (makeRelative)
import Path                         (fromAbsFile)
import ShortCut.Core.Compile.Basic  (rExpr)
import ShortCut.Core.Compile.Paths2 (exprPath, exprPathExplicit)
import ShortCut.Core.Debug          (debugAction, debugWriteLines)
import ShortCut.Core.Util           (digest)

cutModule :: CutModule
cutModule = CutModule
  { mName = "permute"
  , mFunctions =
    [ leaveOneOut
    -- TODO sample n elements n times
    -- TODO partition into training + test data
    -- TODO partition into trainint + test data for cross-validation 
    ]
  }

leaveOneOut :: CutFunction
leaveOneOut = CutFunction
  { fName      = "leave_each_out"
  , fFixity    = Prefix
  , fTypeCheck = combosTypeCheck
  , fRules  = rCombos leaveEachOut
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
leaveEachOut :: [a] -> [[a]]
leaveEachOut xs
  | length xs < 2 = []
  | otherwise = map (dropFromList xs) [1..length xs]

-- splits a list into a list of lists using the provided function
-- TODO produce each output list in a separate Shake monad section?
-- TODO are paths hashes unique now??
-- TODO use writeFileChanged instead of writeFileLines?
--      (if it turns out to be re-running stuff unneccesarily)
rCombos :: ([FilePath] -> [[FilePath]])
        -> CutState -> CutExpr -> Rules ExprPath
rCombos comboFn s expr@(CutFun (ListOf etype) salt _ fnName [iList]) = do
  (ExprPath iPath) <- rExpr s iList
  let oList = fromAbsFile $ exprPath s expr
  oList %> aCombos s comboFn iPath etype salt fnName
  return (ExprPath oList)
rCombos _ _ _ = error "bad argument to rCombos"

-- TODO once back-compilation or whatever works, also use it here?
aCombos :: CutState
        -> ([String] -> [[String]])
        -> FilePath -> CutType -> Int -> String
        -> FilePath -> Action ()
aCombos s@(_,cfg) comboFn iPath lType salt fnName out = do
  need [iPath]
  elements <- fmap lines $ readFile' iPath
  let mkOut p = exprPathExplicit s fnName lType salt [digest p]
      oPaths  = map (fromAbsFile . mkOut) elements
      combos  = comboFn elements
  mapM_ (\(p,c) -> liftIO $ writeFileLines p c) (zip oPaths combos)
  let out' = debugAction cfg "aCombos" out [iPath, extOf lType, fnName, out]
  debugWriteLines cfg out' oPaths
