module ShortCut.Modules.Permute where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Paths   (exprPathExplicit)
import ShortCut.Core.Compile.Basic (rExpr)
import ShortCut.Core.Debug (debugAction, debugWriteLines)

cutModule :: CutModule
cutModule = CutModule
  { mName = "permute"
  , mFunctions =
    [ leaveOneOut
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
rCombos comboFn s@(_,cfg) expr@(CutFun _ _ _ fnName [iList]) = do
  (ExprPath iPath) <- rExpr s iList
  let oType = ListOf $ typeOf iList
      (ExprPath oList) = exprPathExplicit cfg True oType fnName [show expr, iPath] -- TODO need fnName too like before?
      lType  = typeOf iList -- TODO is this right?
  oList %> aCombos cfg comboFn iPath lType fnName
  return (ExprPath oList)
rCombos _ _ _ = error "bad argument to rCombos"

-- TODO once back-compilation or whatever works, also use it here? not necessary but maybe simpler
aCombos :: CutConfig -> ([String] -> [[String]])
        -> FilePath -> CutType -> String -> FilePath -> Action ()
aCombos cfg comboFn iPath lType fnName out = do
  need [iPath]
  elements <- fmap lines $ readFile' iPath
  let combos = comboFn elements
      oPaths = map (\(ExprPath p) -> p)
             $ map (\e -> exprPathExplicit cfg True lType
                                           fnName [iPath, out, e]) elements
  mapM_ (\(c,p) -> liftIO $ writeFileLines p c) (zip combos oPaths)
  let out' = debugAction cfg "aCombos" out [iPath, extOf lType, fnName, out]
  debugWriteLines cfg out' oPaths
