-- TODO rename to something that makes sense

module ShortCut.Modules.Macros where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile (cExpr, hashedTmp')

cutModule :: CutModule
cutModule = CutModule
  { mName = "macros"
  , mFunctions =
    [ leaveOneOut
    ]
  }

leaveOneOut :: CutFunction
leaveOneOut = CutFunction
  { fName      = "leave_each_out"
  , fFixity    = Prefix
  , fTypeCheck = macroTypeCheck
  , fCompiler  = cSplit leaveEachOut
  }

macroTypeCheck :: [CutType] -> Either String CutType
macroTypeCheck [(ListOf t)] = Right $ ListOf $ ListOf t
macroTypeCheck _ = Left "type error in leave_each_out!"

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
-- TODO this should take a function call, and that's where you get the name!
-- TODO produce each output list in a separate Shake monad section?
-- TODO are paths hashes unique now??
-- TODO use writeFileChanged instead of writeFileLines?
--      (if it turns out to be re-running stuff unneccesarily)
cSplit :: ([FilePath] -> [[FilePath]])
       -> CutConfig -> CutExpr -> Rules FilePath
cSplit comboFn cfg expr@(CutFun _ fnName [iList]) = do
  iPath <- cExpr cfg iList
  let oType = ListOf $ typeOf iList
      oList = hashedTmp' cfg oType expr [iPath, fnName]
  oList %> \out -> do
    need [iPath]
    elements <- fmap lines $ readFile' iPath
    let combos = comboFn elements
        lType  = typeOf iList
        oPaths = map (\e -> hashedTmp' cfg lType iList [out, e]) elements
    mapM_ (\(c,p) -> liftIO $ writeFileLines p c) (zip combos oPaths)
    writeFileChanged out $ unlines oPaths
  return oList
cSplit _ _ _ = error "bad argument to cSplit"
