module ShortCut.Modules.Sets where

import Data.Set (Set, union, difference, intersection ,fromList, toList)
import Development.Shake
import ShortCut.Core.Compile (cBop)
import ShortCut.Core.Parse (typeError)
import ShortCut.Core.Types

cutModule :: CutModule
cutModule = CutModule
  { mName = "setops"
  , mFunctions =
    [ mkSetBop "|" union
    , mkSetBop "~" difference
    , mkSetBop "&" intersection
    ]
  }

mkSetBop :: String -> (Set String -> Set String -> Set String) -> CutFunction
mkSetBop name fn = CutFunction
  { fName      = name
  , fTypeCheck = bopTypeCheck
  , fFixity    = Infix
  , fCompiler  = cSet fn
  }

-- if the user gives two lists but of different types, complain that they must
-- be the same. if there aren't two lists at all, complain about that first
bopTypeCheck :: [CutType] -> Either String CutType
bopTypeCheck actual@[ListOf a, ListOf b]
  | a == b    = Right $ ListOf a
  | otherwise = Left $ typeError [ListOf a, ListOf a] actual
bopTypeCheck _ = Left "Type error: expected two lists of the same type"

-- apply a set operation to two lists (converted to sets first)
-- TODO if order turns out to be important in cuts, call them lists
cSet :: (Set String -> Set String -> Set String)
     -> CutState -> CutExpr -> Rules FilePath
cSet fn s e@(CutBop extn _ _ s1 s2) = do
  -- liftIO $ putStrLn "entering cSet"
  (p1, p2, p3) <- cBop s extn e (s1, s2)
  p3 %> \out -> do
    lines1 <- readFileLines p1
    lines2 <- readFileLines p2
    -- putQuiet $ unwords [fnName, p1, p2, p3]
    let lines3 = fn (fromList lines1) (fromList lines2)
    writeFileLines out $ toList lines3
  return p3
cSet _ _ _ = error "bad argument to cSet"
