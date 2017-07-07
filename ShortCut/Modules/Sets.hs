module ShortCut.Modules.Sets where

import Data.Set (Set, union, difference, intersection ,fromList, toList)
import Development.Shake
import ShortCut.Core.Paths   (hashedTmp)
import ShortCut.Core.Compile (cBop, cExpr)
import ShortCut.Core.ModuleAPI (typeError)
import ShortCut.Core.Types
import ShortCut.Core.Debug (debugReadLines, debugWriteLines)

cutModule :: CutModule
cutModule = CutModule
  { mName = "setops"
  , mFunctions =
    [ unionBop
    , unionFold
    , intersectionBop
    , intersectionFold
    , differenceBop
    ]
  }

----------------------
-- binary operators --
----------------------

mkSetBop :: String -> (Set String -> Set String -> Set String) -> CutFunction
mkSetBop name fn = CutFunction
  { fName      = name
  , fTypeCheck = bopTypeCheck
  , fFixity    = Infix
  , fCompiler  = cSetBop fn
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
cSetBop :: (Set String -> Set String -> Set String)
     -> CutState -> CutExpr -> Rules ExprPath
cSetBop fn s e@(CutBop extn _ _ _ s1 s2) = do
  -- liftIO $ putStrLn "entering cSetBop"
  (ExprPath p1, ExprPath p2, ExprPath p3) <- cBop s extn e (s1, s2)
  p3 %> \out -> do
    lines1 <- readFileLines p1
    lines2 <- readFileLines p2
    -- putQuiet $ unwords [fnName, p1, p2, p3]
    let lines3 = fn (fromList lines1) (fromList lines2)
    writeFileLines out $ toList lines3
  return (ExprPath p3)
cSetBop _ _ _ = error "bad argument to cSetBop"

unionBop :: CutFunction
unionBop = mkSetBop "|" union

differenceBop :: CutFunction
differenceBop = mkSetBop "~" difference

intersectionBop :: CutFunction
intersectionBop = mkSetBop "&" intersection

---------------------------------------------
-- functions that summarize lists of lists --
---------------------------------------------

mkSetFold :: String -> ([Set String] -> Set String) -> CutFunction
mkSetFold name fn = CutFunction
  { fName      = name
  , fTypeCheck = tSetFold
  , fFixity    = Infix
  , fCompiler  = cSetSummary fn
  }

tSetFold :: [CutType] -> Either String CutType
tSetFold [ListOf (ListOf x)] = Right $ ListOf x
tSetFold _ = Left "expecting a list of lists"

cSetSummary fn s@(_,cfg) e@(CutFun _ _ _ _ sets) = do
  setPaths <- mapM (cExpr s) sets
  let (ExprPath oPath) = hashedTmp cfg e []
  oPath %> \_ -> do
    lists <- mapM (debugReadLines cfg) (map (\(ExprPath p) -> p) setPaths)
    let sets = map fromList lists
        oLst = toList $ fn sets
    debugWriteLines cfg oPath oLst
  return (ExprPath oPath)
cSetSummary _ _ _ = error "bad argument to cSetSummary"

-- avoided calling it `all` because that's a Prelude function
intersectionFold :: CutFunction
intersectionFold = mkSetFold "all" $ foldr1 intersection

-- avoided calling it `any` because that's a Prelude function
unionFold :: CutFunction
unionFold = mkSetFold "any" $ foldr1 union
