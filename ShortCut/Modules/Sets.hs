module ShortCut.Modules.Sets where

import Data.Set                   (Set, union, difference, intersection
                                  ,fromList, toList)
import Development.Shake
import Prelude hiding (div)
import ShortCut.Core.Types
import ShortCut.Core.Compile (cBop)

cutModule :: CutModule
cutModule = CutModule
  { mName = "sets"
  , mFunctions =
    [ mkSetBop "|" union        -- TODO pass the return type to it
    , mkSetBop "~" difference   -- TODO pass the return type to it
    , mkSetBop "&" intersection -- TODO pass the return type to it
    ]
  }

-- TODO how do I say "set of whatever" here? Maybe need rtn argument?
--      what if I give *everything* an argument like this?
--      no no no, just the functions that actually need it!
--      return type should be known once arguments are parsed,
--      and then you can call this function with it!
mkSetBop :: String -> (Set String -> Set String -> Set String) -> CutFunction
mkSetBop name fn = CutFunction
  { fName = name
  -- , fAccepts = [SetOf rtn, SetOf rtn]
  -- , fReturns = SetOf rtn
  , fSignature = \(SetOf a) -> (SetOf a, [SetOf a, SetOf a])
  , fFixity  = Infix
  , fCompiler = cSet fn
  }

-- apply a set operation to two sets (implemented as lists so far)
-- TODO if order turns out to be important in cuts, call them lists
cSet :: (Set String -> Set String -> Set String)
     -> CutConfig -> CutExpr -> Rules FilePath
cSet fn cfg e@(CutBop extn _ s1 s2) = do
  -- liftIO $ putStrLn "entering cSet"
  (p1, p2, p3) <- cBop cfg extn e (s1, s2)
  p3 %> \out -> do
    lines1 <- readFileLines p1
    lines2 <- readFileLines p2
    -- putQuiet $ unwords [fnName, p1, p2, p3]
    let lines3 = fn (fromList lines1) (fromList lines2)
    writeFileLines out $ toList lines3
  return p3
cSet _ _ _ = error "bad argument to cSet"
