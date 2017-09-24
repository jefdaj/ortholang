module ShortCut.Modules.Sets where

-- TODO move this stuff to Core? (maybe split Compile into a couple modules?)

import Data.Set (Set, union, difference, intersection ,fromList, toList)
import Development.Shake
import ShortCut.Core.Compile.Paths2  (tmpToExpr)
import ShortCut.Core.Compile.Rules (rBop, rExpr, typeError)
import ShortCut.Core.Types
import ShortCut.Core.Debug (debugReadLines, debugWriteLines,
                            debugRules, debugAction)
import Development.Shake.FilePath ((</>))
import ShortCut.Core.Util (resolveSymlinks)
import Path (fromAbsFile) -- TODO remove and use Path everywhere

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

-- a kludge to resolve the difference between load_* and load_*_each paths
-- TODO is it deterministic?
canonicalLinks :: CutType -> [FilePath] -> IO [FilePath]
canonicalLinks rtn =
  if rtn `elem` [ListOf str, ListOf num]
    then return
    else \ps -> mapM resolveSymlinks ps

----------------------
-- binary operators --
----------------------

mkSetBop :: String -> (Set String -> Set String -> Set String) -> CutFunction
mkSetBop name fn = CutFunction
  { fName      = name
  , fTypeCheck = bopTypeCheck
  , fFixity    = Infix
  , fRules  = rListBop fn
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
rListBop :: (Set String -> Set String -> Set String)
     -> CutState -> CutExpr -> Rules ExprPath
rListBop fn s@(_,cfg) e@(CutBop extn _ _ _ s1 s2) = do
  -- liftIO $ putStrLn "entering rListBop"
  -- let fixLinks = liftIO . canonicalLinks (typeOf e)
  let fixLinks = canonicalLinks (typeOf e)
  (ExprPath p1, ExprPath p2, ExprPath p3) <- rBop s extn e (s1, s2)
  p3 %> aSetBop cfg fixLinks fn p1 p2
  return (ExprPath p3)
rListBop _ _ _ = error "bad argument to rListBop"

aSetBop :: CutConfig -> ([String] -> IO [String])
        -> (Set String -> Set String -> Set String)
        -> FilePath -> FilePath -> FilePath -> Action ()
aSetBop cfg fixLinks fn p1 p2 out = do
  need [p1, p2] -- this is required for parallel evaluation!
  lines1 <- liftIO . fixLinks =<< readFileLines p1
  lines2 <- liftIO . fixLinks =<< readFileLines p2
  -- putQuiet $ unwords [fnName, p1, p2, p3]
  let lines3 = fn (fromList lines1) (fromList lines2)
      out' = debugAction cfg "aSetBop" out [p1, p2, out]
  debugWriteLines cfg out' $ toList lines3

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
  , fFixity    = Prefix
  , fRules  = rListFold fn
  }

tSetFold :: [CutType] -> Either String CutType
tSetFold [ListOf (ListOf x)] = Right $ ListOf x
tSetFold _ = Left "expecting a list of lists"

rListFold :: ([Set String] -> Set String) -> CutState -> CutExpr -> Rules ExprPath
rListFold fn s@(_,cfg) e@(CutFun _ _ _ _ [lol]) = do
  (ExprPath setsPath) <- rExpr s lol
  let oPath    = fromAbsFile $ tmpToExpr s e
      oPath'   = cfgTmpDir cfg </> oPath
      oPath''  = debugRules cfg "rListFold" e oPath
      fixLinks = canonicalLinks (typeOf e)
  oPath %> \_ -> aSetFold cfg fixLinks fn oPath' setsPath
  return (ExprPath oPath'')
rListFold _ _ _ = error "bad argument to rListFold"

aSetFold :: CutConfig
         -> ([String] -> IO [String])
         -> ([Set String] -> Set String)
         -> FilePath -> FilePath -> Action ()
aSetFold cfg fixLinks fn oPath setsPath = do
  lists <- debugReadLines cfg setsPath
  listContents  <- mapM (debugReadLines cfg) $ map (cfgTmpDir cfg </>) lists
  listContents' <- liftIO $ mapM (liftIO . fixLinks) listContents
  -- liftIO $ putStrLn $ "listContents': " ++ show listContents'
  let sets = map fromList listContents'
      oLst = toList $ fn sets
      oPath' = debugAction cfg "aSetFold" oPath [oPath, setsPath]
  debugWriteLines cfg oPath' oLst

-- avoided calling it `all` because that's a Prelude function
intersectionFold :: CutFunction
intersectionFold = mkSetFold "all" $ foldr1 intersection

-- avoided calling it `any` because that's a Prelude function
unionFold :: CutFunction
unionFold = mkSetFold "any" $ foldr1 union
