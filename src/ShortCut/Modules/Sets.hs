module ShortCut.Modules.Sets where

import Data.Set (Set, union, difference, intersection ,fromList, toList)
import Development.Shake
import ShortCut.Core.Paths (exprPath, fromCutPath, readPaths, readStrings, writeStrings)
import ShortCut.Core.Compile.Basic (rBop, rExpr, typeError)
import ShortCut.Core.Types
import ShortCut.Core.Debug (debugRules, debugAction)
import Development.Shake.FilePath ((</>))
import ShortCut.Core.Util (resolveSymlinks, typeMatches, nonEmptyType)
-- import Path (fromCutPath cfg) -- TODO remove and use Path everywhere

cutModule :: CutModule
cutModule = CutModule
  { mName = "setops"
  , mFunctions =
    -- TODO unify bops and funs into one thing (all fns have optional infix version?)
    [ unionBop       , unionFold
    , intersectionBop, intersectionFold
    , differenceBop  , differenceFold
    ]
  }

-- a kludge to resolve the difference between load_* and load_*_each paths
-- TODO remove this or shunt it into Paths.hs or something!
canonicalLinks :: CutConfig -> CutType -> [FilePath] -> IO [FilePath]
canonicalLinks cfg rtn =
  if rtn `elem` [ListOf str, ListOf num]
    then return
    else \ps -> mapM (resolveSymlinks cfg) ps

----------------------
-- binary operators --
----------------------

mkSetBop :: String -> String
         -> (Set String -> Set String -> Set String)
         -> CutFunction
mkSetBop name foldName fn = CutFunction
  { fName      = name
  , fTypeCheck = bopTypeCheck
  , fFixity    = Infix
  , fRules     = rSetBop foldName fn
  }

-- if the user gives two lists but of different types, complain that they must
-- be the same. if there aren't two lists at all, complain about that first
bopTypeCheck :: [CutType] -> Either String CutType
bopTypeCheck actual@[ListOf a, ListOf b]
  | typeMatches a b = fmap ListOf $ nonEmptyType [a, b]
  | otherwise = Left $ typeError [ListOf a, ListOf a] actual
bopTypeCheck _ = Left "Type error: expected two lists of the same type"

-- apply a set operation to two lists (converted to sets first)
-- TODO if order turns out to be important in cuts, call them lists
rSetBop :: String -> (Set String -> Set String -> Set String)
     -> CutState -> CutExpr -> Rules ExprPath
rSetBop name fn s (CutBop rtn salt deps _ s1 s2) = rSetFold (foldr1 fn) s fun
  where
    fun = CutFun  rtn salt deps name [lst]
    lst = CutList rtn salt deps [s1, s2]
rSetBop _ _ _ _ = error "bad argument to rSetBop"

-- rSetBop fn s@(_,cfg) e@(CutBop _ _ _ _ s1 s2) = do
--   -- liftIO $ putStrLn "entering rSetBop"
--   -- let fixLinks = liftIO . canonicalLinks cfg (typeOf e)
--   let fixLinks = canonicalLinks cfg (typeOf e)
--       (ListOf t) = typeOf s1 -- element type for translating to/from string
--   (ExprPath p1, ExprPath p2, ExprPath p3) <- rBop s e (s1, s2)
--   p3 %> aSetBop cfg fixLinks t fn p1 p2
--   return (ExprPath p3)
-- rSetBop _ _ _ = error "bad argument to rSetBop"
-- 
-- aSetBop :: CutConfig
--         -> ([String] -> IO [String])
--         -> CutType
--         -> (Set String -> Set String -> Set String)
--         -> FilePath -> FilePath -> FilePath -> Action ()
-- aSetBop cfg fixLinks etype fn p1 p2 out = do
--   need [p1, p2] -- this is required for parallel evaluation!
--   -- lines1 <- liftIO . fixLinks =<< readFileLines p1
--   -- lines2 <- liftIO . fixLinks =<< readFileLines p2
--   paths1 <- liftIO . fixLinks =<< readStrings etype cfg p1
--   paths2 <- liftIO . fixLinks =<< readStrings etype cfg p2
--   -- putQuiet $ unwords [fnName, p1, p2, p3]
--   let paths3 = fn (fromList paths1) (fromList paths2)
--       out' = debugAction cfg "aSetBop" out [p1, p2, out]
--   -- liftIO $ putStrLn $ "paths3: " ++ show paths3
--   writeStrings etype cfg out' $ toList paths3 -- TODO delete file on error (else it looks empty!)

-- TODO rename these all -> union, any -> intersection?
unionBop :: CutFunction
unionBop = mkSetBop "|" "all" union

intersectionBop :: CutFunction
intersectionBop = mkSetBop "&" "any" intersection

-- TODO rename diff -> only? difference? missing?
differenceBop :: CutFunction
differenceBop = mkSetBop "~" "diff" difference

---------------------------------------------
-- functions that summarize lists of lists --
---------------------------------------------

mkSetFold :: String -> ([Set String] -> Set String) -> CutFunction
mkSetFold name fn = CutFunction
  { fName      = name
  , fTypeCheck = tSetFold
  , fFixity    = Prefix
  , fRules  = rSetFold fn
  }

tSetFold :: [CutType] -> Either String CutType
tSetFold [ListOf (ListOf x)] = Right $ ListOf x
tSetFold _ = Left "expecting a list of lists"

rSetFold :: ([Set String] -> Set String) -> CutState -> CutExpr -> Rules ExprPath
rSetFold fn s@(_,cfg) e@(CutFun _ _ _ _ [lol]) = do
  (ExprPath setsPath) <- rExpr s lol
  let oPath    = fromCutPath cfg $ exprPath s e
      oPath'   = cfgTmpDir cfg </> oPath
      oPath''  = debugRules cfg "rSetFold" e oPath
      (ListOf t) = typeOf lol
      fixLinks = canonicalLinks cfg (typeOf e) -- TODO move to aSetFold
  oPath %> \_ -> aSetFold cfg fixLinks fn t oPath' setsPath
  return (ExprPath oPath'')
rSetFold _ _ _ = error "bad argument to rSetFold"

-- TODO writeStrings should delete the outfile on errors!
aSetFold :: CutConfig
         -> ([String] -> IO [String])
         -> ([Set String] -> Set String)
         -> CutType
         -> FilePath -> FilePath
         -> Action ()
aSetFold cfg fixLinks fn (ListOf etype) oPath setsPath = do
  -- liftIO $ putStrLn $ "aSetFold collapsing lists from " ++ extOf (ListOf etype) ++ " -> " ++ extOf etype
  setPaths  <- readPaths cfg setsPath
  setElems  <- mapM (readStrings etype cfg) (map (fromCutPath cfg) setPaths)
  setElems' <- liftIO $ mapM fixLinks setElems
  -- liftIO $ putStrLn $ "setElems: " ++ show setElems
  -- liftIO $ putStrLn $ "setElems': " ++ show setElems'
  let sets = map fromList setElems'
      oLst = toList $ fn sets
      oPath' = debugAction cfg "aSetFold" oPath [oPath, setsPath]
  writeStrings etype cfg oPath' oLst
aSetFold _ _ _ _ _ _ = error "bad argument to aSetFold"

-- avoided calling it `all` because that's a Prelude function
intersectionFold :: CutFunction
intersectionFold = mkSetFold "all" $ foldr1 intersection

-- avoided calling it `any` because that's a Prelude function
unionFold :: CutFunction
unionFold = mkSetFold "any" $ foldr1 union

differenceFold :: CutFunction
differenceFold = mkSetFold "diff" $ foldr1 difference
