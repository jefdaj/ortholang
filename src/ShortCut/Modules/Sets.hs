module ShortCut.Modules.Sets where

import Development.Shake

import Data.Function               (on)
import Data.List                   (nubBy)
import Data.Set                    (Set, union, difference, intersection, fromList,
                                    toList)
import Development.Shake.FilePath  ((</>))
import ShortCut.Core.Compile.Basic (rExpr, typeError)
import ShortCut.Core.Debug         (debugRules, debugAction, debugReadFile)
import ShortCut.Core.Paths         (exprPath, fromCutPath, readPaths,
                                    readStrings, writeStrings)
import ShortCut.Core.Types
import ShortCut.Core.Util          (resolveSymlinks, typeMatches, nonEmptyType,
                                    digest)

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

---------------------------------------
-- binary operators (infix versions) --
---------------------------------------

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

-- TODO rename these all -> union, any -> intersection?
unionBop :: CutFunction
unionBop = mkSetBop "|" "all" union

intersectionBop :: CutFunction
intersectionBop = mkSetBop "&" "any" intersection

-- TODO rename diff -> only? difference? missing?
differenceBop :: CutFunction
differenceBop = mkSetBop "~" "diff" difference

-----------------------------
-- folds (prefix versions) --
-----------------------------

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

-- avoided calling it `all` because that's a Prelude function
intersectionFold :: CutFunction
intersectionFold = mkSetFold "all" $ foldr1 intersection

-- avoided calling it `any` because that's a Prelude function
unionFold :: CutFunction
unionFold = mkSetFold "any" $ foldr1 union

differenceFold :: CutFunction
differenceFold = mkSetFold "diff" $ foldr1 difference

--------------------
-- implementation --
--------------------

-- apply a set operation to two lists (converted to sets first)
-- TODO if order turns out to be important in cuts, call them lists
rSetBop :: String -> (Set String -> Set String -> Set String)
     -> CutState -> CutExpr -> Rules ExprPath
rSetBop name fn s (CutBop rtn salt deps _ s1 s2) = rSetFold (foldr1 fn) s fun
  where
    fun = CutFun  rtn salt deps name [lst]
    lst = CutList rtn salt deps [s1, s2]
rSetBop _ _ _ _ = error "bad argument to rSetBop"

rSetFold :: ([Set String] -> Set String) -> CutState -> CutExpr -> Rules ExprPath
rSetFold fn s@(_,cfg) e@(CutFun _ _ _ _ [lol]) = do
  (ExprPath setsPath) <- rExpr s lol
  let oPath    = fromCutPath cfg $ exprPath s e
      oPath'   = cfgTmpDir cfg </> oPath
      oPath''  = debugRules cfg "rSetFold" e oPath
      (ListOf t) = typeOf lol
      -- fixLinks = canonicalLinks cfg (typeOf e) -- TODO move to aSetFold
  oPath %> \_ -> aSetFold cfg fn t oPath' setsPath
  return (ExprPath oPath'')
rSetFold _ _ _ = error "bad argument to rSetFold"

-- TODO writeStrings should delete the outfile on errors!
aSetFold :: CutConfig
         -> ([Set String] -> Set String)
         -> CutType
         -> FilePath -> FilePath
         -> Action ()
aSetFold cfg fn (ListOf etype) oPath setsPath = do
  setPaths  <- readPaths cfg setsPath
  setElems  <- mapM (readStrings etype cfg) (map (fromCutPath cfg) setPaths)
  setElems' <- liftIO $ mapM (canonicalLinks cfg etype) setElems
  let sets = map fromList setElems'
      oLst = toList $ fn sets
      oPath' = debugAction cfg "aSetFold" oPath [oPath, setsPath]
  oLst'' <- if etype `elem` [str, num]
              then mapM return oLst
              else dedupByContent cfg oLst
  writeStrings etype cfg oPath' oLst''
aSetFold _ _ _ _ _ = error "bad argument to aSetFold"

-- a kludge to resolve the difference between load_* and load_*_each paths
-- TODO remove this or shunt it into Paths.hs or something!
canonicalLinks :: CutConfig -> CutType -> [FilePath] -> IO [FilePath]
canonicalLinks cfg rtn =
  if rtn `elem` [str, num]
    then return
    else \ps -> mapM (resolveSymlinks cfg) ps

-- TODO would resolving symlinks be enough? if so, much less disk IO!
-- see https://stackoverflow.com/a/8316542/429898
dedupByContent :: CutConfig -> [FilePath] -> Action [FilePath]
dedupByContent cfg paths = do
  hashes <- mapM (digestFile cfg) paths
  let paths' = map fst $ nubBy ((==) `on` snd) $ zip paths hashes
  return paths'

digestFile :: CutConfig -> FilePath -> Action String
digestFile cfg path = debugReadFile cfg path >>= return . digest
