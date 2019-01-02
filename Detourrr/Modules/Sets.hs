module Detourrr.Modules.Sets where

-- TODO unify bops and funs into one thing (all fns have optional infix version?)
-- TODO writeStrings should delete the outfile on errors!
-- TODO remove long form of diff?

import Development.Shake
import Detourrr.Core.Types

import Data.Function               (on)
import Data.List                   (nubBy)
import Data.Set                    (Set, union, difference, intersection, fromList,
                                    toList)
import Development.Shake.FilePath  ((</>))
import Detourrr.Core.Compile.Basic (rExpr, typeError, debugRules)
import Detourrr.Core.Actions       (readStrings, readPaths, writeStrings, debugA, hashContent)
-- import Detourrr.Core.Debug         (debugRules, debugA)
import Detourrr.Core.Paths         (exprPath, toDtrPath, fromDtrPath)
import Detourrr.Core.Util          (resolveSymlinks)

dtrModule :: DtrModule
dtrModule = DtrModule
  { mName = "Sets"
  , mDesc = "Set operations for use with lists"
  , mTypes = []
  , mFunctions = some : (concat $ map mkSetFunctions setOpDescs)
  }

type SetOpDesc =
  ( String -- name of the prefix function
  , Char   -- name of the infix/binary function
  , Set String -> Set String -> Set String -- haskell set op
  )

{- An awkward intermediate: until I get around to actually merging DtrBops into
 - DtrFuns, we'll just create them from a common description. The Bop versions
 - work on two lists and can be chained together; the prefix (regular function)
 - ones work on lists of lists.
 -
 - TODO rename these all -> union, any -> intersection?
 - TODO rename diff -> only? difference? missing?
 -}
setOpDescs :: [SetOpDesc]
setOpDescs =
  [ ("any" , '|', union)
  , ("all" , '&', intersection)
  , ("diff", '~', difference)
  ]

mkSetFunctions :: SetOpDesc -> [DtrFunction]
mkSetFunctions (foldName, opChar, setFn) = [setBop, setFold]
  where
    mkBopDesc  name = name ++ " : X.list -> X.list -> X.list"
    mkFoldDesc name = name ++ " : X.list.list -> X.list"
    setBop = DtrFunction
      { fName      = [opChar]
      , fTypeCheck = tSetBop
      , fDesc = Nothing, fTypeDesc  = mkBopDesc [opChar]
      , fFixity    = Infix
      , fRules     = rSetBop foldName setFn
      }
    setFold = DtrFunction
      { fName      = foldName
      , fTypeCheck = tSetFold
      , fDesc = Nothing, fTypeDesc  = mkFoldDesc foldName
      , fFixity    = Prefix
      , fRules     = rSetFold (foldr1 setFn)
      }

-- if the user gives two lists but of different types, complain that they must
-- be the same. if there aren't two lists at all, complain about that first
tSetBop :: [DtrType] -> Either String DtrType
tSetBop actual@[ListOf a, ListOf b]
  | typeMatches a b = fmap ListOf $ nonEmptyType [a, b]
  | otherwise = Left $ typeError [ListOf a, ListOf a] actual
tSetBop _ = Left "Type error: expected two lists of the same type"

tSetFold :: [DtrType] -> Either String DtrType
tSetFold [ListOf (ListOf x)] = Right $ ListOf x
tSetFold _ = Left "expecting a list of lists"

-- apply a set operation to two lists (converted to sets first)
-- TODO if order turns out to be important in dtrs, call them lists
rSetBop :: String -> (Set String -> Set String -> Set String)
     -> DtrState -> DtrExpr -> Rules ExprPath
rSetBop name fn s (DtrBop rtn salt deps _ s1 s2) = rSetFold (foldr1 fn) s fun
  where
    fun = DtrFun  rtn salt deps name [lst]
    lst = DtrList rtn salt deps [s1, s2]
rSetBop _ _ _ _ = error "bad argument to rSetBop"

rSetFold :: ([Set String] -> Set String) -> DtrState -> DtrExpr -> Rules ExprPath
rSetFold fn s@(_, cfg, ref, _) e@(DtrFun _ _ _ _ [lol]) = do
  (ExprPath setsPath) <- rExpr s lol
  let oPath      = fromDtrPath cfg $ exprPath s e
      oPath'     = cfgTmpDir cfg </> oPath
      oPath''    = debugRules cfg "rSetFold" e oPath
      (ListOf t) = typeOf lol
  oPath %> \_ -> aSetFold cfg ref fn t oPath' setsPath
  return (ExprPath oPath'')
rSetFold _ _ _ = error "bad argument to rSetFold"

aSetFold :: DtrConfig -> Locks -> ([Set String] -> Set String)
         -> DtrType -> FilePath -> FilePath -> Action ()
aSetFold cfg ref fn (ListOf etype) oPath setsPath = do
  setPaths  <- readPaths cfg ref setsPath
  setElems  <- mapM (readStrings etype cfg ref) (map (fromDtrPath cfg) setPaths)
  setElems' <- liftIO $ mapM (canonicalLinks cfg etype) setElems
  let sets = map fromList setElems'
      oLst = toList $ fn sets
      oPath' = debugA cfg "aSetFold" oPath [oPath, setsPath]
  oLst'' <- if etype `elem` [str, num]
              then mapM return oLst
              else dedupByContent cfg ref oLst
  writeStrings etype cfg ref oPath' oLst''
aSetFold _ _ _ _ _ _ = error "bad argument to aSetFold"

-- a kludge to resolve the difference between load_* and load_*_each paths
-- TODO remove this or shunt it into Paths.hs or something!
canonicalLinks :: DtrConfig -> DtrType -> [FilePath] -> IO [FilePath]
canonicalLinks cfg rtn =
  if rtn `elem` [str, num]
    then return
    else \ps -> mapM (resolveSymlinks (Just $ cfgTmpDir cfg)) ps

-- TODO would resolving symlinks be enough? if so, much less disk IO!
-- see https://stackoverflow.com/a/8316542/429898
dedupByContent :: DtrConfig -> Locks -> [FilePath] -> Action [FilePath]
dedupByContent cfg ref paths = do
  -- TODO if the paths are already in the load cache, no need for content?
  hashes <- mapM (hashContent cfg ref) $ map (toDtrPath cfg) paths
  let paths' = map fst $ nubBy ((==) `on` snd) $ zip paths hashes
  return paths'

some :: DtrFunction
some = DtrFunction
  { fName      = "some"
  , fTypeCheck = tSetFold
  , fDesc = Nothing, fTypeDesc  = "some : X.list.list -> X.list"
  , fFixity    = Prefix
  , fRules     = rSome
  }

rSome :: DtrState -> DtrExpr -> Rules ExprPath
rSome s (DtrFun rtn salt deps _ lol) = rExpr s diffExpr
  where
    anyExpr  = DtrFun rtn salt deps "any" lol
    allExpr  = DtrFun rtn salt deps "all" lol
    diffExpr = DtrBop rtn salt deps "~" anyExpr allExpr
rSome _ _ = error "bad argument to rSome"
