module ShortCut.Modules.Sets where

-- TODO unify bops and funs into one thing (all fns have optional infix version?)
-- TODO writeStrings should delete the outfile on errors!

import Development.Shake
import ShortCut.Core.Types

import Data.Function               (on)
import Data.List                   (nubBy)
import Data.Set                    (Set, union, difference, intersection, fromList,
                                    toList)
import Development.Shake.FilePath  ((</>))
import ShortCut.Core.Compile.Basic (rExpr, typeError)
import ShortCut.Core.Actions       (readStrings, readPaths, writeStrings, digestFile)
import ShortCut.Core.Debug         (debugRules, debugAction)
import ShortCut.Core.Paths         (exprPath, fromCutPath)
import ShortCut.Core.Util          (resolveSymlinks)

cutModule :: CutModule
cutModule = CutModule
  { mName = "setops"
  , mFunctions = some : (concat $ map mkSetFunctions setOpDescs)
  }

type SetOpDesc =
  ( String -- name of the prefix function
  , Char   -- name of the infix/binary function
  , Set String -> Set String -> Set String -- haskell set op
  )

{- An awkward intermediate: until I get around to actually merging CutBops into
 - CutFuns, we'll just create them from a common description. The Bop versions
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

mkSetFunctions :: SetOpDesc -> [CutFunction]
mkSetFunctions (foldName, opChar, setFn) = [setBop, setFold]
  where
    setBop = CutFunction
      { fName      = [opChar]
      , fTypeCheck = tSetBop
      , fFixity    = Infix
      , fRules     = rSetBop foldName setFn
      }
    setFold = CutFunction
      { fName      = foldName
      , fTypeCheck = tSetFold
      , fFixity    = Prefix
      , fRules     = rSetFold (foldr1 setFn)
      }

-- if the user gives two lists but of different types, complain that they must
-- be the same. if there aren't two lists at all, complain about that first
tSetBop :: [CutType] -> Either String CutType
tSetBop actual@[ListOf a, ListOf b]
  | typeMatches a b = fmap ListOf $ nonEmptyType [a, b]
  | otherwise = Left $ typeError [ListOf a, ListOf a] actual
tSetBop _ = Left "Type error: expected two lists of the same type"

tSetFold :: [CutType] -> Either String CutType
tSetFold [ListOf (ListOf x)] = Right $ ListOf x
tSetFold _ = Left "expecting a list of lists"

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
rSetFold fn s@(_,cfg,ref) e@(CutFun _ _ _ _ [lol]) = do
  (ExprPath setsPath) <- rExpr s lol
  let oPath      = fromCutPath cfg $ exprPath s e
      oPath'     = cfgTmpDir cfg </> oPath
      oPath''    = debugRules cfg "rSetFold" e oPath
      (ListOf t) = typeOf lol
  oPath %> \_ -> aSetFold cfg ref fn t oPath' setsPath
  return (ExprPath oPath'')
rSetFold _ _ _ = error "bad argument to rSetFold"

aSetFold :: CutConfig -> Locks -> ([Set String] -> Set String)
         -> CutType -> FilePath -> FilePath -> Action ()
aSetFold cfg ref fn (ListOf etype) oPath setsPath = do
  setPaths  <- readPaths cfg ref setsPath
  setElems  <- mapM (readStrings etype cfg ref) (map (fromCutPath cfg) setPaths)
  setElems' <- liftIO $ mapM (canonicalLinks cfg etype) setElems
  let sets = map fromList setElems'
      oLst = toList $ fn sets
      oPath' = debugAction cfg "aSetFold" oPath [oPath, setsPath]
  oLst'' <- if etype `elem` [str, num]
              then mapM return oLst
              else dedupByContent cfg ref oLst
  writeStrings etype cfg ref oPath' oLst''
aSetFold _ _ _ _ _ _ = error "bad argument to aSetFold"

-- a kludge to resolve the difference between load_* and load_*_each paths
-- TODO remove this or shunt it into Paths.hs or something!
canonicalLinks :: CutConfig -> CutType -> [FilePath] -> IO [FilePath]
canonicalLinks cfg rtn =
  if rtn `elem` [str, num]
    then return
    else \ps -> mapM (resolveSymlinks (Just $ cfgTmpDir cfg)) ps

-- TODO would resolving symlinks be enough? if so, much less disk IO!
-- see https://stackoverflow.com/a/8316542/429898
dedupByContent :: CutConfig -> Locks -> [FilePath] -> Action [FilePath]
dedupByContent cfg ref paths = do
  hashes <- mapM (digestFile cfg ref) paths
  let paths' = map fst $ nubBy ((==) `on` snd) $ zip paths hashes
  return paths'

some :: CutFunction
some = CutFunction
  { fName      = "some"
  , fTypeCheck = tSetFold
  , fFixity    = Prefix
  , fRules     = rSome
  }

rSome :: CutState -> CutExpr -> Rules ExprPath
rSome s (CutFun rtn salt deps _ lol) = rExpr s diffExpr
  where
    anyExpr  = CutFun rtn salt deps "any" lol
    allExpr  = CutFun rtn salt deps "all" lol
    diffExpr = CutBop rtn salt deps "~" anyExpr allExpr
rSome _ _ = error "bad argument to rSome"
