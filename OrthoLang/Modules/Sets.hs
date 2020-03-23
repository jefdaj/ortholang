module OrthoLang.Modules.Sets where

-- TODO unify bops and funs into one thing (all fns have optional infix version?)
-- TODO writeStrings should delete the outfile on errors!
-- TODO remove long form of diff?

import Development.Shake
import OrthoLang.Core.Types

import qualified Data.Set as Set

import Data.Function               (on)
import Data.List                   (nubBy)
import Data.Set                    (Set, union, difference, intersection, fromList,
                                    toList)
import Data.IORef                  (readIORef)
import Development.Shake.FilePath  ((</>))
import OrthoLang.Core.Compile.Basic (rExpr, typeError, debugRules)
import OrthoLang.Core.Actions       (readStrings, readPaths, writeStrings, traceA, hashContent)
-- import OrthoLang.Core.Debug         (debugRules, traceA)
import OrthoLang.Core.Paths         (exprPath, toOrthoLangPath, fromOrthoLangPath)
import OrthoLang.Core.Util          (resolveSymlinks)
import OrthoLang.Core.Sanitize      (unhashIDs)

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "Sets"
  , mDesc = "Set operations for use with lists"
  , mTypes = []
  , mFunctions = some : (concat $ map mkSetFunctions setOpDescs)
  }

type SetOpDesc =
  ( String -- name of the prefix function
  , Char   -- name of the infix/binary function
  , String -- long name for the binary operator (should be a valid filename)
  , Set String -> Set String -> Set String -- haskell set op
  )

{- An awkward intermediate: until I get around to actually merging OrthoLangBops into
 - OrthoLangFuns, we'll just create them from a common description. The Bop versions
 - work on two lists and can be chained together; the prefix (regular function)
 - ones work on lists of lists.
 -
 - TODO rename these all -> union, any -> intersection?
 - TODO rename diff -> only? difference? missing?
 -}
setOpDescs :: [SetOpDesc]
setOpDescs =
  [ ("any" , '|', "union"       , union)
  , ("all" , '&', "intersection", intersection)
  , ("diff", '~', "difference"  , difference)
  ]

mkSetFunctions :: SetOpDesc -> [OrthoLangFunction]
mkSetFunctions (foldName, opChar, opName, setFn) = [setBop, setFold]
  where
    mkBopDesc  name = name ++ " : X.list -> X.list -> X.list"
    mkFoldDesc name = name ++ " : X.list.list -> X.list"
    setBop = OrthoLangFunction
      { fNames     = [[opChar], opName]
      , fTypeCheck = tSetBop
      , fTypeDesc  = mkBopDesc [opChar]
      , fFixity    = Infix, fTags = []
      , fNewRules = Nothing, fOldRules = rSetBop foldName setFn
      }
    setFold = OrthoLangFunction
      { fNames     = [foldName]
      , fTypeCheck = tSetFold
      , fTypeDesc  = mkFoldDesc foldName
      , fFixity    = Prefix, fTags = []
      , fNewRules = Nothing, fOldRules = rSetFold (foldr1 setFn)
      }

-- if the user gives two lists but of different types, complain that they must
-- be the same. if there aren't two lists at all, complain about that first
tSetBop :: [OrthoLangType] -> Either String OrthoLangType
tSetBop actual@[ListOf a, ListOf b]
  | typeMatches a b = fmap ListOf $ nonEmptyType [a, b]
  | otherwise = Left $ typeError [ListOf a, ListOf a] actual
tSetBop _ = Left "Type error: expected two lists of the same type"

tSetFold :: [OrthoLangType] -> Either String OrthoLangType
tSetFold [ListOf (ListOf x)] = Right $ ListOf x
tSetFold _ = Left "expecting a list of lists"

-- apply a set operation to two lists (converted to sets first)
-- TODO if order turns out to be important in cuts, call them lists
rSetBop :: String -> (Set String -> Set String -> Set String)
     -> OrthoLangState -> OrthoLangExpr -> Rules ExprPath
rSetBop name fn s (OrthoLangBop rtn salt deps _ s1 s2) = rSetFold (foldr1 fn) s fun
  where
    fun = OrthoLangFun  rtn salt deps name [lst]
    lst = OrthoLangList rtn salt deps [s1, s2]
rSetBop _ _ _ _ = fail "bad argument to rSetBop"

rSetFold :: ([Set String] -> Set String) -> OrthoLangState -> OrthoLangExpr -> Rules ExprPath
rSetFold fn s@(_, cfg, ref, ids) e@(OrthoLangFun _ _ _ _ [lol]) = do
  (ExprPath setsPath) <- rExpr s lol
  let oPath      = fromOrthoLangPath cfg $ exprPath s e
      oPath'     = cfgTmpDir cfg </> oPath
      oPath''    = debugRules cfg "rSetFold" e oPath
      (ListOf t) = typeOf lol
  oPath %> \_ -> aSetFold cfg ref ids fn t oPath' setsPath
  return (ExprPath oPath'')
rSetFold _ _ _ = fail "bad argument to rSetFold"

aSetFold :: OrthoLangConfig -> Locks -> HashedIDsRef -> ([Set String] -> Set String)
         -> OrthoLangType -> FilePath -> FilePath -> Action ()
aSetFold cfg ref idsRef fn (ListOf etype) oPath setsPath = do
  setPaths  <- readPaths cfg ref setsPath
  setElems  <- mapM (readStrings etype cfg ref) (map (fromOrthoLangPath cfg) setPaths)
  setElems' <- liftIO $ mapM (canonicalLinks cfg etype) setElems
  ids <- liftIO $ readIORef idsRef
  let sets = map fromList setElems'
      sets' = (map . Set.map) (unhashIDs False ids) sets -- TODO will this work?
      oLst = toList $ fn sets'
      oPath' = traceA "aSetFold" oPath [oPath, setsPath]
  oLst'' <- if etype `elem` [str, num]
              then mapM return oLst
              else dedupByContent cfg ref oLst -- TODO remove?
  writeStrings etype cfg ref oPath' oLst''
aSetFold _ _ _ _ _ _ _ = fail "bad argument to aSetFold"

-- a kludge to resolve the difference between load_* and load_*_each paths
-- TODO remove this or shunt it into Paths.hs or something!
canonicalLinks :: OrthoLangConfig -> OrthoLangType -> [FilePath] -> IO [FilePath]
canonicalLinks cfg rtn =
  if rtn `elem` [str, num]
    then return
    else \ps -> mapM (resolveSymlinks (Just $ cfgTmpDir cfg)) ps

-- TODO would resolving symlinks be enough? if so, much less disk IO!
-- see https://stackoverflow.com/a/8316542/429898
dedupByContent :: OrthoLangConfig -> Locks -> [FilePath] -> Action [FilePath]
dedupByContent cfg ref paths = do
  -- TODO if the paths are already in the load cache, no need for content?
  hashes <- mapM (hashContent cfg ref) $ map (toOrthoLangPath cfg) paths
  let paths' = map fst $ nubBy ((==) `on` snd) $ zip paths hashes
  return paths'

some :: OrthoLangFunction
some = OrthoLangFunction
  { fNames     = ["some"]
  , fTypeCheck = tSetFold
  , fTypeDesc  = "some : X.list.list -> X.list"
  , fFixity    = Prefix, fTags = []
  , fNewRules = Nothing, fOldRules = rSome
  }

rSome :: OrthoLangState -> OrthoLangExpr -> Rules ExprPath
rSome s (OrthoLangFun rtn salt deps _ lol) = rExpr s diffExpr
  where
    anyExpr  = OrthoLangFun rtn salt deps "any" lol
    allExpr  = OrthoLangFun rtn salt deps "all" lol
    diffExpr = OrthoLangBop rtn salt deps "~" anyExpr allExpr
rSome _ _ = fail "bad argument to rSome"
