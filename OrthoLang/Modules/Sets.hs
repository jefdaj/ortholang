module OrthoLang.Modules.Sets where

-- TODO unify bops and funs into one thing (all fns have optional infix version?)
-- TODO writeStrings should delete the outfile on errors!
-- TODO remove long form of diff?

import Development.Shake
import OrthoLang.Core

import qualified Data.Set as Set

import Data.Function               (on)
import Data.List                   (nubBy)
import Data.Set                    (Set, union, difference, intersection, fromList,
                                    toList)
import Data.IORef                  (readIORef)
import Development.Shake.FilePath  ((</>))
import OrthoLang.Core (rExpr, debugRules)
import OrthoLang.Core       (readStrings, readPaths, writeStrings, traceA, hashContent)
-- import OrthoLang.Core         (debugRules, traceA)
import OrthoLang.Core         (exprPath, toPath, fromPath)
import OrthoLang.Core          (resolveSymlinks)
import OrthoLang.Core      (unhashIDs)
import Data.Maybe (fromJust)

orthoLangModule :: Module
orthoLangModule = Module
  { mName = "Sets"
  , mDesc = "Set operations for use with lists"
  , mTypes = []
  , mFunctions = some : map mkSetFunction setOpDescs
  }

type SetOpDesc =
  ( String -- name of the prefix function
  , Char   -- name of the infix/binary function
  , Set String -> Set String -> Set String -- haskell set op
  )

{- An awkward intermediate: until I get around to actually merging Bops into
 - Funs, we'll just create them from a common description. The Bop versions
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

mkSetFunction :: SetOpDesc -> Function
mkSetFunction (foldName, opChar, setFn) = setFold
  where
    -- mkBopDesc  name = name ++ " : X.list -> X.list -> X.list"
    mkFoldDesc name = name ++ " : X.list.list -> X.list"
    setFold = Function
      { fName = foldName
      , fOpChar    = Just opChar
      , fTypeCheck = tSetFold
      , fTypeDesc  = mkFoldDesc foldName
      , fTags = []
      , fNewRules = Nothing, fOldRules = rSetFold (foldr1 setFn)
      }

tSetFold :: [Type] -> Either String Type
tSetFold [ListOf (ListOf x)] = Right $ ListOf x
tSetFold _ = Left "expecting a list of lists"

rSetFold :: ([Set String] -> Set String) -> RulesFn
rSetFold fn scr e@(Fun _ _ _ _ [lol]) = do
  (ExprPath setsPath) <- rExpr scr lol
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let oPath      = fromPath cfg $ exprPath cfg dRef scr e
      oPath'     = cfgTmpDir cfg </> oPath
      oPath''    = debugRules cfg "rSetFold" e oPath
      (ListOf t) = typeOf lol
  oPath %> \_ -> aSetFold fn t oPath' setsPath
  return (ExprPath oPath'')
rSetFold _ _ _ = fail "bad argument to rSetFold"

aSetFold :: ([Set String] -> Set String)
         -> Type -> FilePath -> FilePath -> Action ()
aSetFold fn (ListOf etype) oPath setsPath = do
  setPaths  <- readPaths setsPath
  cfg <- fmap fromJust getShakeExtra
  setElems  <- mapM (readStrings etype) (map (fromPath cfg) setPaths)
  setElems' <- liftIO $ mapM (canonicalLinks cfg etype) setElems
  idsRef <- fmap fromJust getShakeExtra
  ids <- liftIO $ readIORef idsRef
  let sets = map fromList setElems'
      sets' = (map . Set.map) (unhashIDs False ids) sets -- TODO will this work?
      oLst = toList $ fn sets'
      oPath' = traceA "aSetFold" oPath [oPath, setsPath]
  oLst'' <- if etype `elem` [str, num]
              then mapM return oLst
              else dedupByContent oLst -- TODO remove?
  writeStrings etype oPath' oLst''
aSetFold _ _ _ _ = fail "bad argument to aSetFold"

-- a kludge to resolve the difference between load_* and load_*_each paths
-- TODO remove this or shunt it into Paths.hs or something!
canonicalLinks :: Config -> Type -> [FilePath] -> IO [FilePath]
canonicalLinks cfg rtn =
  if rtn `elem` [str, num]
    then return
    else \ps -> mapM (resolveSymlinks (Just $ cfgTmpDir cfg)) ps

-- TODO would resolving symlinks be enough? if so, much less disk IO!
-- see https://stackoverflow.com/a/8316542/429898
dedupByContent :: [FilePath] -> Action [FilePath]
dedupByContent paths = do
  -- TODO if the paths are already in the load cache, no need for content?
  cfg <- fmap fromJust getShakeExtra
  hashes <- mapM hashContent $ map (toPath cfg) paths
  let paths' = map fst $ nubBy ((==) `on` snd) $ zip paths hashes
  return paths'

some :: Function
some = Function
  { fOpChar = Nothing, fName = "some"
  , fTypeCheck = tSetFold
  , fTypeDesc  = "some : X.list.list -> X.list"
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rSome
  }

rSome :: RulesFn
rSome s (Fun rtn salt deps _ lol) = rExpr s diffExpr
  where
    anyExpr  = Fun rtn salt deps "any" lol
    allExpr  = Fun rtn salt deps "all" lol
    diffExpr = Bop rtn salt deps "~" anyExpr allExpr
rSome _ _ = fail "bad argument to rSome"
