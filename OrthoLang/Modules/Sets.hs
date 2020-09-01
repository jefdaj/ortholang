module OrthoLang.Modules.Sets where

-- TODO unify bops and funs into one thing (all fns have optional infix version?)
-- TODO writeStrings should delete the outfile on errors!
-- TODO remove long form of diff?

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter
import qualified Data.Set as Set

import Data.Set      (Set, union, difference, intersection, fromList, toList)
import Data.Function (on)
import Data.IORef    (readIORef)
import Data.List     (nubBy)
import Data.Maybe    (fromJust)

------------
-- module --
------------

olModule :: Module
olModule = Module
  { mName = "Sets"
  , mDesc = "Set operations for use with lists"
  , mTypes = []
  , mGroups = []
  , mEncodings = []
  , mFunctions = some : map mkSetFunction setOpDescs
  }

--------------------
-- any, all, diff --
--------------------

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
mkSetFunction (foldName, opChar, setFn) = newBop foldName opChar al al (aSetFold setFn)
  [] -- TODO nondeterministic?
  where
    al = ListSigs $ AnyType "the type of the list elements"

-- TODO where is the setFn used?
-- aSetFold :: NewAction1
aSetFold :: (Set String -> Set String -> Set String)
         -> ExprPath -> FilePath -> Action ()
aSetFold setFn (ExprPath oPath) setsPath = do
  let loc = "modules.sets.aSetFold"
  setPaths  <- readPaths loc setsPath
  cfg  <- fmap fromJust getShakeExtra
  dRef <- fmap fromJust getShakeExtra
  (ListOf (ListOf eType)) <- liftIO $ decodeNewRulesType cfg dRef (ExprPath setsPath) -- TODO convenience fn for this as Action
  setElems  <- mapM (readStrings loc eType) (map (fromPath loc cfg) setPaths)
  setElems' <- liftIO $ mapM (canonicalLinks cfg eType) setElems
  idsRef <- fmap fromJust getShakeExtra
  ids <- liftIO $ readIORef idsRef
  let sets = map fromList setElems'
      sets' = (map . Set.map) (unhashIDs False ids) sets -- TODO will this work?
      oLst = toList $ (foldr1 setFn) sets'
      oPath' = traceA loc oPath [oPath, setsPath]
  oLst'' <- if eType `elem` [str, num]
              then mapM return oLst
              else dedupByContent oLst -- TODO remove?
  writeStrings loc eType oPath' oLst''

-- a kludge to resolve the difference between load_* and load_*_each paths
-- TODO remove this or shunt it into Paths.hs or something!
canonicalLinks :: Config -> Type -> [FilePath] -> IO [FilePath]
canonicalLinks cfg rtn =
  if rtn `elem` [str, num]
    then return
    else \ps -> mapM (resolveSymlinks (Just [tmpdir cfg])) ps

-- TODO would resolving symlinks be enough? if so, much less disk IO!
-- see https://stackoverflow.com/a/8316542/429898
dedupByContent :: [FilePath] -> Action [FilePath]
dedupByContent paths = do
  -- TODO if the paths are already in the load cache, no need for content?
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.sets.dedupByContent"
  hashes <- mapM hashContent $ map (toPath loc cfg) paths
  let paths' = map fst $ nubBy ((==) `on` snd) $ zip paths hashes
  return paths'

----------
-- some --
----------

some :: Function
some = Function
  { fOpChar = Nothing, fName = "some"
  -- , fTypeCheck = tSetFold
  -- , fTypeDesc  = "some : X.list.list -> X.list"
  , fInputs = [ListSigs (ListSigs (AnyType "any type"))]
  , fOutput =  ListSigs (AnyType "any type")
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rSome
  }

-- TODO rewrite as an ExprExpansion
rSome :: RulesFn
rSome s (Fun rtn seed deps _ lol) = rExpr s diffExpr
  where
    anyExpr  = Fun rtn seed deps "any" lol
    allExpr  = Fun rtn seed deps "all" lol
    diffExpr = Bop rtn seed deps "~" anyExpr allExpr
rSome _ _ = fail "bad argument to rSome"
