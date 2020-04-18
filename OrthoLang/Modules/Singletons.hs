module OrthoLang.Modules.Singletons
  (

  -- * Expr transformers
    withSingleton
  , withSingletonArg
  , withSingletons

  -- * OrthoLang functions for transforming (TODO remove?)
  -- , singletons

  -- * OrthoLang module
  , olModule -- TODO don't export this?

  )
  where

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter
import Control.Monad (forM)
import Data.Maybe (fromJust)

debugA' :: String -> String -> Action ()
debugA' name msg = debugA ("ortholang.modules.singletons." ++ name) msg

olModule :: Module
olModule = Module
  { mName = "BlastDB"
  , mDesc = "Create, load, and download BLAST databases"
  , mTypes = []
  , mGroups = []
  , mEncodings = []
  , mFunctions =
    [ singletons -- TODO non-plural version too
    ]
  }

-- | Take a single expr and wrap it in a singleton list
withSingleton :: Expr -> Expr
withSingleton e = Lst (typeOf e) (depsOf e) [e]

-- | Take a list and wrap it in a singleton list, making a list of lists
-- TODO could it be implemented using 'withSingleton' above?
withSingletons :: Expr -> Expr
withSingletons e = Fun (ListOf $ typeOf e) (saltOf e) (depsOf e) "singletons" [e]

-- | Take a function call with one arg, and make the arg a singleton list
withSingletonArg :: Expr -> Expr
withSingletonArg (Fun rtn salt deps name [s]) = Fun rtn salt deps name [withSingleton s]
withSingletonArg e = error $ "bad argument to withSingletonArg: " ++ show e

-- Only used for the makeblastdb_*_each functions so far
-- TODO hide from users
singletons :: Function
singletons =
  let name = "singletons"
  in Function
    { fOpChar    = Nothing
    , fName      = name
    , fTags      = [Hidden]
    -- , fTypeDesc  = name ++ " : X.list -> X.list.list"
    -- , fTypeCheck = tSingletons
    , fInputs    =           [ListSigs (AnyType "any type")]
    , fOutput    =  ListSigs (ListSigs (AnyType "any type"))
    , fNewRules  = NewNotImplemented
    , fOldRules  = rSingletons
    }

-- (ListOf (Some ot "any type")) (ListOf (ListOf (Some ot "any type")))
-- shown as "t.list -> t.list.list, where t is any type"
-- tSingletons :: [Type] -> Either String Type
-- tSingletons [ListOf x] = Right $ ListOf $ ListOf x
-- tSingletons _ = Left "tSingletons expected a list"

rSingletons :: RulesFn
rSingletons scr expr@(Fun rtn _ _ _ [listExpr]) = do
  (ExprPath listPath') <- rExpr scr listExpr
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let outPath  = exprPath cfg dRef scr expr
      loc = "modules.singletons.rSingletons"
      outPath' = fromPath loc cfg outPath
      listPath = toPath loc cfg listPath'
      (ListOf (ListOf t)) = rtn
  outPath' %> \_ -> aSingletons t outPath listPath
  return $ ExprPath outPath'
rSingletons _ _ = fail "bad argument to rSingletons"

aSingletons :: Type -> Action1
aSingletons elemType outPath listPath = do
  cfg <- fmap fromJust getShakeExtra
  let listPath' = fromPath loc cfg listPath
      outPath'  = fromPath loc cfg outPath
      loc = "modules.singletons.aSingletons"
      dbg = debugA' loc
  dbg $ "listpath': " ++ listPath'
  dbg $ "outpath': " ++ outPath'
  elems <- readStrings loc elemType listPath'
  dbg $ "elems: " ++ show elems
  singletonPaths <- forM elems $ \e -> do
    let singletonPath' = cachedLinesPath cfg [e] -- TODO nondeterministic?
        singletonPath  = toPath loc cfg singletonPath'
    dbg $ "singletonPath': " ++ singletonPath'
    writeStrings loc elemType singletonPath' [e]
    return singletonPath
  writePaths loc outPath' singletonPaths -- TODO nondeterministic?
