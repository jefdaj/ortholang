module ShortCut.Core.Types
  -- data structures
  ( CutAssign
  , CutExpr(..)
  , CutConfig(..)
  -- , WrapperConfig(..)
  , CutType(..)
  , CutVar(..)
  , CutScript
  , CutState
  -- , Assoc(..) -- we reuse this from Parsec
  , CutFixity(..)
  -- parse monad
  , ParseM
  , runParseM
  -- repl monad
  -- , print
  , prompt
  , runReplM
  , ReplM
  -- misc
  -- , prettyShow
  , str, num -- TODO load these from modules
  , typeOf
  -- , nonEmptyType
  , extOf
  , depsOf
  , rDepsOf
  , defaultShow
  -- module stuff (in flux)
  , CutFunction(..)
  , CutModule(..)
  , saltOf
  , setSalt
  , prefixOf
  -- wrappers to prevent confusing the various paths
  , CacheDir(..)
  , ExprPath(..)
  , VarPath(..)
  , ResPath(..)
  -- misc experimental stuff
  , extractExprs
  , ActionFn
  , RulesFn
  , TypeChecker
  )
  where

-- import Prelude hiding (print)
import qualified Text.Parsec as P

import Development.Shake              (Rules, Action)
import Control.Monad.State.Lazy       (StateT, execStateT, lift)
import Control.Monad.Trans.Maybe      (MaybeT(..), runMaybeT)
import Data.List                      (nub)
import System.Console.Haskeline       (InputT, getInputLine, runInputT, Settings)
import Text.Parsec                    (ParseError)
import Data.Maybe            (fromJust)
-- import Text.PrettyPrint.HughesPJClass (Doc, text, doubleQuotes)

-- TODO where should these go?
-- TODO edit ActionFn?
type ActionFn    = CutConfig -> CacheDir -> [ExprPath] -> Action ()
type RulesFn     = CutState -> CutExpr -> Rules ExprPath
type TypeChecker = [CutType] -> Either String CutType

newtype CacheDir = CacheDir FilePath deriving Show -- ~/.shortcut/cache/<modname>
newtype ExprPath = ExprPath FilePath deriving Show -- ~/.shortcut/exprs/<fnname>/<hash>.<type>
newtype VarPath  = VarPath  FilePath deriving Show -- ~/.shortcut/vars/<varname>.<type>
newtype ResPath  = ResPath  FilePath deriving Show -- ~/.shortcut/vars/result[.<hash>.<type>]

-- Filename extension, which in ShortCut is equivalent to variable type
-- TODO can this be done better with phantom types?
-- data Ext = ListOf Ext | Ext String
  -- deriving (Eq, Show, Read)

newtype CutVar = CutVar String deriving (Eq, Show, Read)
 
-- the common fields are:
-- * return type
-- * salt, which can be changed to force re-evaluation of an expr + all depends
--   (it should start at 0 and be incremented, though that doesn't really matter)
-- TODO start from 1 instead of 0?
-- TODO test that it works correctly! in particular, it should go thru refs!
--      (do we need to add salts of subepxressions or something? or use randoms?)
-- * list of dependencies (except lits don't have any)
data CutExpr
  = CutLit CutType Int String
  | CutRef CutType Int [CutVar] CutVar -- do refs need a salt? yes! (i think?)
  | CutBop CutType Int [CutVar] String  CutExpr CutExpr
  | CutFun CutType Int [CutVar] String [CutExpr]
  | CutList CutType Int [CutVar] [CutExpr]
  deriving (Eq, Show)

-- TODO is this not actually needed? seems "show expr" handles it?
saltOf :: CutExpr -> Int
saltOf (CutLit _ n _)       = n
saltOf (CutRef _ n _ _)     = n
saltOf (CutBop _ n _ _ _ _) = n
saltOf (CutFun _ n _ _ _)   = n
saltOf (CutList _ n _ _)     = n

setSalt :: Int -> CutExpr -> CutExpr
setSalt n (CutLit t _ s)          = CutLit t n s
setSalt n (CutRef t _ ds v)       = CutRef t n ds v
setSalt n (CutBop t _ ds s e1 e2) = CutBop t n ds s e1 e2
setSalt n (CutFun t _ ds s es)    = CutFun t n ds s es
setSalt n (CutList t _ ds es)      = CutList t n ds es

-- TODO add names to the CutBops themselves... or associate with prefix versions?
prefixOf :: CutExpr -> String
prefixOf (CutLit rtn _ _     ) = extOf rtn
prefixOf (CutFun _ _ _ name _) = name
prefixOf (CutList _ _ _ _    ) = "list"
prefixOf (CutRef _ _ _ _     ) = error  "CutRefs don't need a prefix"
prefixOf (CutBop _ _ _ n _ _ ) = case n of
                                   "+" -> "add"
                                   "-" -> "subtract"
                                   "*" -> "multiply"
                                   "/" -> "divide"
                                   "~" -> "difference"
                                   "&" -> "intersection"
                                   "|" -> "union"
                                   _   -> error "unknown CutBop"


-- TODO have a separate CutAssign for "result"?
type CutAssign = (CutVar, CutExpr)
type CutScript = [CutAssign]

data CutType
  = Empty -- TODO remove this? should never be a need to define an empty list
  | ListOf CutType
  | CutType
    { tExt  :: String
    , tDesc :: String -- TODO include a longer help text too
    , tShow :: FilePath -> IO String
    }
  -- deriving (Eq, Show, Read)

defaultShow :: FilePath -> IO String
defaultShow = fmap (unlines . fmtLines . lines) . readFile
  where
    nLines      = 5
    fmtLine  l  = if length l > 80 then take 77 l ++ "..." else l
    fmtLines ls = let nPlusOne = map fmtLine $ take (nLines + 1) ls
                  in if length nPlusOne > nLines
                    then init nPlusOne ++ ["..."]
                    else nPlusOne

-- TODO is it dangerous to just assume they're the same by extension?
--      maybe we need to assert no duplicates while loading modules?
-- TODO should this use typesMatch?
instance Eq CutType where
  Empty      == Empty      = True
  (ListOf a) == (ListOf b) = a == b
  t1         == t2         = extOf t1 == extOf t2

instance Show CutType where
  show = extOf

typeOf :: CutExpr -> CutType
typeOf (CutLit  t _ _      ) = t
typeOf (CutRef  t _ _ _    ) = t
typeOf (CutBop  t _ _ _ _ _) = t
typeOf (CutFun  t _ _ _ _  ) = t
typeOf (CutList t _ _ _    ) = ListOf t -- t can be Empty
-- typeOf (CutList _ _ _ ts     ) = ListOf $ nonEmptyType $ map typeOf ts
-- typeOf (CutList _ _ _ []     ) = Empty
-- typeOf (CutList _ _ _ []     ) = ListOf Empty

-- Works around a bug where if the first element is an empty list but others
-- have elements, it would call the whole thing an "emptylist.list".
-- Note no typechecking happens here; heterogenous lists won't be noticed.
-- nonEmptyType :: [CutExpr] -> CutType
-- nonEmptyType    []  = Empty
-- nonEmptyType (x:[]) = typeOf x -- catches (ListOf Empty)
-- nonEmptyType (_:xs) = nonEmptyType xs

-- note that traceShow in here can cause an infinite loop
-- and that there will be an issue if it's called on Empty alone
extOf :: CutType -> String
extOf Empty      = "empty" -- for lists with nothing in them yet
extOf (ListOf t) = extOf t ++ ".list"
extOf t          = tExt t

varOf :: CutExpr -> [CutVar]
varOf (CutRef _ _ vs _) = vs
varOf _                 = []

depsOf :: CutExpr -> [CutVar]
depsOf (CutLit  _ _ _         ) = []
depsOf (CutRef  _ _ vs v      ) = v:vs
depsOf (CutBop  _ _ vs _ e1 e2) = nub $ vs ++ concatMap varOf [e1, e2]
depsOf (CutFun  _ _ vs _ es   ) = nub $ vs ++ concatMap varOf es
depsOf (CutList _ _ vs   es   ) = nub $ vs ++ concatMap varOf es

rDepsOf :: CutScript -> CutVar -> [CutVar]
rDepsOf scr var = map fst rDeps
  where
    rDeps = filter (\(_,e) -> isRDep e) scr
    isRDep expr = elem var $ depsOf expr

-- TODO move to modules as soon as parsing works again
-- TODO keep literals in the core along with refs and stuff? seems reasonable
-- TODO how about lists/sets, are those core too?

str :: CutType
str = CutType
  { tExt  = "str"
  , tDesc = "string"
  -- the init is for removing newlines
  , tShow = \f -> readFile f >>= (return . (\x -> "\"" ++ x ++ "\"") . init)
  }

num :: CutType
num = CutType
  { tExt  = "num"
  , tDesc = "number in scientific notation"
  , tShow = \f -> readFile f >>= return . init
  }

------------
-- config --
------------

-- TODO always load defaults for WorkDir, TmpDir, Verbose
-- TODO make these into FilePaths and an Int/Bool
-- TODO rename cfg prefix to just c?
data CutConfig = CutConfig
  { cfgScript  :: Maybe FilePath
  , cfgTmpDir  :: FilePath
  , cfgWorkDir :: FilePath
  , cfgDebug   :: Bool
  , cfgModules :: [CutModule]
  , cfgWrapper :: Maybe FilePath
  , cfgReport  :: Maybe String
  }
  deriving Show

-----------------
-- Parse monad --
-----------------

type CutState = (CutScript, CutConfig)
type ParseM a = P.Parsec String CutState a

runParseM :: ParseM a -> CutState -> String -> Either ParseError a
runParseM p s = P.runParser p s "somefile"

----------------
-- Repl monad --
----------------

type ReplM a = StateT CutState (MaybeT (InputT IO)) a

-- TODO use useFile(Handle) for stdin?
-- TODO use getExternalPrint to safely print during Tasty tests!
runReplM :: Settings IO -> ReplM a -> CutState -> IO (Maybe CutState)
runReplM settings replm state =
  runInputT settings $ runMaybeT $ execStateT replm state

prompt :: String -> ReplM (Maybe String)
prompt = lift . lift . getInputLine

-- eODO does this need rewriting for externalPrint?
-- print :: (String -> IO ()) -> String -> ReplM ()
-- print pFn = liftIO . pFn
-- print pFn = lift . lift . pFn

--------------------------------
-- Module stuff (all in flux) --
--------------------------------

-- TODO replace current CutType with something like this:
-- TODO does eq make sense here?
-- data CutType = CutType
--   { tName :: String
--   , tExt  :: String
--   , tDesc :: String
--   }
--   deriving (Eq, Show, Read)

-- TODO should there be any more fundamental difference between fns and bops?
data CutFixity = Prefix | Infix
  deriving (Eq, Show, Read)

-- type CutSignature = CutType -> (CutType, [CutType])

-- TODO does eq make sense here? should i just be comparing names??
-- TODO pretty instance like "union: [set, set] -> set"? just "union" for now
data CutFunction = CutFunction
  { fName      :: String
  , fTypeCheck :: [CutType] -> Either String CutType
  , fFixity    :: CutFixity
  , fRules  :: CutState -> CutExpr -> Rules ExprPath
  -- , fHidden    :: Bool -- hide "internal" functions like reverse blast
  }
  -- deriving (Eq, Read)

-- TODO does eq make sense here?
data CutModule = CutModule
  { mName :: String
  , mFunctions :: [CutFunction]
  }
  -- deriving (Eq, Read)

-- TODO what about prettyShow in Pretty.hs?
instance Show CutModule where
  show = mName

-- TODO what if it's a function call?
-- do we have to make a rule that you can't use those?
-- (uuuugly! but not a show-stopper for now)
extractExprs :: CutScript -> CutExpr -> [CutExpr]
extractExprs  _  (CutList _ _ _ es) = es
extractExprs scr (CutRef  _ _ _ v ) = extractExprs scr $ fromJust $ lookup v scr
extractExprs _   (CutFun _ _ _ _ _) = error explainFnBug
extractExprs scr (CutBop _ _ _ _ l r) = extractExprs scr l ++ extractExprs scr r
extractExprs  _   e               = error $ "bad arg to extractExprs: " ++ show e

-- TODO will this get printed, or will there just be a parse error?
explainFnBug :: String
explainFnBug =
  "You've stumbled on an outstanding bug. Sorry about that! \
  \The problem is that when doing transformations involving lists \
  \like repeat or map, ShortCut can't \"see\" through future function calls; \
  \it can only manipulate lists whose elements are known *before* running the \
  \program. If you want Jeff to consider rewriting some things to fix that, \
  \drop him a line!"
