module ShortCut.Core.Types
  -- data structures
  ( CutAssign
  , CutExpr(..)
  , CutConfig(..)
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
  , print
  , prompt
  , runReplM
  , ReplM
  -- misc
  -- , prettyShow
  , str, num -- TODO load these from modules
  , typeOf
  , extOf
  , depsOf
  , rDepsOf
  , defaultCat
  -- module stuff (in flux)
  , CutFunction(..)
  , CutModule(..)
  -- , getSalt
  , setSalt
  -- wrappers to prevent confusing the various paths
  , CacheDir(..)
  , ExprPath(..)
  , VarPath(..)
  , ResPath(..)
  )
  where

import Prelude hiding (print)
import qualified Text.Parsec as P

import Control.Monad.State.Lazy       (StateT, execStateT, lift)
import Control.Monad.Trans.Maybe      (MaybeT(..), runMaybeT)
import Data.List                      (nub)
import Development.Shake              (Rules)
import System.Console.Haskeline       (InputT, getInputLine, runInputT,
                                       defaultSettings)
import Text.Parsec                    (ParseError)
import Text.PrettyPrint.HughesPJClass (Doc, text, doubleQuotes)
import Control.Monad.IO.Class   (liftIO)

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
  = CutLit  CutType Int String
  | CutRef  CutType Int [CutVar] CutVar -- do refs need a salt? yes! (i think?)
  | CutBop  CutType Int [CutVar] String  CutExpr CutExpr
  | CutFun  CutType Int [CutVar] String [CutExpr]
  | CutList CutType Int [CutVar] [CutExpr]
  deriving (Eq, Show)

-- TODO is this not actually needed? seems "show expr" handles it?
-- getSalt :: CutExpr -> Int
-- getSalt (CutLit  _ n _)       = n
-- getSalt (CutRef  _ n _ _)     = n
-- getSalt (CutBop  _ n _ _ _ _) = n
-- getSalt (CutFun  _ n _ _ _)   = n
-- getSalt (CutList _ n _ _)     = n

setSalt :: Int -> CutExpr -> CutExpr
setSalt n (CutLit  t _ s)          = CutLit  t n s
setSalt n (CutRef  t _ ds v)       = CutRef  t n ds v
setSalt n (CutBop  t _ ds s e1 e2) = CutBop  t n ds s e1 e2
setSalt n (CutFun  t _ ds s es)    = CutFun  t n ds s es
setSalt n (CutList t _ ds es)      = CutList t n ds es

-- TODO have a separate CutAssign for "result"?
type CutAssign = (CutVar, CutExpr)
type CutScript = [CutAssign]

data CutType
  = EmptyList -- TODO remove this? should never be a need to define an empty list
  | ListOf CutType
  | CutType
    { tExt  :: String
    , tDesc :: String -- TODO include a longer help text too
    , tCat  :: String -> IO Doc
    }
  -- deriving (Eq, Show, Read)

defaultCat :: String -> IO Doc
defaultCat s = (return . text . unlines) toShow
  where
    nLines = 5
    toShow = if length (lines s) > nLines
               then (take nLines $ lines s) ++ ["..."]
               else lines s

-- TODO is it dangerous to just assume they're the same by extension?
--      maybe we need to assert no duplicates while loading modules?
instance Eq CutType where
  EmptyList  == EmptyList  = True
  (ListOf a) == (ListOf b) = a == b
  t1         == t2         = extOf t1 == extOf t2

instance Show CutType where
  show = extOf

typeOf :: CutExpr -> CutType
typeOf (CutLit  t _ _          ) = t
typeOf (CutRef  t _ _ _        ) = t
typeOf (CutBop  t _ _ _ _ _    ) = t
typeOf (CutFun  t _ _ _ _      ) = t
typeOf (CutList EmptyList _ _ _) = EmptyList
typeOf (CutList t  _ _ _       ) = ListOf t

-- note that traceShow in here can cause an infinite loop
extOf :: CutType -> String
extOf (ListOf t   ) = extOf t ++ ".list"
extOf EmptyList     = "list"
extOf t = tExt t

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
  , tCat  = return . doubleQuotes . text . init
  }

num :: CutType
num = CutType
  { tExt  = "num"
  , tDesc = "number in scientific notation"
  , tCat  = return . text . init
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
  , cfgDebug   :: Bool
  , cfgModules :: [CutModule]
  }
  deriving (Show)

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
runReplM :: ReplM a -> CutState -> IO (Maybe CutState)
runReplM r s = runInputT defaultSettings $ runMaybeT $ execStateT r s

prompt :: String -> ReplM (Maybe String)
prompt = lift . lift . getInputLine

-- TODO does this need rewriting for externalPrint?
print :: (String -> IO ()) -> String -> ReplM ()
print pFn = liftIO . pFn
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
  , fCompiler  :: CutState -> CutExpr -> Rules ExprPath
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
