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
  -- module stuff (in flux)
  , CutFunction(..)
  , CutModule(..)
  )
  where

import Prelude hiding (print)
import qualified Text.Parsec as P

import Data.Scientific           (Scientific())
import Text.Parsec               (ParseError)
import Control.Monad.State.Lazy  (StateT, execStateT, lift, liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import System.Console.Haskeline  (InputT, getInputLine, runInputT,
                                  defaultSettings, outputStrLn)
import Development.Shake (Rules)

-- Filename extension, which in ShortCut is equivalent to variable type
-- TODO can this be done better with phantom types?
-- data Ext = ListOf Ext | Ext String
  -- deriving (Eq, Show, Read)

newtype CutVar = CutVar String deriving (Eq, Show, Read)
 
-- TODO only keep the extensions themselves here, not the whole CutType?
data CutExpr
  = CutLit CutType String
  | CutRef CutType CutVar
  | CutBop CutType String  CutExpr CutExpr
  | CutFun CutType String [CutExpr]
  | CutSet CutType [CutExpr]
  deriving (Eq, Show, Read)

type CutAssign = (CutVar, CutExpr)
type CutScript = [CutAssign]

data CutType
  = CutType String String
  | ListOf CutType
  | EmptyList -- TODO remove this? should never be a need to define an empty list
  deriving (Eq, Show, Read)

typeOf :: CutExpr -> CutType
typeOf (CutLit t _    ) = t
typeOf (CutRef t _    ) = t
typeOf (CutBop t _ _ _) = t
typeOf (CutFun t _ _  ) = t
typeOf (CutSet EmptyList _) = EmptyList
typeOf (CutSet t _    ) = ListOf t

-- TODO move to modules as soon as parsing works again
-- TODO keep literals in the core along with refs and stuff? seems reasonable
-- TODO how about lists/sets, are those core too?
str, num :: CutType
str = CutType "str"    "string"
num = CutType "num"    "number in scientific notation"

------------
-- config --
------------

-- TODO always load defaults for WorkDir, TmpDir, Verbose
-- TODO make these into FilePaths and an Int/Bool
-- TODO rename cfg prefix to just c?
data CutConfig = CutConfig
  { cfgScript  :: Maybe FilePath
  , cfgTmpDir  :: FilePath
  , cfgVerbose :: Bool
  , cfgModules :: [CutModule]
  }
  -- deriving (Eq, Show, Read)

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

runReplM :: ReplM a -> CutState -> IO (Maybe CutState)
runReplM r s = runInputT defaultSettings $ runMaybeT $ execStateT r s

prompt :: String -> ReplM (Maybe String)
prompt = lift . lift . getInputLine

print :: String -> ReplM ()
print = lift . lift . outputStrLn

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

type CutSignature = CutType -> (CutType, [CutType])

-- TODO does eq make sense here? should i just be comparing names??
-- TODO pretty instance like "union: [set, set] -> set"? just "union" for now
data CutFunction = CutFunction
  { fName      :: String
  , fTypeCheck :: [CutType] -> Either String CutType
  , fFixity    :: CutFixity
  , fCompiler  :: CutConfig -> CutExpr -> Rules FilePath
  }
  -- deriving (Eq, Read)

-- TODO does eq make sense here?
data CutModule = CutModule
  { mName :: String
  , mFunctions :: [CutFunction]
  }
  -- deriving (Eq, Read)
