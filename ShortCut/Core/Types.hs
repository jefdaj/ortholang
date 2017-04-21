{-# LANGUAGE FlexibleInstances #-}

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
  , prettyShow
  , str, num -- TODO load these from modules
  , typeOf
  -- module stuff (in flux)
  , CutFunction(..)
  , CutModule(..)
  )
  where

import Prelude hiding (print)
import qualified Text.Parsec as P
import Text.PrettyPrint.HughesPJClass

-- import Control.Monad.Identity    (Identity)
import Data.Scientific           (Scientific())
import Text.Parsec               (ParseError)
-- import Text.Parsec.Expr          (Assoc(..))
import Control.Monad.State.Lazy  (StateT, execStateT, lift, liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import System.Console.Haskeline  (InputT, getInputLine, runInputT,
                                  defaultSettings, outputStrLn)
import Development.Shake (Rules)

-- Filename extension, which in ShortCut is equivalent to variable type
-- TODO can this be done better with phantom types?
-- data Ext = SetOf Ext | Ext String
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
  | SetOf CutType
  deriving (Eq, Show, Read)

instance Pretty CutType where
  pPrint (CutType ext desc) = text ext <+> parens (text desc)
  pPrint (SetOf t) = text "set of" <+> pPrint t <> text "s"

typeOf :: CutExpr -> CutType
typeOf (CutLit t _    ) = t
typeOf (CutRef t _    ) = t
typeOf (CutBop t _ _ _) = t
typeOf (CutFun t _ _  ) = t
typeOf (CutSet t _    ) = SetOf t

-- TODO move to modules as soon as parsing works again
-- TODO keep literals in the core along with refs and stuff? seems reasonable
-- TODO how about lists/sets, are those core too?
str, num :: CutType
str = CutType "str"    "string"
num = CutType "num"    "number in scientific notation"

---------------------
-- pretty printers --
---------------------

-- I would put these in a separate Pretty.hs, but that causes orphan instances

instance Pretty CutVar where
  pPrint (CutVar s) = text s

-- TODO add descriptions here? if so, need to separate actual extension code
-- instance Pretty Ext where
--   pPrint (SetOf e) = pPrint e <> text "s"
--   pPrint (Ext   e) = text e

instance {-# OVERLAPPING #-} Pretty CutAssign where
  pPrint (v, e) = pPrint v <+> text "=" <+> pPrint e
  -- this adds type info, but makes the pretty-print not valid source code
  -- pPrint (v, e) = text (render (pPrint v) ++ "." ++ render (pPrint $ typeExt e))

instance {-# OVERLAPPING #-} Pretty CutScript where
  pPrint [] = empty
  pPrint as = fsep $ map pPrint as

instance Pretty CutExpr where
  pPrint e@(CutLit _ s)
    | typeOf e == num = text $ show $ (read s :: Scientific)
    | otherwise       = text $ show s
  pPrint (CutRef _ v)       = pPrint v
  pPrint (CutFun _ s es)    = text s <+> sep (map pNested es)
  pPrint (CutSet _ _)       = undefined -- TODO figure this out!
  pPrint (CutBop _ c e1 e2) = if (length $ render $ one) > 80 then two else one
    where
      bopWith fn = fn (pPrint e1) (nest (-2) (text c) <+> pPrint e2)
      one = bopWith (<+>)
      two = bopWith ($+$)

-- this adds parens around nested function calls
-- without it things can get really messy!
pNested :: CutExpr -> Doc
pNested e@(CutFun _ _ _  ) = parens $ pPrint e
pNested e@(CutBop _ _ _ _) = parens $ pPrint e
pNested e = pPrint e

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

instance Pretty CutConfig where
  pPrint cfg = vcat $ map (\(k,fn) -> text k <+> text "=" <+> text (fn cfg))
    [ ("script" , show . cfgScript )
    , ("tmpdir" , show . cfgTmpDir )
    , ("verbose", show . cfgVerbose)
    ]

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

-- TODO GET RID OF THAT EXTRA RETURN TYPE ARG YOU JUST ADDED EVERYWHERE
-- TODO START MAKING GIT BRANCHES FOR EACH LITTLE IDEA TO KEEP SANE
-- TODO THEN TRY MIMICKING HASKELL'S "SET A" NOTATION: "SETOF A"
--      YOUR TYPECHECKER CAN JUST HAVE A SPECIAL CASE FOR THOSE!
--      CHECK THAT ALL THE "A"S AND "SETOF A"S MATCH,
--      THEN CHECK THE REST NORMALLY
--      DON'T GO OVERBOARD FOR NOW; JUST AN "A" TYPE IS ENOUGH TO TRY IT
--      CAN ALSO MAKE A SPECIAL CASE WHEN PRINTING THE TYPE SIGNATURES + ERRORS
--      but how to avoid putting those placeholder "a"s in at runtime?
-- TODO another idea, which could go in a branch:
--      have the args list take an argument, and also contain the return type
--      (not just a list, but whatever you get it)
--      that way you can adjust based on the first arg
--      but would that make handling sets hard as opposed to regular types?
--      would it mean having pattern matches that can fail?

-- TODO does eq make sense here? should i just be comparing names??
-- TODO pretty instance like "union: [set, set] -> set"? just "union" for now
data CutFunction = CutFunction
  { fName    :: String
  , fAccepts :: [CutType]
  , fReturns :: CutType
  , fFixity  :: CutFixity
  , fCompiler :: CutConfig -> CutExpr -> CutType -> Rules FilePath
  }
  -- deriving (Eq, Read)

-- TODO change this to something useful
instance Pretty CutFunction where
  pPrint fn = text $ "CutFunction '" ++ fName fn ++ "'"

instance Show CutFunction where
  show = prettyShow

-- TODO does eq make sense here?
data CutModule = CutModule
  { mName :: String
  , mFunctions :: [CutType -> CutFunction]
  }
  -- deriving (Eq, Read)

-- TODO change this to something useful
instance Pretty CutModule where
  pPrint fn = text $ "CutModule '" ++ mName fn ++ "'"

instance Show CutModule where
  show = prettyShow
