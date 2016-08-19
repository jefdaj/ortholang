{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShortCut.Types
  ( CutError(..), VarName(..), CutType(..), typeOf
  , ParsedExpr(..), ParsedVar, ParsedAssign, ParsedScript
  , TypedExpr(..), TypedAssign, CutScript
  , CutConfig(..), CutState, CutM, CutT(..), runCutM, runCutT
  , getScript, getConfig, putScript, putConfig
  -- shortcut types (haskell values)
  , str, num, faa, fna, gen, gom, csv
  )
  where

import Text.PrettyPrint.HughesPJClass

import Control.Monad.Except   (MonadError, ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity)
import Control.Monad.State    (MonadState, StateT, runState, runStateT, get, put)
import Control.Monad.Trans    (MonadTrans, lift)
import Data.List              (intersperse)
import Data.Scientific        (Scientific())
import Text.Parsec            (ParseError)

--------------------
-- error messages --
--------------------

-- TODO remove and use Parsec's error messages instead?
data CutError
  = InvalidSyntax  ParseError
  | NoSuchFunction String
  | NoSuchVariable String
  | WrongArgTypes  String [String] [String]
  | WrongArgNumber String Int Int
  deriving Eq

instance Show CutError where
  show (InvalidSyntax  err)  = "Invalid syntax for ShortCut code " ++ show err
  show (NoSuchFunction name) = "No such function: " ++ name
  show (NoSuchVariable name) = "No such variable: " ++ name
  show (WrongArgNumber name n1 n2) = unlines
    [ "Wrong number of arguments for " ++ name ++ ": "
    , "need " ++ show n1 ++ " but got " ++ show n2 ++ "."
    ]
  show (WrongArgTypes name es as) = unlines
    [ "Wrong argument types for the function '" ++ name ++ "'."
    , "  Need: " ++ (concat $ intersperse ", " es)
    , "  Got:  " ++ (concat $ intersperse ", " as)
    ]

-----------------------
-- initial AST types --
-----------------------

data ParsedExpr
  = Bop Char ParsedExpr ParsedExpr
  | Cmd String [ParsedExpr]
  | Num Scientific
  | Ref ParsedVar
  | Fil String
  deriving (Eq, Show, Read)

newtype VarName = VarName String deriving (Eq, Show, Read)

type ParsedVar    = VarName
type ParsedAssign = (ParsedVar, ParsedExpr)
type ParsedScript = [ParsedAssign]

---------------------
-- typed AST types --
---------------------

-- TODO unify these with the above Parsed ones
-- TODO rethink paths and literals:
--        literals are strings or numbers
--        no need to represent paths separately at all?
--        no need for an extension tag in either

-- Filename extension, which in ShortCut is equivalent to variable type
-- TODO can this be done better with phantom types?
-- data Ext = SetOf Ext | Ext String
  -- deriving (Eq, Show, Read)

data TypedExpr
  = TStr String
  | TNum Scientific
  | TRef CutType ParsedVar
  | TSet CutType [TypedExpr]
  | TBop CutType String  TypedExpr TypedExpr
  | TCmd CutType String [TypedExpr]
  deriving (Eq, Show, Read)

type TypedAssign = (ParsedVar, TypedExpr)
type CutScript = [TypedAssign]

data CutType
  = CutType String String
  | SetOf CutType
  deriving (Eq, Show, Read)

instance Pretty CutType where
  pPrint (CutType ext desc) = text ext <+> parens (text desc)
  pPrint (SetOf t) = text "set of" <+> pPrint t

typeOf :: TypedExpr -> CutType
typeOf (TStr _) = str
typeOf (TNum _) = num
typeOf (TRef t _) = t
typeOf (TSet t _) = SetOf t
typeOf (TBop t _ _ _) = t
typeOf (TCmd t _ _) = t

str, num, faa, fna, gen, gom, csv :: CutType
str = CutType "str"    "string"
num = CutType "num"    "number in scientific notation"

-- TODO separate faa, fna and remove gen, gom
faa = CutType "faa"    "fasta amino acid"
fna = CutType "fna"    "fasta nucleic acid"
gen = CutType "gene"   "gene" -- TODO deprecate
gom = CutType "genome" "genome" -- TODO deprecate
csv = CutType "csv"    "spreadsheet"

-- coreTypes :: [CutType]
-- coreTypes = [str, num, faa, fna, gen, gom, csv]

---------------------
-- pretty printers --
---------------------

-- I would put these in a separate Pretty.hs, but that causes orphan instances

instance Pretty VarName where
  pPrint (VarName s) = text s

-- TODO add descriptions here? if so, need to separate actual extension code
-- instance Pretty Ext where
--   pPrint (SetOf e) = pPrint e <> text "s"
--   pPrint (Ext   e) = text e

instance {-# OVERLAPPING #-} Pretty TypedAssign where
  pPrint (v, e) = pPrint v <+> text "=" <+> pPrint e
  -- this adds type info, but makes the pretty-print not valid source code
  -- pPrint (v, e) = text (render (pPrint v) ++ "." ++ render (pPrint $ typeExt e))

instance Pretty CutScript where
  pPrint [] = empty
  pPrint as = fsep $ map pPrint as

instance Pretty TypedExpr where
  pPrint (TNum n)         = text $ show n
  pPrint (TSet _ _)       = undefined -- TODO figure this out!
  pPrint (TStr s)         = text $ show s
  pPrint (TRef _ v)       = pPrint v
  pPrint (TCmd _ s es)    = text s <+> sep (map pNested es)
  pPrint (TBop _ c e1 e2) = if (length $ render $ one) > 80 then two else one
    where
      bopWith fn = fn (pPrint e1) (nest (-2) (text c) <+> pPrint e2)
      one = bopWith (<+>)
      two = bopWith ($+$)

-- this adds parens around nested function calls
-- without it things can get really messy!
pNested :: TypedExpr -> Doc
pNested e@(TCmd _ _ _  ) = parens $ pPrint e
pNested e@(TBop _ _ _ _) = parens $ pPrint e
pNested e = pPrint e

------------
-- config --
------------

-- TODO always load defaults for WorkDir, TmpDir, Verbose
-- TODO make these into FilePaths and an Int/Bool
data CutConfig = CutConfig
  { cfgScript  :: Maybe String
  , cfgWorkDir :: Maybe String
  , cfgTmpDir  :: Maybe String
  , cfgVerbose :: Maybe String
  }

---------------
-- Cut monad --
---------------

type CutState = (CutScript, CutConfig)

getScript :: MonadState CutState m => m CutScript
getScript = fmap fst get

getConfig :: MonadState CutState m => m CutConfig
getConfig = fmap snd get

putScript :: MonadState CutState m => CutScript -> m ()
putScript scr = get >>= \(_, c) -> put (scr, c)

putConfig :: MonadState CutState m => CutConfig -> m ()
putConfig cfg = get >>= \(s, _) -> put (s, cfg)

type CutM a = CutT Identity a

newtype CutT m a = CutT { unCutT :: ExceptT CutError (StateT CutState m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState CutState
    , MonadError CutError
    )

instance MonadTrans CutT where
  lift = CutT . lift . lift

runCutM :: CutM a -> CutState -> (Either CutError a, CutState)
runCutM = runState . runExceptT . unCutT

runCutT :: CutT m a -> CutState -> m (Either CutError a, CutState)
runCutT = runStateT . runExceptT . unCutT
