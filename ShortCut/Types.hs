{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShortCut.Types where

import Control.Monad.Except           (MonadError, ExceptT, runExceptT)
import Control.Monad.IO.Class         (MonadIO)
import Control.Monad.Identity         (Identity)
import Control.Monad.RWS.Lazy         (RWST, runRWS, runRWST)
import Control.Monad.Reader           (MonadReader)
import Control.Monad.State            (MonadState)
import Control.Monad.Trans            (MonadTrans, lift)
import Control.Monad.Writer           (MonadWriter)
import Data.List                      (intersperse)
import Data.Scientific                (Scientific())
import Development.Shake.FilePath     ((<.>), (</>))
import Text.Parsec                    (ParseError)
import Text.PrettyPrint.HughesPJ      ((<+>), vcat, text, sep, empty)
import Text.PrettyPrint.HughesPJClass (Pretty, pPrint)

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
data Ext = SetOf Ext | Ext String
  deriving (Eq, Show, Read)

data TypedExpr
  = TStr String
  | TNum Scientific
  | TRef Ext ParsedVar
  | TSet Ext [TypedExpr]
  | TBop Ext String  TypedExpr TypedExpr
  | TCmd Ext String [TypedExpr]
  deriving (Eq, Show, Read)

type TypedAssign = (ParsedVar, TypedExpr)
type TypedScript = [TypedAssign]

getExt :: TypedExpr -> Ext
getExt (TStr   _    ) = Ext "str"
getExt (TNum   _    ) = Ext "num"
getExt (TRef e _    ) = e
getExt (TCmd e _ _  ) = e
getExt (TBop e _ _ _) = e
getExt (TSet e _    ) = e

---------------------
-- pretty printers --
---------------------

instance Pretty VarName where
  pPrint (VarName s) = text s

instance Pretty Ext where
  pPrint (Ext e) = text e

instance {-# OVERLAPPING #-} Pretty TypedAssign where
  pPrint (v, e) = pPrint v <+> text "=" <+> pPrint e

instance Pretty TypedScript where
  pPrint [] = empty
  pPrint as = vcat $ map pPrint as

instance Pretty TypedExpr where
  pPrint (TNum n)         = text $ show n
  pPrint (TSet _ _)       = undefined -- TODO figure this out!
  pPrint (TStr s)         = text $ show s
  pPrint (TRef _ v)       = pPrint v
  pPrint (TCmd _ s es)    = text s <+> sep (map pPrint es)
  pPrint (TBop _ c e1 e2) = pPrint e1 <+> text c <+> pPrint e2

---------------
-- Cut monad --
---------------

type CutConfig = [(String, String)]
type CutLog    = [String]
type CutState  = TypedScript

type CutM a = CutT Identity a

newtype CutT m a = CutT
  { unCutT :: ExceptT CutError (RWST CutConfig CutLog CutState m) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError  CutError
    , MonadReader CutConfig
    , MonadWriter CutLog
    , MonadState  CutState
    )

instance MonadTrans CutT where
  lift = CutT . lift . lift

runCutM :: CutM a -> CutConfig -> CutState
        -> (Either CutError a, CutState, CutLog)
runCutM = runRWS . runExceptT . unCutT

runCutT :: CutT m a -> CutConfig -> CutState
        -> m (Either CutError a, CutState, CutLog)
runCutT = runRWST . runExceptT . unCutT
