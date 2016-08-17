{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

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
import Data.Scientific                (Scientific)
import Development.Shake.FilePath     ((<.>), (</>))
import Text.Parsec                    (ParseError)
import Text.PrettyPrint.HughesPJ      ((<+>), vcat, text, sep, empty)
import Text.PrettyPrint.HughesPJClass (Pretty, pPrint)

--------------------
-- error messages --
--------------------

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
type ParsedAssign = (ParsedVar, ParsedExpr) -- TODO could add type tags here
type ParsedScript = [ParsedAssign]

-- TODO add tests for round-tripping everything based on these

instance Pretty VarName where
  pPrint (VarName s) = text s

instance Pretty ParsedExpr where
  pPrint (Num n)       = text $ show n
  pPrint (Fil s)       = text $ show s
  pPrint (Ref v)       = pPrint v
  pPrint (Cmd s es)    = text s <+> sep (map pPrint es)
  pPrint (Bop c e1 e2) = pPrint e1 <+> text [c] <+> pPrint e2

instance {-# OVERLAPPING #-} Pretty ParsedAssign where
  pPrint (v, e) = pPrint v <+> text "=" <+> pPrint e

instance Pretty ParsedScript where
  pPrint [] = empty
  pPrint as = vcat $ map pPrint as

----------------------------------
-- new non-GADT typed AST types --
----------------------------------

-- TODO unify these with the above Parsed ones
-- TODO rethink paths and literals:
--        literals are strings or numbers
--        no need to represent paths separately at all?
--        no need for an extension tag in either

-- Filename extension, which in ShortCut is equivalent to variable type
-- TODO can this be done better with phantom types?
data Ext = SetOf Ext | Ext String
  deriving (Eq, Show, Read)

data TypedExpr2
  = Str2 String
  | Num2 Scientific
  | Ref2 Ext ParsedVar
  | Set2 Ext [TypedExpr2]
  | Bop2 Ext String  TypedExpr2 TypedExpr2
  | Cmd2 Ext String [TypedExpr2]
  deriving (Eq, Show, Read)

type TypedAssign2 = (ParsedVar, TypedExpr2)
type TypedScript2 = [TypedAssign2]

getExt :: TypedExpr2 -> Ext
getExt (Str2   _    ) = Ext "str"
getExt (Num2   _    ) = Ext "num"
getExt (Ref2 e _    ) = e
getExt (Cmd2 e _ _  ) = e
getExt (Bop2 e _ _ _) = e
getExt (Set2 e _    ) = e

-- TODO remove
ext2 :: Returns a -> Ext
ext2 = Ext . ext

-----------------------
-- Typed AST Types --
-----------------------

newtype FastaNA = FastaNA FilePath deriving (Eq, Ord, Show)
newtype FastaAA = FastaAA FilePath deriving (Eq, Ord, Show)
newtype Genome  = Genome  FilePath deriving (Eq, Ord, Show)

-- This may change, but the idea is that gene names always reference their genome.
-- That way we don't have to copy the associated sequence around, just the name.
data Gene = Gene Genome String deriving (Eq, Ord, Show)

-- type aliases to shorten the Typed constructor signatures
-- these are just shorthand, and shouldn't need to be exported
-- type Gen    = Typed Gene
-- type Gom    = Typed Genome -- TODO unify with Fna?
-- type RSet a = Returns [a] -- TODO remove?
type Faa     = Typed FastaAA
type Fna     = Typed FastaNA
type Gens    = Typed [Gene]
type Goms    = Typed [Genome]
type Sci     = Typed Scientific
type Fil     = Typed FilePath
type List  a = Typed [a] -- TODO use actual sets here? rename?

-- TODO try making these tuples instead! would that remove the need to list fns here?
data Typed a where
  -- refs
  Reference :: Returns a -> String -> Typed a
  -- literals
  File   :: String     -> Fil
  Number :: Scientific -> Sci
  -- load functions
  LoadFNA     :: Fil -> Fna
  LoadFAA     :: Fil -> Faa
  LoadGenes   :: Fil -> Gens
  LoadGenomes :: Fil -> Goms
  -- math
  Add      :: (Sci, Sci) -> Sci
  Subtract :: (Sci, Sci) -> Sci
  Multiply :: (Sci, Sci) -> Sci
  Divide   :: (Sci, Sci) -> Sci
  -- sets
  Union      :: Ord a => (List a, List a) -> List a
  Intersect  :: Ord a => (List a, List a) -> List a
  Difference :: Ord a => (List a, List a) -> List a
  -- scripts
  FilterGenes   :: (Gens, Goms, Sci) -> Gens
  FilterGenomes :: (Goms, Gens, Sci) -> Goms
  WorstBest     :: (Gens, Goms) -> Sci

deriving instance Eq   (Typed a)
deriving instance Show (Typed a)

instance Pretty (Typed a) where
  pPrint (File           s) = text $ "\"" ++ s ++ "\""
  pPrint (Number         n) = text $ show n
  pPrint (Reference    _ s) = text s
  pPrint (LoadFNA        s) = text "load_fna" <+> pPrint s
  pPrint (LoadFAA        s) = text "load_faa" <+> pPrint s
  pPrint (LoadGenes      s) = text "load_genes" <+> pPrint s
  pPrint (LoadGenomes    s) = text "load_genomes" <+> pPrint s
  pPrint (Add        (n1,n2)) = pPrint n1 <+> text "+" <+> pPrint n2
  pPrint (Subtract   (n1,n2)) = pPrint n1 <+> text "-" <+> pPrint n2
  pPrint (Multiply   (n1,n2)) = pPrint n1 <+> text "*" <+> pPrint n2
  pPrint (Divide     (n1,n2)) = pPrint n1 <+> text "/" <+> pPrint n2
  pPrint (Union      (s1,s2)) = pPrint s1 <+> text "+" <+> pPrint s2
  pPrint (Intersect  (s1,s2)) = pPrint s1 <+> text "&" <+> pPrint s2
  pPrint (Difference (s1,s2)) = pPrint s1 <+> text "-" <+> pPrint s2
  pPrint (FilterGenes (a1,a2,a3)) =
    text "filter_genes" <+> pPrint a1 <+> pPrint a2 <+> pPrint a3
  pPrint (FilterGenomes (a1,a2,a3)) =
    text "filter_genomes" <+> pPrint a1 <+> pPrint a2 <+> pPrint a3
  pPrint (WorstBest (a1,a2)) = text "worst_best_evalue" <+> pPrint a1 <+> pPrint a2

-- This is for 'tagging' Typed expressions with their return types,
-- which simplifies typechecking larger expressions later.
data Returns a where
  RNumber  :: Returns Scientific
  RFile    :: Returns String
  RFastaNA :: Returns FastaNA
  RFastaAA :: Returns FastaAA
  RGene    :: Returns Gene
  RGenome  :: Returns Genome
  RGenes   :: Returns [Gene]
  RGenomes :: Returns [Genome]

deriving instance Eq   (Returns a)
deriving instance Ord  (Returns a)
deriving instance Show (Returns a)

instance Pretty (Returns a) where
  pPrint RNumber  = text "number"
  pPrint RFile    = text "string"
  pPrint RFastaNA = text "fasta (nucleic acid)"
  pPrint RFastaAA = text "fasta (amino acid)"
  pPrint RGene    = text "gene"
  pPrint RGenes   = text "genes"
  pPrint RGenome  = text "genome"
  pPrint RGenomes = text "genomes"

-- TODO use .txt for all the lists? or .list?
ext :: Returns a -> String
ext RNumber  = "num"
ext RFile    = "path"
ext RFastaNA = "fna"
ext RFastaAA = "faa"
ext RGene    = "gene"
ext RGenes   = "genes"
ext RGenome  = "genome"
ext RGenomes = "genomes"

-- This is a wrapper that lets you deal with a `Typed a` value without
-- caring about the type of `a`, or find out that type by pattern matching
-- (match the `Returns a` and GHC infers you have a matching `Typed a`).
data TypedExpr where
  TypedExpr :: Returns a -> Typed a -> TypedExpr

deriving instance Show TypedExpr

instance Pretty TypedExpr where
  pPrint (TypedExpr r e) = pPrint e <+> text "->" <+> pPrint r

-- TODO should TypedVar be a Typed too? Or separate?
-- TODO should it contain a VarName, or be its own thing?
newtype TypedVar = TypedVar String deriving (Eq, Show, Read)

type TypedAssign = (TypedVar, TypedExpr)
type TypedScript = [TypedAssign]

instance Pretty TypedVar where
  pPrint (TypedVar s) = text s

instance {-# OVERLAPPING #-} Pretty TypedAssign where
  pPrint (v, e) = pPrint v <+> text "=" <+> pPrint e

-- TODO move back to Compile.hs or somewhere else? It's not a type!
-- TODO flip arguments for consistency with everything else
-- There's a kludge here for the special case of "result", which is like the
-- "main" function of a ShortCut script, and always goes to <tmpdir>/result.
namedTmp :: ParsedVar -> TypedExpr2 -> FilePath
namedTmp (VarName var) expr = tmpDir </> base
  where
    base  = if var == "result" then var else var <.> e
    Ext e = getExt expr

-- TODO deduplicate with the one in Compile.hs
--      (actually, load from config)
tmpDir :: FilePath
tmpDir = "_shortcut"

--------------------
-- main Cut monad --
--------------------

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
