{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShortCut.Types
  ( CutType(..), typeOf
  , CutVar(..), CutExpr(..), CutAssign, CutScript
  , CutConfig(..), CutState, Parser, ParserT, runParser, runParserT
  , getScript, getConfig, putScript, putConfig
  , str, num, faa, fna, gen, gom, csv
  , prettyShow
  , Repl, prompt, print, runRepl
  -- , Repl(..), runRepl, -- prompt, print
  )
  where

import Prelude hiding (print)
import qualified Text.Parsec as P
import Text.PrettyPrint.HughesPJClass

import Control.Monad.Identity (Identity)
import Data.Scientific        (Scientific())
import Text.Parsec            (ParseError)
import System.Console.Haskeline (InputT, getInputLine, runInputT, defaultSettings,
                                 outputStrLn)
import Control.Monad.Trans            (lift)

--------------------
-- error messages --
--------------------

-- TODO remove and use Parsec's error messages instead?
-- data CutError
--   = InvalidSyntax  ParseError
--   | NoSuchFunction String
--   | NoSuchVariable String
--   | WrongArgTypes  String [String] [String]
--   | WrongArgNumber String Int Int
--   deriving Eq
-- 
-- instance Show CutError where
--   show (InvalidSyntax  err)  = "Invalid syntax for ShortCut code " ++ show err
--   show (NoSuchFunction name) = "No such function: " ++ name
--   show (NoSuchVariable name) = "No such variable: " ++ name
--   show (WrongArgNumber name n1 n2) = unlines
--     [ "Wrong number of arguments for " ++ name ++ ": "
--     , "need " ++ show n1 ++ " but got " ++ show n2 ++ "."
--     ]
--   show (WrongArgTypes name es as) = unlines
--     [ "Wrong argument types for the function '" ++ name ++ "'."
--     , "  Need: " ++ (concat $ intersperse ", " es)
--     , "  Got:  " ++ (concat $ intersperse ", " as)
--     ]

-----------------------
-- initial AST types --
-----------------------

-- data ParsedExpr
--   = Bop Char ParsedExpr ParsedExpr
--   | Cmd String [ParsedExpr]
--   | Num Scientific
--   | Ref Var
--   | Fil String
--   deriving (Eq, Show, Read)
-- 
-- type ParsedAssign = (Var, ParsedExpr)
-- type ParsedScript = [ParsedAssign]

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
str, num, faa, fna, gen, gom, csv :: CutType
str = CutType "str"    "string"
num = CutType "num"    "number in scientific notation"
faa = CutType "faa"    "fasta amino acid"
fna = CutType "fna"    "fasta nucleic acid"
gen = CutType "gene"   "gene" -- TODO deprecate
gom = CutType "genome" "genome" -- TODO deprecate
csv = CutType "csv"    "spreadsheet"

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

instance Pretty CutScript where
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
data CutConfig = CutConfig
  { cfgScript  :: Maybe String
  , cfgWorkDir :: String
  , cfgTmpDir  :: String
  , cfgVerbose :: Bool
  }
  deriving (Eq, Show, Read)

instance Pretty CutConfig where
  pPrint cfg = vcat $ map (\(k,fn) -> text k <+> text "=" <+> text (fn cfg))
    [ ("script" , show . cfgScript )
    , ("workdir", show . cfgWorkDir)
    , ("tmpdir" , show . cfgTmpDir )
    , ("verbose", show . cfgVerbose)
    ]

---------------
-- Cut monad --
---------------

type CutState  = (CutScript, CutConfig)
type ParserT m = P.ParsecT String CutState m
type Parser    = ParserT Identity

runParser :: Parser a -> CutState -> String -> Either ParseError a
runParser p s = P.runParser p s "somefile"

runParserT :: Monad m => ParserT m a -> CutState -> String -> m (Either ParseError a)
runParserT psr st = P.runParserT psr st "somefile"

getScript :: Monad m => ParserT m CutScript
getScript = fmap fst P.getState

getConfig :: Monad m => ParserT m CutConfig
getConfig = fmap snd P.getState

putScript :: Monad m => CutScript -> ParserT m ()
putScript scr = P.getState >>= \(_, c) -> P.putState (scr, c)

putConfig :: Monad m => CutConfig -> ParserT m ()
putConfig cfg = P.getState >>= \(s, _) -> P.putState (s, cfg)

-- TODO modState, modConfig (maybe instead of the put ones?)

----------------
-- Repl monad --
----------------

-- type Repl a = ParserT (InputT IO) a
type Repl a = P.ParsecT String CutState (InputT IO) a

runRepl = undefined

prompt :: String -> Repl (Maybe String)
prompt = lift . getInputLine

print :: String -> Repl ()
print = lift . outputStrLn
