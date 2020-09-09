module OrthoLang.Modules.BioMartR where

-- TODO separate functions per database + type? get_refseq_proteome etc
-- TODO get all the load_* fns to handle .gz files too

-- TODO rename to start with biomartr_?
-- TODO show progress downloading stuff first
-- TODO can you just add refseq on the end of the query itself? wtf
-- TODO fix "invalid path: 'Not available'" (pass error on to user)

-- TODO put nix and R stuff in here too (ask how best on StackOverflow/Nix ML)
-- TODO parse strings into "search tables" (name + optional db + optional id)
-- TODO separate cache dir for this stuff, and "load"/link results to it
-- TODO "show" the results using that convenient .txt file biomartr saves
-- TODO combine databases if none is specified
-- TODO once this works, document it in a notebook entry!
-- TODO known bug: hashes are getting mixed up in rParseSearches

-- TODO what's the overall plan?
--      1. parse searches regardless of function being used
--      2. feed search table to biomartr cmd along with fn name

-- import OrthoLang.Modules.Blast (gom) -- TODO fix that/deprecate
import OrthoLang.Types
import OrthoLang.Interpreter
import Development.Shake
import OrthoLang.Modules.SeqIO (fna, faa)
import Control.Monad (void)
import Text.Parsec            (spaces, runParser)
import Text.Parsec (Parsec, try, choice, (<|>), many1)
import Text.Parsec.Char (char, string, alphaNum, oneOf)
import Text.Parsec.Combinator (between, optionMaybe)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (intercalate)
import Data.Either (partitionEithers)
import System.Directory (createDirectoryIfMissing)
import OrthoLang.Util (trim)

------------
-- module --
------------

olModule :: Module
olModule = Module
  { mName = "BiomartR"
  , mDesc = "Search + download genomes and proteomes from Biomart"
  , mTypes = [search, fna, faa, fnagz, faagz]
  , mGroups = []
  , mEncodings = [gz]
  , mRules = []
  , mFunctions =
    [ parseSearches -- TODO hide from end users?
    -- TODO single and _each versions?
    , getGenomesExplicit, getGenomes
    , getProteomesExplicit, getProteomes
    ]
  }

search :: Type
search = Type
  { tExt  = "search" -- TODO should these be recognizable (tsv)?
  , tDesc = "intermediate table describing biomartr searches"
  -- , tShow = \ls f -> readFileStrict ls f
  , tShow = defaultShow
  }

gz :: Encoding
gz = Encoding
  { enExt = "gz"
  , enDesc = "gunzip archive" -- TODO and maybe also tarballed?
  , enShow = \cfg _ path -> return $ "gunzip archive '" ++ toGeneric cfg path ++ "'"
  }

fnagz :: Type
fnagz = EncodedAs gz fna

faagz :: Type
faagz = EncodedAs gz faa

-----------------
-- get_genomes --
-----------------

-- TODO add a temporary outpath and clean up after it?
aBiomartrExplicit :: String -> NewAction1
aBiomartrExplicit bmFn (ExprPath out) sTable = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.biomartr.aBiomartrExplicit"
      tmpDir = fromPath loc cfg $ cacheDir cfg "biomartr"
  liftIO $ createDirectoryIfMissing True tmpDir
  aNewRulesS1 "biomartr.R" (\d -> d {cmdArguments = [bmFn, out, sTable]}) (ExprPath out) sTable

getGenomesExplicit :: Function
getGenomesExplicit = newFnA1
  "get_genomes_explicit"
  (Exactly $ ListOf str)
  (Exactly $ ListOf fnagz)
  (aBiomartrExplicit "getGenome")
  [ReadsURL]

getGenomes :: Function
getGenomes = newExprExpansion
  "get_genomes"
  [Exactly $ ListOf str]
  (Exactly $ ListOf fnagz)
  mGetGenomes
  [ReadsURL]

mGetGenomes :: ExprExpansion
mGetGenomes _ _ (Fun r _ ds _ [ss]) = Fun r Nothing ds "get_genomes_explicit" [fn]
  where
    fn = Fun search Nothing ds "parse_searches" [ss]
mGetGenomes _ _ e = error "modules.biomartr.mGetGenomes" $ "bad argument: " ++ show e

-------------------
-- get_proteomes --
-------------------

getProteomesExplicit :: Function
getProteomesExplicit = newFnA1
  "get_proteomes_explicit"
  (Exactly search)
  (Exactly $ ListOf fnagz)
  (aBiomartrExplicit "getProteome")
  [ReadsURL]

getProteomes :: Function
getProteomes = newExprExpansion
  "get_proteomes"
  [Exactly $ ListOf str]
  (Exactly $ ListOf fnagz)
  mGetProteomes
  [ReadsURL]

mGetProteomes :: ExprExpansion
mGetProteomes _ _ (Fun r _ ds _ [ss]) = Fun r Nothing ds "get_proteomes_explicit" [fn]
  where
    fn = Fun search Nothing ds "parse_searches" [ss]
mGetProteomes _ _ e = error "modules.biomartr.mGetProteomes" $ "bad argument: " ++ show e

--------------------
-- parse_searches --
--------------------

type Parser a = Parsec String () a

type Species    = String
type Database   = String
type Identifier = String

data Search = Search Species (Maybe Database) (Maybe Identifier)
  deriving (Eq, Show)

pSpecies :: Parser Species
pSpecies = fmap trim $ many1 $ alphaNum <|> oneOf ". "

-- TODO be stricter in only accepting valid IDs here?
-- TODO decide database from ID?
pIden :: Parser Identifier
pIden = many1 $ alphaNum <|> oneOf "._"

-- TODO convert to lowercase first?
pName :: Parser Database
pName = choice $ map (try . string)
  [ "genbank"
  , "refseq"
  , "ensembl"
  , "ensemblgenomes"
  ]

pFilters :: Parser (Maybe Database, Maybe Identifier)
pFilters = do
  -- TODO fail on empty parens?
  name <- optionMaybe $ try $ pName <* spaces -- TODO leading space OK?
  iden <- optionMaybe pIden -- TODO trailing space OK?
  return (name, iden)

pSearch :: Parser Search
pSearch = do
  species <- pSpecies
  filters <- optionMaybe $ inParens pFilters
  case filters of
    Nothing    -> return $ Search species Nothing Nothing
    Just (n,i) -> return $ Search species n i
  where
    inParens = between (pSym' '(') (pSym' ')')
    pSym'    = void . char 

-- TODO bug: this is getting passed a filepath and is partially parsing it
--           it expects a single filepath but gets a list of them
--           then when it reads the list it gets more filenames. doh!
readSearch :: String -> Either String Search
readSearch txt = case runParser pSearch () "search string" txt of
  Left  e -> Left  $ show e
  Right s@(Search _ _ _) -> Right s

-- TODO use cassava?
toTsvRows :: [Search] -> [String]
toTsvRows ss = map (intercalate "\t") (header:map row ss)
  where
    header             = ["organism", "database", "identifier"]
    row (Search s d i) = [s, fromMaybe "NA" d, fromMaybe "NA" i]

-- TODO rename to start with biomartr
parseSearches :: Function
parseSearches = newFnA1
  "parse_searches"
  (Exactly $ ListOf str)
  (Exactly search)
  aParseSearches
  [Hidden, ReadsURL]

aParseSearches :: NewAction1
aParseSearches (ExprPath out) sList = do
  let loc = "modules.biomartr.aParseSearches"
      out''  = traceA loc out [sList, out]
  parses <- (fmap . map) readSearch (readLits loc sList)
  let (errors, searches') = partitionEithers parses
  -- TODO better error here
  if (not . null) errors
    then error "invalid search!"
    else writeLits loc out'' $ toTsvRows searches'
