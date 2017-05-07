module ShortCut.Modules.BioMartR where

-- TODO put nix and R stuff in here too (ask how best on StackOverflow/Nix ML)
-- TODO parse strings into "search tables" (name + optional db + optional id)
-- TODO separate cache dir for this stuff, and "load"/link results to it
-- TODO "show" the results using that convenient .txt file biomartr saves
-- TODO combine databases if none is specified
-- TODO once this works, document it in a notebook entry!

import ShortCut.Core.Types
import ShortCut.Modules.Blast (gom) -- TODO fix that/deprecate
import Development.Shake
import ShortCut.Core.Parse (defaultTypeCheck)
import ShortCut.Core.Compile (cExpr, hashedTmp)
import Control.Monad (void, when)
import Text.Parsec            (spaces, runParser)
import Text.Parsec (Parsec, try, choice, (<|>), many1)
import Text.Parsec.Char (char, string, alphaNum, oneOf)
import Text.Parsec.Combinator (between, optionMaybe)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Either (partitionEithers)
import Data.Char (isSpace)

------------------------
-- module description --
------------------------

cutModule :: CutModule
cutModule = CutModule
  { mName = "biomartr"
  , mFunctions =
    [ parseSearch -- TODO hide from end users?
    -- , getGenome
    -- , getGenomes
    ]
  }

search :: CutType
search = CutType
  { tExt  = "tsv" -- TODO should these be unique?
  , tDesc = "intermediate table describing biomartr searches"
  , tCat  = id
  }

parseSearch :: CutFunction
parseSearch = CutFunction
  { fName      = "parse_search"
  , fTypeCheck = defaultTypeCheck [str] search
  , fFixity    = Prefix
  , fCompiler  = cParseSearches
  }

getGenome :: CutFunction
getGenome = CutFunction
  { fName      = "get_genome"
  , fTypeCheck = defaultTypeCheck [str] gom
  , fFixity    = Prefix
  , fCompiler  = cGetGenome
  }

getGenomes :: CutFunction
getGenomes = CutFunction
  { fName      = "get_genomes"
  , fTypeCheck = defaultTypeCheck [(ListOf str)] (ListOf gom)
  , fFixity    = Prefix
  , fCompiler  = cGetGenomes
  }

--------------------------
-- parse search strings --
--------------------------

type Parser a = Parsec String () a

type Species    = String
type Database   = String
type Identifier = String

data Search = Search Species (Maybe Database) (Maybe Identifier)

-- from http://stackoverflow.com/a/6270337
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

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
    inParens = between (pSym '(') (pSym ')')
    pSym     = void . char 

readSearch :: FilePath -> IO (Either String Search)
readSearch p = do
  txt <- readFile p
  return $ case runParser pSearch () "search string" txt of
    Left  e -> Left  $ show e
    Right s -> Right $ s

-- TODO use cassava?
toTsv :: [Search] -> String
toTsv ss = unlines $ map (intercalate "\t") (header:map row ss)
  where
    header             = ["species", "db", "id"]
    row (Search s d i) = [s, fromMaybe "NA" d, fromMaybe "NA" i]

cParseSearches :: CutConfig -> CutExpr -> Rules FilePath
cParseSearches cfg expr@(CutFun _ _ ss) = do
  sFiles <- mapM (cExpr cfg) ss
  let sTable = hashedTmp cfg expr sFiles
  sTable %> \out -> do
    need sFiles
    parses <- liftIO $ mapM readSearch sFiles
    let (errors, searches) = partitionEithers parses
        tsv = toTsv searches
    -- TODO better error here
    when (not $ null errors) (error "invalid search!")
    liftIO $ writeFile out tsv
  return sTable
cParseSearches _ _ = error "bad arguments to cParseSearches"

-----------------
-- get genomes --
-----------------

-- TODO this is where to parse the searches?
cGetGenome :: CutConfig -> CutExpr -> Rules FilePath
cGetGenome cfg expr@(CutFun _ _ [s]) = undefined
cGetGenome _ _ = error "bad cGetGenome call"

-- TODO this is where to parse the searches?
cGetGenomes :: CutConfig -> CutExpr -> Rules FilePath
cGetGenomes cfg expr@(CutFun _ _ ss) = undefined
cGetGenomes _ _ = error "bad cGetGenomes call"
