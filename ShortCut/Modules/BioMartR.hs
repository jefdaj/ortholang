module ShortCut.Modules.BioMartR where

-- TODO put nix and R stuff in here too (ask how best on StackOverflow/Nix ML)
-- TODO parse strings into "search tables" (name + optional db + optional id)
-- TODO separate cache dir for this stuff, and "load"/link results to it
-- TODO "show" the results using that convenient .txt file biomartr saves
-- TODO combine databases if none is specified
-- TODO once this works, document it in a notebook entry!
-- TODO known bug: hashes are getting mixed up in cParseSearches

import ShortCut.Core.Types
import ShortCut.Modules.Blast (gom) -- TODO fix that/deprecate
import Development.Shake
import ShortCut.Core.Parse (defaultTypeCheck)
import ShortCut.Core.Compile (cExpr, hashedTmp, hashedTmp', cList)
import Control.Monad (void)
import Text.Parsec            (spaces, runParser)
import Text.Parsec (Parsec, try, choice, (<|>), many1)
import Text.Parsec.Char (char, string, alphaNum, oneOf)
import Text.Parsec.Combinator (between, optionMaybe)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Either (partitionEithers)
import Data.Char (isSpace)
import Development.Shake.FilePath ((</>))

------------------------
-- module description --
------------------------

cutModule :: CutModule
cutModule = CutModule
  { mName = "biomartr"
  , mFunctions =
    [ parseSearch -- TODO hide from end users?
    , getGenomes
    -- , getGenome
    ]
  }

search :: CutType
search = CutType
  { tExt  = "search" -- TODO should these be recognizable (tsv)?
  , tDesc = "intermediate table describing biomartr searches"
  , tCat  = undefined
  }

parseSearch :: CutFunction
parseSearch = CutFunction
  { fName      = "parse_search"
  , fTypeCheck = defaultTypeCheck [str] search
  , fFixity    = Prefix
  , fCompiler  = cParseSearches
  }

-- getGenome :: CutFunction
-- getGenome = CutFunction
--   { fName      = "get_genome"
--   , fTypeCheck = defaultTypeCheck [str] gom
--   , fFixity    = Prefix
--   , fCompiler  = cGetGenome
--   }

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

-- TODO bug: this is getting passed a filepath and is partially parsing it
--           it expects a single filepath but gets a list of them
--           then when it reads the list it gets more filenames. doh!
readSearch :: FilePath -> IO (Either String Search)
readSearch p = do
  txt <- readFile p
  return $ case runParser pSearch () "search string" txt of
    Left  e -> Left  $ show e
    Right s@(Search sp d i) -> Right s

-- TODO use cassava?
toTsv :: [Search] -> String
toTsv ss = unlines $ map (intercalate "\t") (header:map row ss)
  where
    header             = ["organism", "database", "identifier"]
    row (Search s d i) = [s, fromMaybe "NA" d, fromMaybe "NA" i]

cParseSearches :: CutConfig -> CutExpr -> Rules FilePath
cParseSearches cfg expr@(CutList _ _ _) = do
  sList <- cExpr cfg expr
  let searchTable = hashedTmp' cfg search expr [sList]
  searchTable %> \out -> do
    tmp <- readFile' sList
    let sLines = map (cfgTmpDir cfg </>) (lines tmp)
    need sLines
    parses <- liftIO $ mapM readSearch sLines
    let (errors, searches') = partitionEithers parses
    -- TODO better error here
    if (not . null) errors
      then error "invalid search!"
      else liftIO $ writeFile out $ toTsv searches'
  return searchTable
cParseSearches _ _ = error "bad arguments to cParseSearches"

------------------
-- run biomartr --
------------------

-- TODO this is where to parse the searches?
-- cGetGenome :: CutConfig -> CutExpr -> Rules FilePath
-- cGetGenome cfg expr@(CutFun _ _ _ [s]) = undefined
-- cGetGenome _ _ = error "bad cGetGenome call"

-- TODO factor out a "trivial string file" function?
cGetGenomes :: CutConfig -> CutExpr -> Rules FilePath
cGetGenomes cfg expr@(CutFun _ _ _ [ss]) = do
  bmFn   <- cExpr cfg (CutLit str "getGenome")
  sTable <- cParseSearches cfg ss
  -- TODO separate tmpDirs for genomes, proteomes, etc?
  let bmTmp = cfgTmpDir cfg </> "cache" </> "biomartr"
      goms  = hashedTmp cfg expr [bmFn, sTable]
  goms %> \out -> do
    need [bmFn, sTable]
    -- TODO should biomartr get multiple output paths?
    quietly $ cmd "biomartr.R" [bmTmp, out, bmFn, sTable]
  return goms
cGetGenomes _ _ = error "bad cGetGenomes call"
