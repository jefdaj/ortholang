module ShortCut.Modules.BioMartR where

-- TODO put nix and R stuff in here too (ask how best on StackOverflow/Nix ML)
-- TODO parse strings into "search tables" (name + optional db + optional id)
-- TODO separate cache dir for this stuff, and "load"/link results to it
-- TODO "show" the results using that convenient .txt file biomartr saves
-- TODO combine databases if none is specified
-- TODO once this works, document it in a notebook entry!
-- TODO known bug: hashes are getting mixed up in cParseSearches

-- TODO what's the overall plan?
--      1. parse searches regardless of function being used
--      2. feed search table to biomartr script along with fn name

-- import ShortCut.Modules.Blast (gom) -- TODO fix that/deprecate
import ShortCut.Core.Types
import Development.Shake
import ShortCut.Core.ModuleAPI (defaultTypeCheck)
import ShortCut.Core.Paths   (hashedTmp, hashedTmp')
import ShortCut.Core.Compile (cExpr)
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
import Text.PrettyPrint.HughesPJ (text)

------------------------
-- module description --
------------------------

cutModule :: CutModule
cutModule = CutModule
  { mName = "biomartr"
  , mFunctions =
    [ parseSearch -- TODO hide from end users?
    , getGenomes
    , getProteomes
    -- , getGenome
    ]
  }

search :: CutType
search = CutType
  { tExt  = "search" -- TODO should these be recognizable (tsv)?
  , tDesc = "intermediate table describing biomartr searches"
  , tCat  = undefined
  }

-- TODO unify with fna? or replace it?
fnagz :: CutType
fnagz = CutType
  { tExt  = "fna.gz"
  , tDesc = "gzipped fasta nucleic acid acid (gene list or genome)"
  , tCat  = \_ -> return $ text "tCat not implemented yet for fnagz"
  }

-- TODO unify with faa? or replace it?
faagz :: CutType
faagz = CutType
  { tExt  = "faa.gz"
  , tDesc = "gzipped fasta amino acid (proteome)"
  , tCat  = \_ -> return $ text "tCat not implemented yet for faagz"
  }

parseSearch :: CutFunction
parseSearch = CutFunction
  { fName      = "parse_search"
  , fTypeCheck = defaultTypeCheck [str] search
  , fFixity    = Prefix
  , fCompiler  = cParseSearches
  }

getGenomes :: CutFunction
getGenomes = CutFunction
  { fName      = "get_genomes"
  , fTypeCheck = defaultTypeCheck [(ListOf str)] (ListOf fnagz)
  , fFixity    = Prefix
  , fCompiler  = cBioMartR "getGenome"
  }

getProteomes :: CutFunction
getProteomes = CutFunction
  { fName      = "get_proteomes"
  , fTypeCheck = defaultTypeCheck [(ListOf str)] (ListOf faagz)
  , fFixity    = Prefix
  , fCompiler  = cBioMartR "getProteome"
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
    Right s@(Search _ _ _) -> Right s

-- TODO use cassava?
toTsv :: [Search] -> String
toTsv ss = unlines $ map (intercalate "\t") (header:map row ss)
  where
    header             = ["organism", "database", "identifier"]
    row (Search s d i) = [s, fromMaybe "NA" d, fromMaybe "NA" i]

cParseSearches :: CutState -> CutExpr -> Rules FilePath
cParseSearches s@(_,cfg) expr@(CutList _ _ _ _) = do
  sList <- cExpr s expr
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
-- cGetGenome (_,cfg) expr@(CutFun _ _ _ [s]) = undefined
-- cGetGenome _ _ = error "bad cGetGenome call"

-- TODO factor out a "trivial string file" function?
cBioMartR :: String -> CutState -> CutExpr -> Rules FilePath
cBioMartR fn s@(_,cfg) expr@(CutFun _ _ _ _ [ss]) = do
  bmFn   <- cExpr s (CutLit str 0 fn)
  sTable <- cParseSearches s ss
  -- TODO separate tmpDirs for genomes, proteomes, etc?
  let bmTmp = cfgTmpDir cfg </> "cache" </> "biomartr"
      outs  = hashedTmp cfg expr [bmFn, sTable]
  outs %> \out -> do
    need [bmFn, sTable]
    -- TODO should biomartr get multiple output paths?
    quietly $ cmd "biomartr.R" [bmTmp, out, bmFn, sTable]
  return outs
cBioMartR _ _ _ = error "bad cBioMartR call"
