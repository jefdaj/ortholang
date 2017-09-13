module ShortCut.Modules.BioMartR where

-- TODO put nix and R stuff in here too (ask how best on StackOverflow/Nix ML)
-- TODO parse strings into "search tables" (name + optional db + optional id)
-- TODO separate cache dir for this stuff, and "load"/link results to it
-- TODO "show" the results using that convenient .txt file biomartr saves
-- TODO combine databases if none is specified
-- TODO once this works, document it in a notebook entry!
-- TODO known bug: hashes are getting mixed up in rParseSearches

-- TODO what's the overall plan?
--      1. parse searches regardless of function being used
--      2. feed search table to biomartr script along with fn name

-- import ShortCut.Modules.Blast (gom) -- TODO fix that/deprecate
import ShortCut.Core.Types
import Development.Shake
import ShortCut.Core.Paths   (exprPath, exprPathExplicit)
import ShortCut.Core.Rules (rExpr, defaultTypeCheck)
import ShortCut.Core.Config (wrappedCmd)
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
-- import System.FilePath (takeDirectory, takeFileName)

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
  , tShow  = undefined
  }

-- TODO unify with fna? or replace it?
fnagz :: CutType
fnagz = CutType
  { tExt  = "fna.gz"
  , tDesc = "gzipped fasta nucleic acid acid (gene list or genome)"
  , tShow  = \_ -> return "tShow not implemented yet for fnagz"
  }

-- TODO unify with faa? or replace it?
faagz :: CutType
faagz = CutType
  { tExt  = "faa.gz"
  , tDesc = "gzipped fasta amino acid (proteome)"
  , tShow  = \_ -> return "tShow not implemented yet for faagz"
  }

parseSearch :: CutFunction
parseSearch = CutFunction
  { fName      = "parse_search"
  , fTypeCheck = defaultTypeCheck [str] search
  , fFixity    = Prefix
  , fRules  = rParseSearches
  }

getGenomes :: CutFunction
getGenomes = CutFunction
  { fName      = "get_genomes"
  , fTypeCheck = defaultTypeCheck [(SetOf str)] (SetOf fnagz)
  , fFixity    = Prefix
  , fRules  = rBioMartR "getGenome"
  }

getProteomes :: CutFunction
getProteomes = CutFunction
  { fName      = "get_proteomes"
  , fTypeCheck = defaultTypeCheck [(SetOf str)] (SetOf faagz)
  , fFixity    = Prefix
  , fRules  = rBioMartR "getProteome"
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

rParseSearches :: CutState -> CutExpr -> Rules ExprPath
rParseSearches s@(_,cfg) expr@(CutSet _ _ _ _) = do
  (ExprPath sList) <- rExpr s expr
  -- TODO should this be a cacheFile instead?
  let (ExprPath searchTable) = exprPathExplicit cfg True search "parse_searches"
                                                [show expr, sList]
  searchTable %> \out -> aParseSearches cfg sList out
  return (ExprPath searchTable)
rParseSearches _ _ = error "bad arguments to rParseSearches"

aParseSearches :: CutConfig -> FilePath -> FilePath -> Action ()
aParseSearches cfg sList out = do
  tmp <- readFile' sList
  let sLines = map (cfgTmpDir cfg </>) (lines tmp)
  need sLines
  parses <- liftIO $ mapM readSearch sLines
  let (errors, searches') = partitionEithers parses
  -- TODO better error here
  if (not . null) errors
    then error "invalid search!"
    else liftIO $ writeFile out $ toTsv searches'

------------------
-- run biomartr --
------------------

-- TODO this is where to parse the searches?
-- cGetGenome :: CutConfig -> CutExpr -> Rules ExprPath
-- cGetGenome (_,cfg) expr@(CutFun _ _ _ [s]) = undefined
-- cGetGenome _ _ = error "bad cGetGenome call"

-- TODO factor out a "trivial string file" function?
rBioMartR :: String -> CutState -> CutExpr -> Rules ExprPath
rBioMartR fn s@(_,cfg) expr@(CutFun _ _ _ _ [ss]) = do
  (ExprPath bmFn  ) <- rExpr s (CutLit str 0 fn)
  (ExprPath sTable) <- rParseSearches s ss
  -- TODO separate tmpDirs for genomes, proteomes, etc?
  let bmTmp = cfgTmpDir cfg </> "cache" </> "biomartr"
      (ExprPath outs) = exprPath cfg True expr [ExprPath bmFn, ExprPath sTable]
  outs %> \_ -> do
    need [bmFn, sTable]
    -- TODO should biomartr get multiple output paths?
    quietly $ wrappedCmd cfg [outs] [Cwd bmTmp] "biomartr.R" [outs, bmFn, sTable]
  return (ExprPath outs)
rBioMartR _ _ _ = error "bad rBioMartR call"
