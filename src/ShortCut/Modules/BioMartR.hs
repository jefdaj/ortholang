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
import ShortCut.Core.Actions (readLits)
import ShortCut.Core.Paths  (exprPath, CutPath, toCutPath, fromCutPath)
import ShortCut.Core.Compile.Basic (rExpr, defaultTypeCheck)
import ShortCut.Core.Actions           (wrappedCmd)
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
import ShortCut.Core.Actions (debugTrackWrite)
import ShortCut.Core.Debug   (debugAction)
import System.Directory           (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

------------------------
-- module description --
------------------------

cutModule :: CutModule
cutModule = CutModule
  { mName = "biomartr"
  , mFunctions =
    [ parseSearches -- TODO hide from end users?
    -- TODO single and _each versions?
    , getGenomes
    , getProteomes
    -- , getGenome
    ]
  }

search :: CutType
search = CutType
  { tExt  = "search" -- TODO should these be recognizable (tsv)?
  , tDesc = "intermediate table describing biomartr searches"
  , tShow = readFile
  }

-- TODO unify with fna? or replace it?
fnagz :: CutType
fnagz = CutType
  { tExt  = "fna.gz"
  , tDesc = "gzipped fasta nucleic acid acid (gene list or genome)"
  , tShow = \_ -> return "tShow not implemented yet for fnagz"
  }

-- TODO unify with faa? or replace it?
faagz :: CutType
faagz = CutType
  { tExt  = "faa.gz"
  , tDesc = "gzipped fasta amino acid (proteome)"
  , tShow = \_ -> return "tShow not implemented yet for faagz"
  }

-- TODO does this work at all?
parseSearches :: CutFunction
parseSearches = CutFunction
  { fName      = "parse_searches"
  , fTypeCheck = defaultTypeCheck [ListOf str] search
  , fFixity    = Prefix
  , fRules     = rParseSearches
  }

-----------------
-- get_genomes --
-----------------

getGenomes :: CutFunction
getGenomes = CutFunction
  { fName      = "get_genomes"
  , fTypeCheck = defaultTypeCheck [(ListOf str)] (ListOf fnagz)
  , fFixity    = Prefix
  , fRules     = rBioMartR "getGenome"
  }

-------------------
-- get_proteomes --
-------------------

getProteomes :: CutFunction
getProteomes = CutFunction
  { fName      = "get_proteomes"
  , fTypeCheck = defaultTypeCheck [(ListOf str)] (ListOf faagz)
  , fFixity    = Prefix
  , fRules     = rBioMartR "getProteome"
  }

--------------------
-- parse_searches --
--------------------

type Parser a = Parsec String () a

type Species    = String
type Database   = String
type Identifier = String

data Search = Search Species (Maybe Database) (Maybe Identifier)
  deriving (Eq, Show)

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
readSearch :: String -> Either String Search
readSearch txt = case runParser pSearch () "search string" txt of
  Left  e -> Left  $ show e
  Right s@(Search _ _ _) -> Right s

-- TODO use cassava?
toTsv :: [Search] -> String
toTsv ss = unlines $ map (intercalate "\t") (header:map row ss)
  where
    header             = ["organism", "database", "identifier"]
    row (Search s d i) = [s, fromMaybe "NA" d, fromMaybe "NA" i]

rParseSearches :: CutState -> CutExpr -> Rules ExprPath
rParseSearches s@(_,cfg) expr@(CutFun _ _ _ _ [searches]) = do
  (ExprPath sList) <- rExpr s searches
  -- TODO should this be a cacheFile instead?
  -- exprPathExplicit (_, cfg) prefix rtype salt hashes = toCutPath cfg path [show expr, sList]
  -- let searchTable = fromCutPath cfg $ exprPathExplicit s "parse_searches" search salt []
  let searchTable  = exprPath s expr
      searchTable' = fromCutPath cfg searchTable
      sList' = toCutPath cfg sList
  searchTable' %> \_ -> aParseSearches cfg sList' searchTable
  return (ExprPath searchTable')
rParseSearches _ e = error $ "bad arguments to rParseSearches: " ++ show e

aParseSearches :: CutConfig -> CutPath -> CutPath -> Action ()
aParseSearches cfg sList out = do
  parses <- (fmap . map) readSearch (readLits cfg sList')
  -- let sLines = map (cfgTmpDir cfg </>) (lines tmp)
  -- need sLines
  -- parses <- liftIO $ mapM readSearch sLines
  let (errors, searches') = partitionEithers parses
  -- TODO better error here
  if (not . null) errors
    then error "invalid search!"
    else liftIO $ do -- TODO rewrite in newer writePaths/Lits?
      createDirectoryIfMissing True $ takeDirectory out''
      writeFile out'' $ toTsv searches'
  where
    sList' = fromCutPath cfg sList
    out'   = fromCutPath cfg out
    out''  = debugAction cfg "aParseSearches" out' [sList', out']

------------------
-- run biomartr --
------------------

-- TODO move nearer the top?

-- TODO this is where to parse the searches?
-- cGetGenome :: CutConfig -> CutExpr -> Rules ExprPath
-- cGetGenome (_,cfg) expr@(CutFun _ _ _ [s]) = undefined
-- cGetGenome _ _ = error "bad cGetGenome call"

-- TODO rewrite in expression editing style, inserting parse_searches
rBioMartR :: String -> CutState -> CutExpr -> Rules ExprPath
rBioMartR fn s@(_,cfg) expr@(CutFun rtn salt _ _ [ss]) = do
  (ExprPath bmFn  ) <- rExpr s (CutLit str 0 fn)
  -- (ExprPath sTable) <- rParseSearches s ss
  (ExprPath sTable) <- rExpr s $ CutFun rtn salt (depsOf ss) "parse_searches" [ss]
  -- TODO separate tmpDirs for genomes, proteomes, etc?
  let bmTmp   = cfgTmpDir cfg </> "cache" </> "biomartr"
      tmp'    = toCutPath cfg bmTmp
      out     = exprPath s expr
      out'    = fromCutPath cfg out
      sTable' = toCutPath cfg sTable
      bmFn'   = toCutPath cfg bmFn
  out' %> \_ -> aBioMartR cfg out bmFn' tmp' sTable'
  return (ExprPath out')
rBioMartR _ _ _ = error "bad rBioMartR call"

aBioMartR :: CutConfig -> CutPath -> CutPath -> CutPath -> CutPath -> Action ()
aBioMartR cfg out bmFn bmTmp sTable = do
  need [bmFn', sTable']
  -- TODO should biomartr get multiple output paths?
  liftIO $ createDirectoryIfMissing True bmTmp'
  quietly $ wrappedCmd cfg [out''] [Cwd bmTmp'] "biomartr.R" [out'', bmFn', sTable']
  debugTrackWrite cfg [out'']
  where
    out'    = fromCutPath cfg out
    bmFn'   = fromCutPath cfg bmFn
    bmTmp'  = fromCutPath cfg bmTmp
    sTable' = fromCutPath cfg sTable
    out'' = debugAction cfg "aBioMartR" out' [out', bmFn', bmTmp', sTable']
