module Detourrr.Modules.BioMartR where

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
--      2. feed search table to biomartr script along with fn name

-- import Detourrr.Modules.Blast (gom) -- TODO fix that/deprecate
import Detourrr.Core.Types
import Development.Shake
import Detourrr.Core.Actions (readLits, writeLits, debugA, debugNeed)
import Detourrr.Core.Paths  (exprPath, DtrPath, toDtrPath, fromDtrPath)
import Detourrr.Core.Compile.Basic (rExpr, defaultTypeCheck)
import Detourrr.Core.Actions           (wrappedCmdWrite)
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
-- import Detourrr.Core.Debug   (debugA)
import System.Directory (createDirectoryIfMissing)

------------------------
-- module description --
------------------------

dtrModule :: DtrModule
dtrModule = DtrModule
  { mName = "BiomartR"
  , mDesc = "Search + download genomes and proteomes from Biomart"
  , mTypes = [search, fnagz, faagz]
  , mFunctions =
    [ parseSearches -- TODO hide from end users?
    -- TODO single and _each versions?
    , getGenomes
    , getProteomes
    -- , getGenome
    ]
  }

search :: DtrType
search = DtrType
  { tExt  = "search" -- TODO should these be recognizable (tsv)?
  , tDesc = "intermediate table describing biomartr searches"
  -- , tShow = \ls f -> readFileStrict ls f
  , tShow = defaultShow
  }

-- TODO unify with fna? or replace it?
fnagz :: DtrType
fnagz = DtrType
  { tExt  = "fna.gz"
  , tDesc = "gzipped fasta nucleic acid acid (gene list or genome)"
  , tShow = \_ _ f -> return $ "gzipped fna file '" ++ f ++ "'"
  }

-- TODO unify with faa? or replace it?
faagz :: DtrType
faagz = DtrType
  { tExt  = "faa.gz"
  , tDesc = "gzipped fasta amino acid (proteome)"
  , tShow = \_ _ f -> return $ "gzipped faa file '" ++ f ++ "'"
  }

-- TODO does this work at all?
parseSearches :: DtrFunction
parseSearches = let name = "parse_searches" in DtrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf str] search
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name [ListOf str] search
  , fFixity    = Prefix
  , fRules     = rParseSearches
  }

-----------------
-- get_genomes --
-----------------

getGenomes :: DtrFunction
getGenomes = let name = "get_genomes" in DtrFunction
  { fName      = name 
  , fTypeCheck = defaultTypeCheck [(ListOf str)] (ListOf fnagz)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name [(ListOf str)] (ListOf fnagz)
  , fFixity    = Prefix
  , fRules     = rBioMartR "getGenome"
  }

-------------------
-- get_proteomes --
-------------------

getProteomes :: DtrFunction
getProteomes = let name = "get_proteomes" in DtrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [(ListOf str)] (ListOf faagz)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name [(ListOf str)] (ListOf faagz)
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
toTsvRows :: [Search] -> [String]
toTsvRows ss = map (intercalate "\t") (header:map row ss)
  where
    header             = ["organism", "database", "identifier"]
    row (Search s d i) = [s, fromMaybe "NA" d, fromMaybe "NA" i]

rParseSearches :: DtrState -> DtrExpr -> Rules ExprPath
rParseSearches s@(_, cfg, ref, ids) expr@(DtrFun _ _ _ _ [searches]) = do
  (ExprPath sList) <- rExpr s searches
  let searchTable  = exprPath s expr
      searchTable' = fromDtrPath cfg searchTable
      sList' = toDtrPath cfg sList
  searchTable' %> \_ -> aParseSearches cfg ref ids sList' searchTable
  return (ExprPath searchTable')
rParseSearches _ e = error $ "bad arguments to rParseSearches: " ++ show e

aParseSearches :: DtrConfig -> Locks -> HashedSeqIDsRef -> DtrPath -> DtrPath -> Action ()
aParseSearches cfg ref _ sList out = do
  parses <- (fmap . map) readSearch (readLits cfg ref sList')
  let (errors, searches') = partitionEithers parses
  -- TODO better error here
  if (not . null) errors
    then error "invalid search!"
    else writeLits cfg ref out'' $ toTsvRows searches'
  where
    sList' = fromDtrPath cfg sList
    out'   = fromDtrPath cfg out
    out''  = debugA cfg "aParseSearches" out' [sList', out']

------------------
-- run biomartr --
------------------

-- TODO move nearer the top?

-- TODO this is where to parse the searches?
-- cGetGenome :: DtrConfig -> DtrExpr -> Rules ExprPath
-- cGetGenome (_,cfg) expr@(DtrFun _ _ _ [s]) = undefined
-- cGetGenome _ _ = error "bad cGetGenome call"

-- TODO rewrite in expression editing style, inserting parse_searches
rBioMartR :: String -> DtrState -> DtrExpr -> Rules ExprPath
rBioMartR fn s@(_, cfg, ref, ids) expr@(DtrFun rtn salt _ _ [ss]) = do
  (ExprPath bmFn  ) <- rExpr s (DtrLit str 0 fn)
  -- (ExprPath sTable) <- rParseSearches s ss
  (ExprPath sTable) <- rExpr s $ DtrFun rtn salt (depsOf ss) "parse_searches" [ss]
  -- TODO separate tmpDirs for genomes, proteomes, etc?
  let bmTmp   = cfgTmpDir cfg </> "cache" </> "biomartr"
      tmp'    = toDtrPath cfg bmTmp
      out     = exprPath s expr
      out'    = fromDtrPath cfg out
      sTable' = toDtrPath cfg sTable
      bmFn'   = toDtrPath cfg bmFn
  out' %> \_ -> aBioMartR cfg ref ids out bmFn' tmp' sTable'
  return (ExprPath out')
rBioMartR _ _ _ = error "bad rBioMartR call"

aBioMartR :: DtrConfig -> Locks -> HashedSeqIDsRef
          -> DtrPath -> DtrPath -> DtrPath -> DtrPath -> Action ()
aBioMartR cfg ref _ out bmFn bmTmp sTable = do
  debugNeed cfg "aBioMartR" [bmFn', sTable']
  -- TODO should biomartr get multiple output paths?
  liftIO $ createDirectoryIfMissing True bmTmp'
  wrappedCmdWrite False True cfg ref out'' [bmFn', sTable'] [] [Cwd bmTmp']
    "biomartr.R" [out'', bmFn', sTable']
  where
    out'    = fromDtrPath cfg out
    bmFn'   = fromDtrPath cfg bmFn
    bmTmp'  = fromDtrPath cfg bmTmp
    sTable' = fromDtrPath cfg sTable
    out'' = debugA cfg "aBioMartR" out' [out', bmFn', bmTmp', sTable']
