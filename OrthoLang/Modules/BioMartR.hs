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
import OrthoLang.Core
import Development.Shake
import OrthoLang.Core (readLits, writeLits, traceA, need', runCmd, CmdDesc(..))
import OrthoLang.Core  (exprPath, Path, toPath, fromPath)
import OrthoLang.Core (rExpr, defaultTypeCheck)
import Control.Monad (void)
import Text.Parsec            (spaces, runParser)
import Text.Parsec (Parsec, try, choice, (<|>), many1)
import Text.Parsec.Char (char, string, alphaNum, oneOf)
import Text.Parsec.Combinator (between, optionMaybe)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (intercalate)
import Data.Either (partitionEithers)
import Data.Char (isSpace)
import Development.Shake.FilePath ((</>))
-- import OrthoLang.Core   (traceA)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))

------------------------
-- module description --
------------------------

orthoLangModule :: Module
orthoLangModule = Module
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

search :: Type
search = Type
  { tExt  = "search" -- TODO should these be recognizable (tsv)?
  , tDesc = "intermediate table describing biomartr searches"
  -- , tShow = \ls f -> readFileStrict ls f
  , tShow = defaultShow
  }

-- TODO unify with fna? or replace it?
fnagz :: Type
fnagz = Type
  { tExt  = "fna.gz"
  , tDesc = "gzipped fasta nucleic acid acid (gene list or genome)"
  , tShow = \_ _ f -> return $ "gzipped fna file \"" ++ f ++ "\""
  }

-- TODO unify with faa? or replace it?
faagz :: Type
faagz = Type
  { tExt  = "faa.gz"
  , tDesc = "gzipped fasta amino acid (proteome)"
  , tShow = \_ _ f -> return $ "gzipped faa file \"" ++ f ++ "\""
  }

-- TODO does this work at all?
parseSearches :: Function
parseSearches = let name = "parse_searches" in Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [ListOf str] search
  , fTypeDesc  = mkTypeDesc name [ListOf str] search
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rParseSearches
  }

-----------------
-- get_genomes --
-----------------

getGenomes :: Function
getGenomes = let name = "get_genomes" in Function
  { fOpChar = Nothing, fName = name 
  , fTypeCheck = defaultTypeCheck name [(ListOf str)] (ListOf fnagz)
  , fTypeDesc  = mkTypeDesc name [(ListOf str)] (ListOf fnagz)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rBioMartR "getGenome"
  }

-------------------
-- get_proteomes --
-------------------

getProteomes :: Function
getProteomes = let name = "get_proteomes" in Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [(ListOf str)] (ListOf faagz)
  , fTypeDesc  = mkTypeDesc name [(ListOf str)] (ListOf faagz)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rBioMartR "getProteome"
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

rParseSearches :: RulesFn
rParseSearches scr expr@(Fun _ _ _ _ [searches]) = do
  (ExprPath sList) <- rExpr scr searches
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let searchTable  = exprPath cfg dRef scr expr
      searchTable' = fromPath cfg searchTable
      sList' = toPath cfg sList
  searchTable' %> \_ -> aParseSearches sList' searchTable
  return (ExprPath searchTable')
rParseSearches _ e = error $ "bad arguments to rParseSearches: " ++ show e

aParseSearches :: Path -> Path -> Action ()
aParseSearches sList out = do
  cfg <- fmap fromJust getShakeExtra
  let sList' = fromPath cfg sList
      out'   = fromPath cfg out
      out''  = traceA "aParseSearches" out' [sList', out']
  parses <- (fmap . map) readSearch (readLits sList')
  let (errors, searches') = partitionEithers parses
  -- TODO better error here
  if (not . null) errors
    then error "invalid search!"
    else writeLits out'' $ toTsvRows searches'

------------------
-- run biomartr --
------------------

-- TODO move nearer the top?

-- TODO this is where to parse the searches?
-- cGetGenome :: Config -> Expr -> Rules ExprPath
-- cGetGenome (scr,cfg) expr@(Fun _ _ _ [s]) = undefined
-- cGetGenome _ _ = error "bad cGetGenome call"

-- TODO rewrite in expression editing style, inserting parse_searches
rBioMartR :: String -> RulesFn
rBioMartR fn scr expr@(Fun rtn salt _ _ [ss]) = do
  (ExprPath bmFn  ) <- rExpr scr (Lit str (Salt 0) fn)
  -- (ExprPath sTable) <- rParseSearches s ss
  (ExprPath sTable) <- rExpr scr $ Fun rtn salt (depsOf ss) "parse_searches" [ss]
  -- TODO separate tmpDirs for genomes, proteomes, etc?
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let bmTmp   = cfgTmpDir cfg </> "cache" </> "biomartr"
      tmp'    = toPath cfg bmTmp
      out     = exprPath cfg dRef scr expr
      out'    = fromPath cfg out
      sTable' = toPath cfg sTable
      bmFn'   = toPath cfg bmFn
  out' %> \_ -> aBioMartR out bmFn' tmp' sTable'
  return (ExprPath out')
rBioMartR _ _ _ = error "bad rBioMartR call"

aBioMartR :: Path -> Path -> Path -> Path -> Action ()
aBioMartR out bmFn bmTmp sTable = do
  cfg <- fmap fromJust getShakeExtra
  let out'    = fromPath cfg out
      bmFn'   = fromPath cfg bmFn
      bmTmp'  = fromPath cfg bmTmp
      sTable' = fromPath cfg sTable
      out'' = traceA "aBioMartR" out' [out', bmFn', bmTmp', sTable']
  need' "ortholang.modules.biomartr.aBioMartR" [bmFn', sTable']
  -- TODO should biomartr get multiple output paths?
  liftIO $ createDirectoryIfMissing True bmTmp'
  -- wrappedCmdWrite False True cfg ref out'' [bmFn', sTable'] [] [Cwd bmTmp']
  --   "biomartr.R" [out'', bmFn', sTable']
  runCmd $ CmdDesc
    { cmdBinary = "biomartr.R"
    , cmdArguments = [out'', bmFn', sTable']
    , cmdExitCode = ExitSuccess
    , cmdOutPath = out''
    , cmdFixEmpties = True
    , cmdParallel = False
    , cmdInPatterns = [bmFn', sTable']
    , cmdExtraOutPaths = []
    , cmdRmPatterns = [] -- TODO remove tmpdir on fail? seems wasteful
    , cmdSanitizePaths = []
    , cmdOptions = [Cwd bmTmp'] -- TODO remove?
    }
