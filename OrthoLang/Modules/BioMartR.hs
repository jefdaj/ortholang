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
-- import OrthoLang.Types (readLits, writeLits, traceA, need', runCmd, CmdDesc(..))
-- import OrthoLang.Types  (exprPath, Path, toPath, fromPath)
-- import OrthoLang.Types (rExpr)
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
-- import OrthoLang.Types   (traceA)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import OrthoLang.Util (trim)

------------------------
-- module description --
------------------------

olModule :: Module
olModule = Module
  { mName = "BiomartR"
  , mDesc = "Search + download genomes and proteomes from Biomart"
  , mTypes = [search, fna, faa, fnagz, faagz]
  , mGroups = []
  , mEncodings = [gz]
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

gz :: Encoding
gz = Encoding
  { enExt = "gz"
  , enDesc = "gunzip archive" -- TODO and maybe also tarballed?
  , enShow = \_ _ path -> return $ "gunzip archive '" ++ path ++ "'"
  }

fnagz :: Type
fnagz = EncodedAs gz fna

faagz :: Type
faagz = EncodedAs gz faa

-- TODO does this work at all?
parseSearches :: Function
parseSearches = let name = "parse_searches" in Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly (ListOf str)]
  , fOutput = Exactly search
  , fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rParseSearches
  }

-----------------
-- get_genomes --
-----------------

getGenomes :: Function
getGenomes = let name = "get_genomes" in Function
  { fOpChar = Nothing, fName = name 
  , fInputs = [Exactly (ListOf str)]
  , fOutput = Exactly (ListOf fnagz)
  , fTags = [ReadsURL]
  , fNewRules = NewNotImplemented, fOldRules = rBioMartR "getGenome"
  }

-------------------
-- get_proteomes --
-------------------

getProteomes :: Function
getProteomes = let name = "get_proteomes" in Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly (ListOf str)]
  , fOutput = Exactly (ListOf faagz)
  , fTags = [ReadsURL]
  , fNewRules = NewNotImplemented, fOldRules = rBioMartR "getProteome"
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
  let loc = "modules.biomartr.rParseSearches"
      searchTable  = exprPath cfg dRef scr expr
      searchTable' = fromPath loc cfg searchTable
      sList' = toPath loc cfg sList
  searchTable' %> \_ -> aParseSearches sList' searchTable
  return (ExprPath searchTable')
rParseSearches _ e = error $ "bad arguments to rParseSearches: " ++ show e

aParseSearches :: Path -> Path -> Action ()
aParseSearches sList out = do
  cfg <- fmap fromJust getShakeExtra
  let sList' = fromPath loc cfg sList
      out'   = fromPath loc cfg out
      loc = "modules.biomartr.aParseSearches"
      out''  = traceA loc out' [sList', out']
  parses <- (fmap . map) readSearch (readLits loc sList')
  let (errors, searches') = partitionEithers parses
  -- TODO better error here
  if (not . null) errors
    then error "invalid search!"
    else writeLits loc out'' $ toTsvRows searches'

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
rBioMartR fn scr expr@(Fun rtn seed _ _ [ss]) = do
  (ExprPath bmFn  ) <- rExpr scr (Lit str fn)
  -- (ExprPath sTable) <- rParseSearches s ss
  (ExprPath sTable) <- rExpr scr $ Fun rtn seed (depsOf ss) "parse_searches" [ss]
  -- TODO separate tmpDirs for genomes, proteomes, etc?
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let loc = "modules.biomartr.rBioMartR"
      bmTmp   = tmpdir cfg </> "cache" </> "biomartr"
      tmp'    = toPath loc cfg bmTmp
      out     = exprPath cfg dRef scr expr
      out'    = fromPath loc cfg out
      sTable' = toPath loc cfg sTable
      bmFn'   = toPath loc cfg bmFn
  out' %> \_ -> aBioMartR out bmFn' tmp' sTable'
  return (ExprPath out')
rBioMartR _ _ _ = error "bad rBioMartR call"

aBioMartR :: Path -> Path -> Path -> Path -> Action ()
aBioMartR out bmFn bmTmp sTable = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.biomartr.aBioMartR"
      out'    = fromPath loc cfg out
      bmFn'   = fromPath loc cfg bmFn
      bmTmp'  = fromPath loc cfg bmTmp
      sTable' = fromPath loc cfg sTable
      out'' = traceA "aBioMartR" out' [out', bmFn', bmTmp', sTable']
  need' loc [bmFn', sTable']
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
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdRmPatterns = [] -- TODO remove tmpdir on fail? seems wasteful
    , cmdSanitizePaths = []
    , cmdOptions = [Cwd bmTmp'] -- TODO remove?
    }
