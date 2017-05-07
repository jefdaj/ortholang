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
import Control.Monad (void)
import Text.Parsec            (spaces, runParser)
import Text.Parsec (Parsec, try, choice, (<|>), many1)
import Text.Parsec.Char (char, string, alphaNum, oneOf)
import Text.Parsec.Combinator (between, optionMaybe)

type Parser a = Parsec String () a

newtype Species = Species String
newtype DbName  = DbName  String
newtype DbIden  = DbIden  String
data Search  = Search Species (Maybe DbName) (Maybe DbIden)

pSpecies :: Parser Species
pSpecies = fmap Species $ many1 $ alphaNum <|> char '.'

-- TODO be stricter in only accepting valid IDs here?
pDbIden :: Parser DbIden
pDbIden = fmap DbIden $ many1 $ alphaNum <|> oneOf "_."

-- TODO convert to lowercase first?
pDbName :: Parser DbName
pDbName = fmap DbName $ choice $ map (try . string)
  [ "genbank"
  , "refseq"
  , "ensembl"
  , "ensemblgenomes"
  ]

pFilters :: Parser (Maybe DbName, Maybe DbIden)
pFilters = do
  -- TODO fail on empty parens?
  name <- optionMaybe $ try $ pDbName <* spaces -- TODO leading space OK?
  iden <- optionMaybe pDbIden -- TODO trailing space OK?
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

cutModule :: CutModule
cutModule = CutModule
  { mName = "biomartr"
  , mFunctions =
    [ getGenome
    , getGenomes
    ]
  }

-- TODO is this needed, since we're just doing stuff in the compiler?
search :: CutType
search = CutType
  { tExt  = "tsv" -- TODO something custom like .search or .bms?
  , tDesc = "table describing biomartr searches"
  , tCat  = id
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

-- runParser :: Parser a -> String -> Either ParseError a
-- runParser p = runParser p "somefile"

-- TODO this is where to parse the searches?
cGetGenome :: CutConfig -> CutExpr -> Rules FilePath
cGetGenome cfg expr@(CutFun _ _ [s]) = undefined
cGetGenome cfg _ = error "bad cGetGenome call"

-- TODO this is where to parse the searches?
cGetGenomes :: CutConfig -> CutExpr -> Rules FilePath
cGetGenomes cfg expr@(CutFun _ _ ss) = undefined
cGetGenomes cfg _ = error "bad cGetGenomes call"
