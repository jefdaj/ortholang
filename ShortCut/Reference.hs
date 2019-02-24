module ShortCut.Reference where

import ShortCut.Core.Types

import Data.List.Split  (splitOn)
import Data.List.Utils  (join)
import ShortCut.Modules (modules)
import Data.Char        (toLower, isAlphaNum)

explainType :: CutType -> String
explainType Empty = error "explain empty type"
explainType (ListOf   t) = explainType t -- TODO add the list part?
explainType (ScoresOf t) = explainType t -- TODO add the scores part?
explainType t = "| `" ++ tExt t ++ "` | " ++ tDesc t ++ " |"

-- TODO these aren't functions!
typesTable :: CutModule -> [String]
typesTable m = if null (mTypes m) then [""] else
  [ "Types:"
  , ""
  , "| Extension | Meaning |"
  , "| :-------- | :------ |"
  ]
  -- ++ map (\f -> "| " ++ fName f ++ " | " ++ (fromMaybe "" $ fDesc f) ++ " |") (mFunctions m)
  -- ++ map (\f -> "| " ++ fName f ++ " | " ++ "" ++ " |") (mFunctions m)
  ++ map explainType (mTypes m)
  ++ [""]

explainFunction :: CutFunction -> String
explainFunction = join " | " . barred . map quoted . elems
  where
    elems  f  = filter (not . (`elem` [":", "->"])) $ splitOn " " $ fTypeDesc f
    barred es = [head es, join ", " $ init $ tail es, last es]
    quoted t  = "`" ++ t ++ "`"

functionsTable :: CutModule -> [String]
functionsTable m = if null (mFunctions m) then [""] else
  [ "Functions:"
  , ""
  , "| Name | Inputs | Output |"
  , "| :--- | :----- | :----- |"
  ]
  ++ map (\f -> "| " ++ explainFunction f ++ " |") (mFunctions m)
  ++ [""]

header :: String
header = "{% import \"macros.jinja\" as macros with context %}"

loadExample :: CutModule -> [String]
loadExample m = ["Examples:", "", "{{ macros.load_cut(user, 'examples/" ++ name ++ ".cut') }}"]
  where
    name = filter isAlphaNum $ map toLower $ mName m

-- TODO only use this as default if there's no custom markdown description written?
-- TODO or move that stuff to the tutorial maybe?
moduleReference :: CutModule -> [String]
moduleReference m =
  [ "## " ++ mName m ++ " module"
  , ""
  , mDesc m ++ "."
  , ""
  ]
  ++ typesTable m
  ++ functionsTable m
  ++ loadExample m
  ++ [""]

-- TODO pick module order to print the reference nicely
writeReference :: IO ()
writeReference = writeFile "reference.md" $ unlines $ 
  [ header, ""] ++
  concatMap moduleReference modules
