module Main where

-- import Paths_ShortCut        (version)
-- import ShortCut.Core         (runRepl, evalFile)
import ShortCut.Core.Types
import ShortCut.Modules (modules)
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)

-- TODO should this generate markdown or html? markdown probably easier

-- generate reference page:
--   load config? or does it not matter?
--   cfgModules cfg -> list of modules
--   for each module:
--     first print the list with markdown sections
--     add a general description of the module here

--     for each type:
--       print its desc
--       later: iterate through listFunctions and try to find ones that take/accept it? (not if hard)
--       later: hyperlink to them if easy

--     for each function:
--       mFunctions mod -> fName etc to get info
--       later: write help for each one (just the important, confusing ones for now)

-- write short module descriptions for all of them

-- TODO these aren't functions!
typesTable :: CutModule -> [String]
typesTable m =
  [ "Types:"
  , ""
  , "| Ext | Meaning |"
  , "| :-- | :------ |"
  ]
  -- ++ map (\f -> "| " ++ fName f ++ " | " ++ (fromMaybe "" $ fDesc f) ++ " |") (mFunctions m)
  ++ map (\f -> "| " ++ fName f ++ " | " ++ "" ++ " |") (mFunctions m)
  ++ [""]

functionType :: CutFunction -> String
functionType = concat . tail . splitOn ":" . fTypeDesc

functionsTable :: CutModule -> [String]
functionsTable m =
  [ "Functions:"
  , ""
  , "| Name | Type |"
  , "| :--- | :--- |"
  ]
  ++ map (\f -> "| `" ++ fName f ++ "` | `" ++ functionType f ++ "` |") (mFunctions m)
  ++ [""]

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
  ++ [""]

-- TODO pick module order to print the reference nicely
main :: IO ()
main = writeFile "reference.md" $ unlines $ concatMap moduleReference modules
