module Main where

-- import Paths_ShortCut        (version)
-- import ShortCut.Core         (runRepl, evalFile)
import ShortCut.Modules      (modules)
import ShortCut.Core.Types   (CutConfig(..))

-- generate reference page:
--   load config? or does it not matter?
--   cfgModules cfg -> list of modules
--   for each module:
--     first print the list with markdown sections
--     add a general description of the module here
--     then go through the functions:
--       mFunctions mod -> fName etc to get info

-- write short module descriptions for all of them

moduleReference :: CutModule -> [String]
moduleReference m =
  [ "## " ++ mName ++ " module"
  , mDesc m
  ]

-- TODO pick module order to print the reference nicely
-- TODO add blank lines between everything?
main :: IO ()
main = print $ unlines $ map moduleReference modules
