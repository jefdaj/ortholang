module OrthoLang.Test.Help where

import OrthoLang.Types
import OrthoLang.Interpreter (help)
import OrthoLang.Modules (modules)
import qualified Data.ByteString.Lazy.Char8 as B8

import Data.Char              (toLower)
import Data.List              (sort, nub)
import OrthoLang.Modules      (modules)
import OrthoLang.Test.Scripts (goldenDiff)
import Paths_OrthoLang       (getDataFileName)
import System.FilePath        ((</>), (<.>))
import Test.Tasty             (TestTree, testGroup)

-- TODO make sure lowercase names of everything are unique! got about 10 overlaps here...
-- TODO include something notfound?
-- TODO add bop infix operators (by mapping them to prefix equivalents)
helpTopics :: [String]
helpTopics = sort $ nub $ map (map toLower) $ ts ++ gs ++ es ++ fs ++ ms
  where
    ms = map mName modules
    ts = map tExtOf $ nub $ concatMap mTypes     modules
    gs = map tgExt  $ nub $ concatMap mGroups    modules
    es = map enExt  $ nub $ concatMap mEncodings modules
    fs = map fName  $ nub $ concatMap mFunctions modules

mkHelpTest :: Config -> FilePath -> String -> TestTree
mkHelpTest _ hDir topic = goldenDiff desc gld helpAct
  where
    desc = "help for '" ++ topic ++ "' looks right"
    gld = hDir </> topic <.> "txt"
    helpAct = do
      txt <- help modules topic
      return $ B8.pack txt

mkTests :: Config -> LocksRef -> IDsRef -> DigestsRef -> IO TestTree
mkTests cfg _ _ _ = do
  hDir <- getDataFileName "tests/help"
  let group = map (mkHelpTest cfg hDir) helpTopics
  return $ testGroup "check repl :help text" group
