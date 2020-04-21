module OrthoLang.Test.Help where

import OrthoLang.Types
import OrthoLang.Interpreter (help, helpTopics)
import OrthoLang.Modules (modules)
import qualified Data.ByteString.Lazy.Char8 as B8

import OrthoLang.Test.Scripts (goldenDiff)
import Paths_OrthoLang       (getDataFileName)
import System.FilePath        ((</>), (<.>))
import Test.Tasty             (TestTree, testGroup)

mkHelpTest :: Config -> FilePath -> String -> TestTree
mkHelpTest cfg hDir topic = goldenDiff td gld helpAct
  where
    td = "help for '" ++ topic ++ "' looks right"
    gld = hDir </> topic <.> "txt"
    helpAct = do
      txt <- help cfg modules topic
      return $ B8.pack txt

mkTests :: Config -> LocksRef -> IDsRef -> DigestsRef -> IO TestTree
mkTests cfg _ _ _ = do
  hDir <- getDataFileName "tests/help"
  let group = map (mkHelpTest cfg hDir) $ helpTopics cfg modules
  return $ testGroup "check repl :help text" group
