-- Simple Golden tests to confirm dependencies are found.

module ShortCut.Test.Deps where

import Data.ByteString.Lazy.Char8 (pack)
import Development.Shake.FilePath ((<.>), (</>))
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Types        (CutConfig(..))
import System.Process             (shell, readCreateProcessWithExitCode)
import Test.Tasty                 (TestTree, TestName, testGroup)
import Test.Tasty.Golden          (goldenVsString)

deps :: [(String, String)]
deps =
  [ ("ncbi-blast", "blastx -version")
  , ("crb-blast" , "crb-blast --version")
  , ("python"    , "python --version")
  , ("R"         , "R --version")
  -- , ("biopython" , "python -c \"import Bio\"") -- TODO needed at top level?
  -- TODO test each script
  ]

-- Unlike the other tests, these don't need access to the runtime config
mkTests :: CutConfig -> IO TestTree
mkTests _ = do
  testDir <- getDataFileName $ "tests" </> "deps"
  return $ testGroup "Dependencies" $ map (mkTestDep testDir) deps

mkTestDep :: FilePath -> (TestName, String) -> TestTree
mkTestDep dir (name, cmd) = goldenVsString name gld act
  where
    gld = dir </> name <.> "txt"
    act = do
      (_, out, err) <- readCreateProcessWithExitCode (shell cmd) ""
      return $ pack (out ++ err)
