-- Simple Golden tests to confirm depCmds are found.
-- TODO should it be more flexible about the exact versions?

module ShortCut.Test.Deps where

import Data.ByteString.Lazy.Char8 (pack)
import Development.Shake.FilePath ((<.>), (</>))
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Types        (CutConfig(..))
import System.Process             (shell, readCreateProcessWithExitCode)
import Test.Tasty                 (TestTree, TestName, testGroup)
import Test.Tasty.Golden          (goldenVsString)

depCmds :: [(String, String)]
depCmds =
  [ ("ncbi-blast", "blastx -version")
  , ("crb-blast" , "crb-blast --version")
  , ("python"    , "python --version")
  , ("r"         , "R --version")
  , ("biopython" , "python -c \"import Bio; print Bio.__version__\"")
  , ("biomartr"  , "Rscript -e \"require(biomartr); packageVersion('biomartr')\"")
  , ("dplyr"     , "Rscript -e \"require(dplyr); packageVersion('dplyr')\"")
  ]

-- Unlike the other tests, these don't need access to the runtime config
mkTests :: CutConfig -> IO TestTree
mkTests _ = do
  testDir <- getDataFileName $ "tests" </> "deps"
  return $ testGroup "Deps" $ map (mkTestDep testDir) depCmds

mkTestDep :: FilePath -> (TestName, String) -> TestTree
mkTestDep dir (name, wrappedCmd) = goldenVsString name gld act
  where
    gld = dir </> name <.> "txt"
    act = do
      (_, out, err) <- readCreateProcessWithExitCode (shell wrappedCmd) ""
      return $ pack $ err ++ out
