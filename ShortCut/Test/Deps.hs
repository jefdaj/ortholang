-- Simple Golden tests to confirm depCmds are found.
-- TODO should it be more flexible about the exact versions?

module ShortCut.Test.Deps where

import Data.ByteString.Lazy.Char8 (pack)
import Development.Shake.FilePath ((<.>), (</>))
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Types        (CutConfig(..), Locks, HashedSeqIDsRef)
import System.Process             (shell, readCreateProcessWithExitCode)
import Test.Tasty                 (TestTree, TestName, testGroup)
import Test.Tasty.Golden          (goldenVsString)
-- import Data.IORef                 (IORef)

depCmds :: [(String, String)]
depCmds =
  [ ("psiblast"  , "psiblast -version")   -- should be psiblast-exb 2.5.0
  , ("ncbi_blast", "blastn -version")     -- should also come from psiblast-exb
  , ("crb_blast" , "crb-blast --version") -- should be older NCBI v2.2.29
  , ("python"    , "python --version")    -- exact version not important
  , ("r"         , "R --version")         -- exact version not important
  , ("biopython" , "python -c \"import Bio; print Bio.__version__\"")
  , ("biomartr"  , "Rscript -e \"require(biomartr); packageVersion('biomartr')\"")
  , ("dplyr"     , "Rscript -e \"require(dplyr); packageVersion('dplyr')\"")
  , ("diamond"   , "diamond --version")
  , ("mmseqs"    , "mmseqs --help | grep Version")
  ]

-- Unlike the other tests, these don't need access to the runtime config
mkTests :: CutConfig -> Locks -> HashedSeqIDsRef -> IO TestTree
mkTests _ _ _ = do
  testDir <- getDataFileName $ "tests" </> "dependencies"
  return $ testGroup "check dependency versions"
         $ map (mkTestDep testDir) depCmds

mkTestDep :: FilePath -> (TestName, String) -> TestTree
mkTestDep dir (name, cmd) = goldenVsString name gld act
  where
    gld = dir </> name <.> "txt"
    act = do
      (_, out, err) <- readCreateProcessWithExitCode (shell cmd) ""
      return $ pack $ err ++ out
