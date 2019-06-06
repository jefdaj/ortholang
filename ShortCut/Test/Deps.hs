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

depCmds :: [(String, String)]
depCmds =
  [ ("biomartr"     , "Rscript -e \"require(biomartr); packageVersion('biomartr')\"")
  , ("biopython"    , "python2 -c \"import Bio; print Bio.__version__\"")
  , ("crb_blast"    , "crb-blast --version") -- should be older NCBI v2.2.29
  , ("diamond"      , "diamond --version")
  , ("dplyr"        , "Rscript -e \"require(dplyr); packageVersion('dplyr')\"")
  , ("mmseqs"       , "mmseqs --help | grep Version")
  , ("muscle"       , "muscle -version")
  , ("ncbi_blast"   , "blastn -version")     -- should also come from psiblast-exb
  , ("orthofinder"  , "orthofinder --help | grep version")
  , ("psiblast"     , "psiblast -version")   -- should be psiblast-exb 2.5.0
  , ("python2"      , "python2 --version")   -- exact version not important
  , ("python3"      , "python3 --version")   -- exact version not important
  , ("r"            , "R --version")         -- exact version not important
  , ("sonicparanoid", "sonicparanoid -h | head -n3 | tail -n1")
  , ("treecl"       , "treeCl --help")       -- has no version output
  ]

knownFailing :: [FilePath]
knownFailing = []

-- Unlike the other tests, these don't need access to the runtime config
mkTests :: CutConfig -> Locks -> HashedSeqIDsRef -> IO TestTree
mkTests cfg _ _ = do
  testDir <- getDataFileName $ "tests"
  return $ testGroup "check dependency versions"
         $ map (mkTestDep cfg testDir) $ filter (\(d, _) -> not $ ("depend_" ++ d) `elem` knownFailing) depCmds

mkTestDep :: CutConfig -> FilePath -> (TestName, String) -> TestTree
mkTestDep _ dir (name, cmd) = goldenVsString desc gld act
  where
    desc = "found expected version of " ++ name
    gld = dir </> "depend" </> "depend_" ++ name <.> "txt"
    act = do
      (_, out, err) <- readCreateProcessWithExitCode (shell cmd) ""
      -- helpful for updating tests
      -- writeFile ("/tmp" </> takeBaseName gld <.> "txt") $ toGeneric cfg $ err ++ out
      return $ pack $ err ++ out
