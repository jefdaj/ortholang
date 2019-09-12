-- Simple Golden tests to confirm versionScripts are found.
-- TODO should it be more flexible about the exact versions?

module ShortCut.Test.Versions where

import Data.ByteString.Lazy.Char8 (pack)
import Development.Shake.FilePath ((<.>), (</>))
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Types        (CutConfig(..), Locks, HashedIDsRef)
import System.Process             (shell, readCreateProcessWithExitCode)
import Test.Tasty                 (TestTree, TestName, testGroup)
import Test.Tasty.Golden          (goldenVsString)

versionScripts :: [(String, FilePath)]
versionScripts =
  [ ("biomartr"     , "biomartr_version.R")
  , ("busco"        , "busco_version.sh")
  , ("crbblast"     , "crbblast_version.sh")
  , ("diamond"      , "diamond_version.sh")
  , ("mmseqs"       , "mmseqs_version.sh")
  , ("muscle"       , "muscle_version.sh")
  , ("blast"        , "blast_version.sh")
  , ("orthofinder"  , "orthofinder_version.sh")
  , ("psiblast"     , "psiblast_version.sh")
  , ("sonicparanoid", "sonicparanoid_version.sh")
  , ("treecl"       , "treecl_version.sh")
  ]

-- Unlike the other tests, these don't need access to the runtime config
mkTests :: CutConfig -> Locks -> HashedIDsRef -> IO TestTree
mkTests cfg _ _ = do
  testDir <- getDataFileName $ "tests"
  return $ testGroup "check dependency versions"
         $ map (mkTestVersion cfg testDir) versionScripts

mkTestVersion :: CutConfig -> FilePath -> (TestName, String) -> TestTree
mkTestVersion _ dir (name, cmd) = goldenVsString desc gld act
  where
    desc = "found expected version of " ++ name
    gld = dir </> "versions" </>  name ++ "_version" <.> "txt"
    act = do
      (_, out, err) <- readCreateProcessWithExitCode (shell cmd) ""
      -- helpful for updating tests
      -- writeFile ("/tmp" </> takeBaseName gld <.> "txt") $ toGeneric cfg $ err ++ out
      return $ pack $ err ++ out
