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
import Data.List.Utils            (replace)

-- OS should be replaced with "mac" or "linux" before using these
-- TODO blastdbget makeblastdb cut blast tar hmmsearch orthogroups.py? greencut psiblast? seqiostuff sonicparanoid
versionScripts :: String -> [(String, FilePath)]
versionScripts os = map (\(a,b) -> (a, replace "OS" os b))
  [ ("bash"         , "bash_OS.sh")
  , ("blast"        , "blast_OS.sh")
  , ("busco"        , "busco_OS.sh")
  , ("crbblast"     , "crbblast_OS.sh")
  , ("curl"         , "curl_OS.sh")
  -- , ("tree"         , "tree_OS.sh") -- TODO where should this go?
  , ("cut"          , "cut_OS.sh")
  , ("diamond"      , "diamond_OS.sh")
  , ("hmmer"        , "hmmer_OS.sh")
  , ("makeblastdb"  , "makeblastdb_OS.sh")
  , ("mmseqs"       , "mmseqs_OS.sh")
  , ("muscle"       , "muscle_OS.sh")
  , ("orthofinder"  , "orthofinder_OS.sh")
  , ("psiblast"     , "psiblast_OS.sh")
  , ("python2"      , "python2_OS.sh")
  , ("py2_packages" , "python2_packages_OS.py")
  , ("python3"      , "python3_OS.sh")
  , ("r"            , "r_OS.sh")
  , ("r_packages"   , "r_packages_OS.R")
  , ("sonicparanoid", "sonicparanoid_OS.sh")
  , ("tar"          , "tar_OS.sh")
  -- , ("treecl"       , "treecl_OS.sh")
  ]

-- Unlike the other tests, these don't need access to the runtime config
mkTests :: CutConfig -> Locks -> HashedIDsRef -> IO TestTree
mkTests cfg _ _ = do
  testDir <- getDataFileName $ "tests"
  return $ testGroup "check dependency versions"
         $ map (mkTestVersion cfg testDir) (versionScripts $ cfgOS cfg)

mkTestVersion :: CutConfig -> FilePath -> (TestName, String) -> TestTree
mkTestVersion cfg dir (name, cmd) = goldenVsString desc gld act
  where
    desc = "found expected version of " ++ name
    gld = dir </> "versions" </>  name ++ "_" ++ cfgOS cfg <.> "txt"
    act = do
      (_, out, err) <- readCreateProcessWithExitCode (shell cmd) ""
      -- helpful for updating tests
      -- writeFile ("/tmp" </> takeBaseName gld <.> "txt") $ toGeneric cfg $ err ++ out
      return $ pack $ err ++ out
