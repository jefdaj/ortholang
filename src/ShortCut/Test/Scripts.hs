module ShortCut.Test.Scripts where

import Data.ByteString.Lazy.Char8 (pack)
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Eval         (evalFile)
import ShortCut.Core.Types        (CutConfig(..))
import ShortCut.Core.Util         (mkTestGroup)
import System.FilePath.Posix      (replaceExtension, takeBaseName, takeDirectory, (</>))
import System.IO.Silently         (silence)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsFile, goldenVsString, findByExtension)
import System.Process             (cwd, readCreateProcess, shell)
import Prelude             hiding (writeFile)
import Data.String.Utils          (replace)
import System.Directory           (getCurrentDirectory)
import System.IO                  (stdout)

-- TODO get rid of as many of these as possible
nonDeterministicCut :: FilePath -> Bool
nonDeterministicCut path = (takeDirectory . takeDirectory) path `elem` dirs
  where
    dirs = map ("tests" </>) ["blast", "crb"]

-- TODO oh dammit, use findByExtension
getTestCuts :: IO [FilePath]
getTestCuts = do
  testDir  <- getDataFileName "tests"
  testCuts <- findByExtension [".cut"] testDir
  return testCuts

-- symlinks from the cache -> elsewhere only work when absolute,
-- but that makes them nondeterministic when tests are run from random tmpdirs.
-- this fixes it by editing test output to include a generic $PWD
-- TODO does this misleadinly imply shell interpolation in tree files?
fixWorkingDir :: String -> String -> String
fixWorkingDir wd = replace wd "$PWD"

goldenScriptAndTree :: (FilePath, FilePath, (Maybe FilePath))
                    -> CutConfig -> IO TestTree
goldenScriptAndTree (cut, gld, mtre) cfg = do
  runCut
  return $ testGroup name bothTests
  where
    name       = takeBaseName cut
    cfg'       = cfg { cfgScript = Just cut, cfgTmpDir = (cfgTmpDir cfg </> name) }
    runCut     = silence $ evalFile stdout cfg'
    scriptRes  = (cfgTmpDir cfg' </> "vars" </> "result")
    scriptTest = goldenVsFile "result" gld scriptRes (return ())
    treeCmd    = (shell $ "tree") { cwd = Just $ cfgTmpDir cfg' }
    treeAct    = do
                   out <- readCreateProcess treeCmd ""
                   wd  <- getCurrentDirectory
                   return $ pack $ fixWorkingDir wd out
    treeTest t = goldenVsString "tmpfiles" t treeAct
    bothTests  = case mtre of
                   Nothing -> [scriptTest]
                   Just t  -> [scriptTest, treeTest t]

mkTests :: CutConfig -> IO TestTree
mkTests cfg = do
  cuts <- getTestCuts
  let results = map findResFile  cuts
      mtrees  = map findTreeFile cuts
      groups  = map goldenScriptAndTree (zip3 cuts results mtrees)
  mkTestGroup cfg "interpret test scripts" groups
  where
    findResFile  c = replaceExtension c "result"
    findTreeFile c = if nonDeterministicCut c
                       then Nothing
                       else Just $ replaceExtension c "tree"
