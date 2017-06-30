module ShortCut.Test.Eval where

import Data.ByteString.Lazy.Char8 (pack)
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Eval         (evalFile)
import ShortCut.Core.Types        (CutConfig(..))
import ShortCut.Core.Util         (mkTestGroup)
import System.FilePath.Posix      (replaceExtension, takeBaseName, (</>))
import System.IO.Silently         (silence)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsFile, goldenVsString, findByExtension)
import System.Process             (cwd, readCreateProcess, shell)
import Prelude             hiding (writeFile)
import Data.String.Utils          (replace)
import System.Directory           (getCurrentDirectory)

testDirs :: [String]
testDirs = 
  [ "tests/blast"
  , "tests/crb"
  , "tests/fasta"
  , "tests/repeat"
  , "tests/lists"
  , "tests/vars"
  , "tests/math"
  ]

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "Interpret" [goldenScripts, goldenScriptTrees]

-- TODO i guess now is the time to make Compile use cfgTmpDir from CutConfig?
goldenScript :: CutConfig -> FilePath -> FilePath -> TestTree
goldenScript cfg cut gld = goldenVsFile name gld res act
  where
    name = takeBaseName cut
    cfg' = cfg { cfgScript = Just cut, cfgTmpDir = (cfgTmpDir cfg </> name) }
    res  = (cfgTmpDir cfg' </> "result")
    act  = silence $ evalFile cfg'

goldenScripts :: CutConfig -> IO TestTree
goldenScripts cfg = do
  tDirs  <- mapM getDataFileName testDirs
  cuts   <- fmap concat $ mapM (findByExtension [".cut"]) tDirs
  let gFiles = map (\s -> replaceExtension s "result") cuts
      gTests = map (\(s,g) -> goldenScript cfg s g) (zip cuts gFiles)
  return $ testGroup "produce expected results" gTests

-- Line goldenScript, except it tests that the proper tree of tmpfiles was
-- created instead of the proper result.  Note that the tree file is unrelated
-- to the TestTree.
-- TODO ensure that tree is installed with coreutils
goldenScriptTree :: CutConfig -> FilePath -> FilePath -> TestTree
goldenScriptTree cfg cut tre = goldenVsString name tre act
  where
    name = takeBaseName cut
    cfg' = cfg { cfgScript = Just cut, cfgTmpDir = (cfgTmpDir cfg </> name) }
    -- crbblast folder is excluded because blast isn't deterministic
    -- TODO list of plugin folders to exlude, or certain filetypes in them only
    cmd  = (shell $ "tree -I 'crbblast'") { cwd = Just $ cfgTmpDir cfg' }
    act  = do
             silence $ evalFile cfg'
             out <- readCreateProcess cmd ""
             wd  <- getCurrentDirectory
             return $ pack $ fixWorkingDir wd out

-- symlinks from the cache -> elsewhere only work when absolute,
-- but that makes them nondeterministic when tests are run from random tmpdirs.
-- this fixes it by editing test output to include a generic $PWD
-- TODO does this misleadinly imply shell interpolation in tree files?
fixWorkingDir :: String -> String -> String
fixWorkingDir wd = replace wd "$PWD"

goldenScriptTrees :: CutConfig -> IO TestTree
goldenScriptTrees cfg = do
  tDirs  <- mapM getDataFileName testDirs
  cuts   <- fmap concat $ mapM (findByExtension [".cut"]) tDirs
  let gFiles = map (\s -> replaceExtension s "tree") cuts
      gTests = map (\(s,g) -> goldenScriptTree cfg s g) (zip cuts gFiles)
  return $ testGroup "create expected tmpfiles" gTests
