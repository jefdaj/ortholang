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
  tDirs  <- mapM getDataFileName ["tests/math", "tests/lists", "tests/vars", "tests/repeat", "tests/fasta"]
  cuts   <- fmap concat $ mapM (findByExtension [".cut"]) tDirs
  let gFiles = map (\s -> replaceExtension s "result") cuts
      gTests = map (\(s,g) -> goldenScript cfg s g) (zip cuts gFiles)
  return $ testGroup "produce expected results" gTests

-- Line goldenScript, except it tests that the proper tree of tmpfiles was
-- created instead of the proper result.  Note that the tree file is unrelated
-- to the TestTree.
-- TODO ensure that tree is installed, or use a more basic command!
goldenScriptTree :: CutConfig -> FilePath -> FilePath -> TestTree
goldenScriptTree cfg cut tre = goldenVsString name tre act
  where
    name = takeBaseName cut
    cfg' = cfg { cfgScript = Just cut, cfgTmpDir = (cfgTmpDir cfg </> name) }
    cmd  = (shell "tree") { cwd = Just $ cfgTmpDir cfg' }
    act  = do
             silence $ evalFile cfg'
             out <- readCreateProcess cmd ""
             -- txt <- readFile tre
             -- putStrLn txt
             return $ pack out

goldenScriptTrees :: CutConfig -> IO TestTree
goldenScriptTrees cfg = do
  tDirs  <- mapM getDataFileName ["tests/math", "tests/lists", "tests/vars", "tests/repeat"]
  cuts   <- fmap concat $ mapM (findByExtension [".cut"]) tDirs
  let gFiles = map (\s -> replaceExtension s "tree") cuts
      gTests = map (\(s,g) -> goldenScriptTree cfg s g) (zip cuts gFiles)
  return $ testGroup "create expected tmpfiles" gTests
