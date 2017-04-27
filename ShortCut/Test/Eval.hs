module ShortCut.Test.Eval where

import Data.ByteString.Lazy.Char8 (pack, writeFile)
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

-- import Debug.Trace

-- import Debug.Trace -- TODO remove before releasing!
-- import Test.Tasty.Golden
-- import System.FilePath.Glob (globDir1)
-- import Paths_ShortCut (getDataFileName)

-- TODO import qualified ShortCut.Core.Interpret.Parse.Tests as P
-- import ShortCut.Core.Interpret.Compile
-- import ShortCut.Core.Interpret.Parse
-- import ShortCut.Core.Interpret.ParseSpec
-- import ShortCut.Core.Types

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "Interpret" [goldenScripts, goldenScriptTrees]

-- TODO need to evaluate the script in a tmpdir, and pass tmpdir/result
--      to goldenVsFile
-- TODO might as well name the test by the basename? if it needs a name
-- TODO use System.FilePath.Posix for manipulations
-- TODO i guess now is the time to make Compile use cfgTmpDir from CutConfig?
-- TODO include goldenTree here too (should pass both at once)
goldenScript :: CutConfig -> FilePath -> FilePath -> TestTree
goldenScript cfg cut gld = goldenVsFile name gld res act
  where
    name = takeBaseName cut
    cfg' = cfg { cfgScript = Just cut, cfgTmpDir = (cfgTmpDir cfg </> name) }
    res  = (cfgTmpDir cfg' </> "result")
    act  = silence $ evalFile cfg'

goldenScripts :: CutConfig -> IO TestTree
goldenScripts cfg = do
  tDir <- getDataFileName "tests/math"
  gFiles <- findByExtension [".result"] tDir
  let cuts   = map (\s -> replaceExtension s "cut") gFiles
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

-- TODO shit, something about the result numbers is nondeterministic; need to fix to pass these!
--      probably because it needs to not rely on the path *to* the shortcut dir, only inside it
goldenScriptTrees :: CutConfig -> IO TestTree
goldenScriptTrees cfg = do
  tDir <- getDataFileName "tests/math"
  gFiles <- findByExtension [".tree"] tDir
  let cuts   = map (\s -> replaceExtension s "cut") gFiles
      gTests = map (\(s,g) -> goldenScriptTree cfg s g) (zip cuts gFiles)
  return $ testGroup "create expected tmpfiles" gTests
