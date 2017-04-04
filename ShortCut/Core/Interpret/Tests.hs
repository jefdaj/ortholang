module ShortCut.Core.Interpret.Tests where

import Paths_ShortCut          (getDataFileName)
import ShortCut.Core.Interpret (eFile)
import ShortCut.Core.Types     (CutConfig(..))
import ShortCut.Core.Util      (mkTestGroup)
import System.FilePath.Posix   (replaceExtension, takeBaseName, (</>))
import System.IO.Silently      (silence)
import Test.Tasty              (TestTree, testGroup)
import Test.Tasty.Golden       (goldenVsFile, findByExtension)

-- import Debug.Trace -- TODO remove before releasing!
-- import Test.Tasty.Golden
-- import System.FilePath.Glob (globDir1)
-- import Paths_ShortCut (getDataFileName)

-- TODO import qualified ShortCut.Core.Interpret.Parse.Tests as P
-- import ShortCut.Core.Interpret.Compile
-- import ShortCut.Core.Interpret.Parse
-- import ShortCut.Core.Interpret.ParseSpec
-- import ShortCut.Core.Types

testDir :: IO FilePath
testDir = getDataFileName "ShortCut/Core/Interpret/tests"

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "Interpret" [goldenScripts]

-- TODO need to evaluate the script in a tmpdir, and pass tmpdir/result
--      to goldenVsFile
-- TODO might as well name the test by the basename? if it needs a name
-- TODO use System.FilePath.Posix for manipulations
-- TODO i guess now is the time to make Compile use cfgTmpDir from CutConfig?
goldenScript :: CutConfig -> FilePath -> FilePath -> TestTree
goldenScript cfg cut gld = goldenVsFile name gld res act
  where
    name = takeBaseName cut
    cfg' = cfg { cfgScript = Just cut, cfgTmpDir = (cfgTmpDir cfg </> name) }
    res  = (cfgTmpDir cfg' </> "result")
    act  = silence $ eFile cfg'

goldenScripts :: CutConfig -> IO TestTree
goldenScripts cfg = do
  tDir <- testDir
  cuts <- findByExtension [".cut"] tDir
  let gFiles = map (\s -> replaceExtension s "golden") cuts
      gTests = map (\(s,g) -> goldenScript cfg s g) (zip cuts gFiles)
  return $ testGroup "interprets test scripts" gTests
