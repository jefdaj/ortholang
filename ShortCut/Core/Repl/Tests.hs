module ShortCut.Core.Repl.Tests where

-- TODO these are actually the interpreter/file tests, aren't they?
--      move them over there and write REPL tests here

-- This module tests only the REPL, not:
--   the interpreter functions called
--   the scripts called
--   the monad functions called
-- TODO write separate tests for each of those!

import ShortCut.Core.Types (CutConfig)
import ShortCut.Core.Util  (mkTestGroup)
import Test.Tasty          (TestTree)

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "Repl" []

-- import Test.Tasty.Golden (goldenVsFile, findByExtension)
-- import System.FilePath.Posix (replaceExtension, takeBaseName, (</>))
-- import Paths_ShortCut (getDataFileName)
-- import ShortCut.Core.Interpret (eFile)
-- import ShortCut.Core.Types (CutConfig(..))
-- -- import Debug.Trace -- TODO remove before releasing!
-- import System.IO.Silently (silence)
-- 
-- mkTests :: CutConfig -> IO TestTree
-- mkTests cfg = mkTestGroup cfg "Repl" [goldenScripts]
-- 
-- testDir :: IO FilePath
-- testDir = getDataFileName "ShortCut/Core/Repl/tests"
-- 
-- -- TODO need to evaluate the script in a tmpdir, and pass tmpdir/result
-- --      to goldenVsFile
-- -- TODO might as well name the test by the basename? if it needs a name
-- -- TODO use System.FilePath.Posix for manipulations
-- -- TODO i guess now is the time to make Compile use cfgTmpDir from CutConfig?
-- goldenScript :: CutConfig -> FilePath -> FilePath -> TestTree
-- goldenScript cfg cut gld = goldenVsFile name gld res act
--   where
--     name = takeBaseName cut
--     cfg' = cfg { cfgScript = Just cut, cfgTmpDir = (cfgTmpDir cfg </> name) }
--     res  = (cfgTmpDir cfg' </> "result")
--     act  = silence $ eFile cfg'
-- 
-- goldenScripts :: CutConfig -> IO TestTree
-- goldenScripts cfg = do
--   tDir <- testDir
--   cuts <- findByExtension [".cut"] tDir
--   let gFiles = map (\s -> replaceExtension s "golden") cuts
--       gTests = map (\(s,g) -> goldenScript cfg s g) (zip cuts gFiles)
--   return $ testGroup "interprets test scripts" gTests
