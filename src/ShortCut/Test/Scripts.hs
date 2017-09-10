module ShortCut.Test.Scripts where

import Data.ByteString.Lazy.Char8 (pack)
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Eval         (evalFile)
import ShortCut.Core.Types        (CutConfig(..))
import ShortCut.Core.Util         (mkTestGroup)
import System.FilePath.Posix      (replaceExtension, takeBaseName, takeDirectory,
                                   takeFileName, (</>), (<.>))
import System.IO.Silently         (silence)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsString, findByExtension)
import System.Process             (cwd, readCreateProcess, shell)
import Prelude             hiding (writeFile)
import Data.String.Utils          (replace)
import System.IO                  (stdout)
import Data.Default.Class         (Default(def))
import qualified Control.Monad.TaggedException as Exception (handle)
import System.IO.LockFile -- TODO only some of it

-- TODO get rid of as many of these as possible
nonDeterministicCut :: FilePath -> Bool
nonDeterministicCut path = testDir `elem` badDirs
  where
    testDir = (takeFileName . takeDirectory) path
    badDirs = ["blast", "crb"]

getTestCuts :: IO [FilePath]
getTestCuts = do
  testDir  <- getDataFileName "tests"
  testCuts <- findByExtension [".cut"] testDir
  return testCuts

-- TODO any particular corner cases to be aware of? (what if inturrupted?)
withLock :: CutConfig -> IO () -> IO ()
withLock cfg act = handleException $ withLockFile def started act
  where
    started  = cfgTmpDir cfg <.> "lock"
    handleException = Exception.handle
        $ putStrLn . ("Locking failed with: " ++) . show

-- TODO is the IO return type needed?
goldenScriptAndTree :: (FilePath, FilePath, (Maybe FilePath))
                    -> CutConfig -> IO TestTree
goldenScriptAndTree (cut, gld, mtre) cfg = return $ testGroup name bothTests
  where
    name       = takeBaseName cut
    cfg'       = cfg { cfgScript = Just cut, cfgTmpDir = (cfgTmpDir cfg </> name) }
    runCut     = silence $ evalFile stdout cfg'
    scriptRes  = (cfgTmpDir cfg' </> "vars" </> "result")
    scriptAct  = do
                   withLock cfg' runCut
                   res <- readFile scriptRes
                   return $ pack $ replace (cfgTmpDir cfg) "$TMPDIR" res
    treeCmd    = (shell $ "tree") { cwd = Just $ cfgTmpDir cfg' }
    treeAct    = do
                   withLock cfg' runCut
                   out <- readCreateProcess treeCmd ""
                   dir <- fmap (reverse . dropWhile (== '/') . reverse)
                        $ getDataFileName ""
                   -- TODO shouldn't I never need this anyway?
                   return $ pack $ replace dir "$TESTDIR" out
    scriptTest = goldenVsString "result" gld scriptAct
    treeTest t = goldenVsString "tmpfiles" t treeAct
    bothTests  = case mtre of
                   Nothing -> [scriptTest]
                   Just t  -> [scriptTest, treeTest t]

mkTests :: CutConfig -> IO TestTree
mkTests cfg = do
  cuts <- getTestCuts
  let results = map findResFile  cuts
      mtrees  = map findTreeFile cuts
      triples = zip3 cuts results mtrees
      groups  = map goldenScriptAndTree triples
  mkTestGroup cfg "interpret test scripts" groups
  where
    findResFile  c = replaceExtension c "result"
    findTreeFile c = if nonDeterministicCut c
                       then Nothing
                       else Just $ replaceExtension c "tree"
