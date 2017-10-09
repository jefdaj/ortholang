module ShortCut.Test.Scripts where

-- TODO if using paths to make paths, is anything not deterministic??

import Data.ByteString.Lazy.Char8 (pack, ByteString)
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Eval         (evalFile)
import ShortCut.Core.Paths        (toGeneric)
import ShortCut.Core.Types        (CutConfig(..))
import ShortCut.Core.Util         (mkTestGroup)
import System.FilePath.Posix      (replaceExtension, takeBaseName, takeDirectory,
                                   takeFileName, (</>), (<.>))
import System.IO.Silently         (hSilence)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsStringDiff, findByExtension)
import Test.Hspec                 (it)
import Test.Tasty.Hspec           (testSpecs, shouldReturn)
import System.Process             (cwd, readCreateProcess, readProcessWithExitCode, shell)
import Prelude             hiding (writeFile)
import System.IO                  (stdout, stderr, writeFile)
import Data.Default.Class         (Default(def))
import qualified Control.Monad.TaggedException as Exception (handle)
import System.IO.LockFile -- TODO only some of it
import ShortCut.Core.Parse            (parseFileIO)
import ShortCut.Core.Pretty       (writeScript)
import Data.Maybe                     (fromJust)
-- import Control.Monad.Trans (liftIO)

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

goldenDiff :: String -> FilePath -> IO ByteString -> TestTree
goldenDiff name file action = goldenVsStringDiff name fn file action
  where
    -- this is taken from the Tasty docs
    fn ref new = ["diff", "-u", ref, new]

mkScriptTest :: CutConfig -> FilePath -> TestTree
mkScriptTest cfg gld = goldenDiff "result is correct" gld scriptAct
  where
    scriptRes = (cfgTmpDir cfg </> "vars" </> "result")
    scriptAct = do
      runCut cfg
      res <- readFile scriptRes
      return $ pack $ toGeneric cfg res

mkTreeTest :: CutConfig -> FilePath -> TestTree
mkTreeTest cfg t = goldenDiff "creates expected tmpfiles" t treeAct
  where
    treeCmd = (shell $ "tree") { cwd = Just $ cfgTmpDir cfg }
    treeAct = do
      runCut cfg
      out <- readCreateProcess treeCmd ""
      return $ pack $ toGeneric cfg out

mkTripTest :: CutConfig -> TestTree
mkTripTest cfg = goldenDiff "unchanged by round-trip to file" tripShow tripAct
  where
    tripCut   = cfgTmpDir cfg <.> "cut"
    tripShow  = cfgTmpDir cfg <.> "show"
    tripSetup = do
      scr1 <- parseFileIO cfg $ fromJust $ cfgScript cfg
      writeScript tripCut scr1
      writeFile tripShow $ show scr1
    tripAct = do
      withLock cfg tripSetup
      scr2 <- parseFileIO cfg tripCut
      return $ pack $ show scr2

-- test that no absolute paths snuck into the tmpfiles
mkAbsTest :: CutConfig -> IO [TestTree]
mkAbsTest cfg = testSpecs $ it "tmpfiles free of absolute paths" $
  absGrep `shouldReturn` ""
  where
    absArgs = [cfgTmpDir cfg, cfgTmpDir cfg </> "exprs", "-R"]
    absGrep = do
      runCut cfg
      (_, out, err) <- readProcessWithExitCode "grep" absArgs ""
      return $ out ++ err

runCut :: CutConfig -> IO ()
runCut cfg = withLock cfg $ hSilence [stdout, stderr] $ evalFile stdout cfg

-- TODO is the IO return type needed?
mkScriptTests :: (FilePath, FilePath, (Maybe FilePath)) -> CutConfig -> IO TestTree
mkScriptTests (cut, gld, mtre) cfg = do
  absTests <- mkAbsTest cfg' -- just one, but comes as a list
  return $ testGroup name $ otherTests ++ absTests
  where
    name       = takeBaseName cut
    cfg'       = cfg { cfgScript = Just cut, cfgTmpDir = (cfgTmpDir cfg </> name) }
    otherTests = [mkTripTest cfg', mkScriptTest cfg' gld] ++ genTests
    genTests   = case mtre of
                   Just t  -> [mkTreeTest cfg' t]
                   Nothing -> []

mkTests :: CutConfig -> IO TestTree
mkTests cfg = do
  cuts <- getTestCuts
  let results  = map findResFile  cuts
      mtrees   = map findTreeFile cuts
      triples  = zip3 cuts results mtrees
      groups   = map mkScriptTests triples
  mkTestGroup cfg "interpret test scripts" groups
  where
    findResFile  c = replaceExtension c "result"
    findTreeFile c = if nonDeterministicCut c
      then Nothing
      else Just $ replaceExtension c "tree"
