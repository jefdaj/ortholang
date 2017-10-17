module ShortCut.Test.Scripts where

import Prelude hiding (writeFile)
import qualified Control.Monad.TaggedException as TE

import Control.Concurrent.Thread.Delay (delay)
import Control.Monad              (when)
import Data.ByteString.Lazy.Char8 (pack, ByteString)
import Data.Default.Class         (Default(def))
import Data.Maybe                 (fromJust)
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Eval         (evalFile)
import ShortCut.Core.Parse        (parseFileIO)
import ShortCut.Core.Paths        (toGeneric)
import ShortCut.Core.Pretty       (writeScript)
import ShortCut.Core.Types        (CutConfig(..))
import ShortCut.Core.Util         (mkTestGroup)
import System.Directory           (doesFileExist)
import System.FilePath.Posix      (replaceExtension, takeBaseName, takeDirectory,
                                   takeFileName, (</>), (<.>))
import System.IO                  (stdout, stderr, writeFile)
import System.IO.LockFile         (withLockFile)
import System.IO.Silently         (hCapture)
import System.Process             (readCreateProcess, readProcessWithExitCode,
                                   cwd, shell)
import Test.Hspec                 (it)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsStringDiff, findByExtension)
import Test.Tasty.Hspec           (testSpecs, shouldReturn)

nonDeterministicCut :: FilePath -> Bool
nonDeterministicCut path = testDir `elem` badDirs
  where
    testDir = (takeFileName . takeDirectory) path
    -- TODO will regular blast be nondeterministic at large scales too?
    badDirs = ["blastcrb", "each"]

getTestCuts :: IO [FilePath]
getTestCuts = do
  testDir  <- getDataFileName "tests"
  testCuts <- findByExtension [".cut"] testDir
  return testCuts

-- TODO any particular corner cases to be aware of? (what if inturrupted?)
withLock :: CutConfig -> IO a -> IO a
withLock cfg act = withErr $ withLockFile def started act
  where
    withErr = TE.handle $ fail . ("Locking failed with: " ++) . show
    started = cfgTmpDir cfg <.> "lock"

goldenDiff :: String -> FilePath -> IO ByteString -> TestTree
goldenDiff name file action = goldenVsStringDiff name fn file action
  where
    -- this is taken from the Tasty docs
    fn ref new = ["diff", "-u", ref, new]

mkOutTest :: CutConfig -> FilePath -> TestTree
mkOutTest cfg gld = goldenDiff "prints expected output" gld scriptAct
  where
    -- TODO put toGeneric back here? or avoid paths in output altogether?
    scriptAct = runCut cfg >>= return . pack

mkTreeTest :: CutConfig -> FilePath -> TestTree
mkTreeTest cfg t = goldenDiff "creates expected tmpfiles" t treeAct
  where
    treeCmd = (shell $ "tree") { cwd = Just $ cfgTmpDir cfg }
    treeAct = do
      _ <- runCut cfg
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
      _    <- withLock cfg tripSetup
      scr2 <- parseFileIO cfg tripCut
      return $ pack $ show scr2

-- test that no absolute paths snuck into the tmpfiles
mkAbsTest :: CutConfig -> IO [TestTree]
mkAbsTest cfg = testSpecs $ it "tmpfiles free of absolute paths" $
  absGrep `shouldReturn` ""
  where
    absArgs = [cfgTmpDir cfg, cfgTmpDir cfg </> "exprs", "-R"]
    absGrep = do
      _ <- runCut cfg
      (_, out, err) <- readProcessWithExitCode "grep" absArgs ""
      return $ out ++ err

-- Without the delays, Tasty messages sometimes get captured in the output
-- If it still happens, try TASTY_HIDE_SUCCESSES=True or TASTY_NUM_THREADS=1.
-- hFlush does not seem to help.
runCut :: CutConfig -> IO String
runCut cfg = withLock cfg $ do
  delay 100
  (out, ()) <- hCapture [stdout, stderr] $ evalFile stdout cfg
  delay 100
  result <- doesFileExist $ cfgTmpDir cfg </> "vars" </> "result"
  when (not result) (fail "script failed")
  return out

-- TODO is the IO return type needed?
mkScriptTests :: (FilePath, FilePath, (Maybe FilePath)) -> CutConfig -> IO TestTree
mkScriptTests (cut, gld, mtre) cfg = do
  absTests <- mkAbsTest cfg' -- just one, but comes as a list
  return $ testGroup name $ otherTests ++ absTests
  where
    name       = takeBaseName cut
    cfg'       = cfg { cfgScript = Just cut, cfgTmpDir = (cfgTmpDir cfg </> name) }
    otherTests = [mkTripTest cfg', mkOutTest cfg' gld] ++ genTests
    genTests   = case mtre of
                   Just t  -> [mkTreeTest cfg' t]
                   Nothing -> []

mkTests :: CutConfig -> IO TestTree
mkTests cfg = do
  cuts <- getTestCuts
  let outs     = map findOutFile  cuts
      mtrees   = map findTreeFile cuts
      triples  = zip3 cuts outs mtrees
      groups   = map mkScriptTests triples
  mkTestGroup cfg "interpret test scripts" groups
  where
    findOutFile  c = replaceExtension c "out"
    findTreeFile c = if nonDeterministicCut c
      then Nothing
      else Just $ replaceExtension c "tree"
