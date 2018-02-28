module ShortCut.Test.Scripts where

-- TODO go with filelock here too?

import Prelude hiding (writeFile)
-- import qualified Control.Monad.TaggedException as TE

import Control.Concurrent.Thread.Delay (delay)
import Control.Monad              (when)
import Data.ByteString.Lazy.Char8 (pack, ByteString)
-- import Data.Default.Class         (Default(def))
import Data.Maybe                 (fromJust)
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Eval         (evalFile)
import ShortCut.Core.Parse        (parseFileIO)
import ShortCut.Core.Paths        (toGeneric)
import ShortCut.Core.Pretty       (writeScript)
import ShortCut.Core.Types        (CutConfig(..), CutLocks)
import ShortCut.Test.Repl         (mkTestGroup)
import System.Directory           (doesFileExist)
import System.FilePath.Posix      (replaceExtension, takeBaseName, takeDirectory,
                                   takeFileName, (</>), (<.>))
import System.IO                  (stdout, stderr, writeFile)
-- import System.IO.LockFile         (withFileLock, LockingParameters(..),
                                   -- RetryStrategy(..))
import System.IO.Silently         (hCapture)
import System.Process             (readCreateProcess, readProcessWithExitCode,
                                   cwd, shell)
import Test.Hspec                 (it)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsStringDiff, findByExtension)
import Test.Tasty.Hspec           (testSpecs, shouldReturn)
-- import System.FileLock            (withFileLock, SharedExclusive(..))
import Data.IORef                     (IORef)

nonDeterministicCut :: FilePath -> Bool
nonDeterministicCut path = testDir `elem` badDirs
  where
    testDir = (takeFileName . takeDirectory) path
    -- TODO will regular blast be nondeterministic at large scales too?
    badDirs = ["blastcrb", "blastrbh", "blasthits"] -- TODO blast? blastdb?

getTestCuts :: IO [FilePath]
getTestCuts = do
  testDir  <- getDataFileName "tests"
  testCuts <- findByExtension [".cut"] testDir
  return testCuts

-- TODO any particular corner cases to be aware of? (what if inturrupted?)
-- withLock :: CutConfig -> IO a -> IO a
-- withLock cfg act = handler $ withFileLock params path act
--   where
--     path    = cfgTmpDir cfg <.> "lock"
--     handler = TE.handle $ fail . ("Locking failed with: " ++) . show
--     params  = LockingParameters
--       { retryToAcquireLock = Indefinitely
--       , sleepBetweenRetires = 1000000 -- 1 second in microseconds
--       }

goldenDiff :: String -> FilePath -> IO ByteString -> TestTree
goldenDiff name file action = goldenVsStringDiff name fn file action
  where
    -- this is taken from the Tasty docs
    fn ref new = ["diff", "-u", ref, new]

mkOutTest :: CutConfig -> IORef CutLocks -> FilePath -> TestTree
mkOutTest cfg ref gld = goldenDiff "prints expected output" gld scriptAct
  where
    -- TODO put toGeneric back here? or avoid paths in output altogether?
    scriptAct = runCut cfg ref >>= return . pack

mkTreeTest :: CutConfig -> IORef CutLocks -> FilePath -> TestTree
mkTreeTest cfg ref t = goldenDiff "creates expected tmpfiles" t treeAct
  where
    -- Note that Test/Repl.hs also has a matching tree command
    -- TODO refactor them to come from the same fn
    treeCmd = (shell "tree -aI '*.lock|*.database'") { cwd = Just $ cfgTmpDir cfg }
    treeAct = do
      _ <- runCut cfg ref
      out <- readCreateProcess treeCmd ""
      return $ pack $ toGeneric cfg out

mkTripTest :: CutConfig -> IORef CutLocks -> TestTree
mkTripTest cfg ref = goldenDiff "unchanged by round-trip to file" tripShow tripAct
  where
    tripCut   = cfgTmpDir cfg <.> "cut"
    tripShow  = cfgTmpDir cfg <.> "show"
    tripSetup = do
      scr1 <- parseFileIO cfg ref $ fromJust $ cfgScript cfg
      writeScript tripCut scr1
      writeFile tripShow $ show scr1
    -- tripAct = withLockIO (cfgTmpDir cfg <.> "lock") $ do
    tripAct = do
      -- _    <- withFileLock (cfgTmpDir cfg) tripSetup
      _ <- tripSetup
      scr2 <- parseFileIO cfg ref tripCut
      return $ pack $ show scr2

-- test that no absolute paths snuck into the tmpfiles
mkAbsTest :: CutConfig -> IORef CutLocks -> IO [TestTree]
mkAbsTest cfg ref = testSpecs $ it "tmpfiles free of absolute paths" $
  absGrep `shouldReturn` ""
  where
    absArgs = [cfgTmpDir cfg, cfgTmpDir cfg </> "exprs", "-R"]
    absGrep = do
      _ <- runCut cfg ref
      (_, out, err) <- readProcessWithExitCode "grep" absArgs ""
      return $ out ++ err

-- Without the delays, Tasty messages sometimes get captured in the output. If
-- it still happens try TASTY_HIDE_SUCCESSES=True, not hFlush (doesn't help) or
-- TASTY_NUM_THREADS=1 (actually seems to make it worse).
runCut :: CutConfig -> IORef CutLocks -> IO String
-- runCut cfg = withFileLock (cfgTmpDir cfg) $ do
-- runCut cfg = do
-- runCut cfg = withLockIO (cfgTmpDir cfg <.> "lock") $ do
runCut cfg ref =  do
  -- delay 50000; hFlush stdout; hFlush stderr; delay 50000 -- 1 second total
  delay 100000 -- 1 second
  (out, ()) <- hCapture [stdout, stderr] $ evalFile stdout cfg ref
  delay 100000 -- 1 second
  result <- doesFileExist $ cfgTmpDir cfg </> "vars" </> "result"
  when (not result) (fail out)
  return out

-- TODO is the IO return type needed?
-- TODO FIGURE OUT HOW TO HAVE EACH STEP LOCK THE DIR IF IT ISN'T YET!
-- TODO OH, EXCEPT WHAT IF THAT'S WHAT'S MAKING THEM FREEZE? CHECK BOTH IDEAS
--      (LOOKS LIKE THEY NEVER REMOVE THE LOCKFILES? HAHA EXPLAINS OTHER ERRORS? FIX IT)
mkScriptTests :: (FilePath, FilePath, (Maybe FilePath))
              -> CutConfig -> IORef CutLocks -> IO TestTree
mkScriptTests (cut, gld, mtre) cfg ref = do
  absTests <- mkAbsTest cfg' ref -- just one, but comes as a list
  return $ testGroup name $ otherTests ++ absTests
  where
    name       = takeBaseName cut
    cfg'       = cfg { cfgScript = Just cut, cfgTmpDir = (cfgTmpDir cfg </> name) }
    otherTests = [mkTripTest cfg' ref, mkOutTest cfg' ref gld] ++ genTests
    genTests   = case mtre of
                   Just t  -> [mkTreeTest cfg' ref t]
                   Nothing -> []

mkTests :: CutConfig -> IORef CutLocks -> IO TestTree
mkTests cfg ref = do
  cuts <- getTestCuts
  let outs     = map findOutFile  cuts
      mtrees   = map findTreeFile cuts
      triples  = zip3 cuts outs mtrees
      groups   = map mkScriptTests triples
  mkTestGroup cfg ref "interpret test scripts" groups
  where
    findOutFile  c = replaceExtension c "out"
    findTreeFile c = if nonDeterministicCut c
      then Nothing
      else Just $ replaceExtension c "tree"
