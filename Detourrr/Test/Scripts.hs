module Detourrr.Test.Scripts where

import Prelude hiding (writeFile)

import Control.Concurrent.Thread.Delay (delay)
import Control.Monad              (when)
import Data.ByteString.Lazy.Char8 (pack, ByteString)
import Data.Maybe                 (fromJust)
import Paths_Detourrr             (getDataFileName)
import Detourrr.Core.Eval         (evalFile)
import Detourrr.Core.Parse        (parseFileIO)
import Detourrr.Core.Paths        (toGeneric)
import Detourrr.Core.Pretty       (writeScript)
import Detourrr.Core.Types        (DtrConfig(..), HashedSeqIDsRef)
import Detourrr.Core.Locks        (Locks, withWriteLock)
import Detourrr.Test.Repl         (mkTestGroup)
import System.Directory           (doesFileExist)
import System.FilePath.Posix      (replaceExtension, takeBaseName, takeDirectory,
                                   takeFileName, (</>), (<.>))
import System.IO                  (stdout, stderr, writeFile)
import System.IO.Silently         (hCapture)
import System.Process             (readCreateProcess, readProcessWithExitCode,
                                   cwd, shell)
import Test.Hspec                 (it)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsStringDiff, findByExtension)
import Test.Tasty.Hspec           (testSpecs, shouldReturn)

nonDeterministicDtr :: FilePath -> Bool
nonDeterministicDtr path = testDir `elem` badDirs
  where
    testDir = (takeFileName . takeDirectory) path
    -- TODO will regular blast be nondeterministic at large scales too?
    badDirs = ["blastcrb", "blastrbh", "blasthits", "plots"] -- TODO blast? blastdb?

getTestScripts :: IO [FilePath]
getTestScripts = do
  exDir    <- getDataFileName "data"
  testDtrs <- findByExtension [".dtr"] exDir
  return testDtrs

goldenDiff :: String -> FilePath -> IO ByteString -> TestTree
goldenDiff name file action = goldenVsStringDiff name fn file action
  where
    -- this is taken from the Tasty docs
    fn ref new = ["diff", "-u", ref, new]

mkOutTest :: DtrConfig -> Locks -> HashedSeqIDsRef -> FilePath -> TestTree
mkOutTest cfg ref ids gld = goldenDiff desc gld scriptAct
  where
    -- TODO put toGeneric back here? or avoid paths in output altogether?
    scriptAct = runDtr cfg ref ids >>= return . pack -- . toGeneric cfg
    dtr  = takeBaseName gld <.> "dtr"
    desc = dtr ++ " prints expected output"

mkTreeTest :: DtrConfig -> Locks -> HashedSeqIDsRef -> FilePath -> TestTree
mkTreeTest cfg ref ids t = goldenDiff desc t treeAct
  where
    -- Note that Test/Repl.hs also has a matching tree command
    -- TODO refactor them to come from the same fn
    desc = takeFileName t ++ " creates expected tmpfiles"
    sedCmd  = "sed 's/lines\\/.*/lines\\/\\.\\.\\./g'"
    treeCmd = (shell $ "tree -aI '*.lock|*.database|*.log|*.tmp|*.html|lines' | " ++ sedCmd)
                { cwd = Just $ cfgTmpDir cfg }
    treeAct = do
      _ <- runDtr cfg ref ids
      out <- readCreateProcess treeCmd ""
      -- sometimes useful for debugging tests:
      -- writeFile "/tmp/latest.txt" out
      return $ pack $ toGeneric cfg out

-- TODO use safe writes here
mkTripTest :: DtrConfig -> Locks -> HashedSeqIDsRef -> TestTree
mkTripTest cfg ref ids = goldenDiff desc tripShow tripAct
  where
    desc = takeBaseName tripDtr ++ " unchanged by round-trip to file"
    tripDtr   = cfgTmpDir cfg <.> "dtr"
    tripShow  = cfgTmpDir cfg <.> "show"
    tripSetup = do
      scr1 <- parseFileIO cfg ref ids $ fromJust $ cfgScript cfg
      writeScript tripDtr scr1
      withWriteLock ref tripShow $ writeFile tripShow $ show scr1
    -- tripAct = withWriteLock'IO (cfgTmpDir cfg <.> "lock") $ do
    tripAct = do
      -- _    <- withFileLock (cfgTmpDir cfg) tripSetup
      _ <- tripSetup
      scr2 <- parseFileIO cfg ref ids tripDtr
      return $ pack $ show scr2

-- test that no absolute paths snuck into the tmpfiles
mkAbsTest :: DtrConfig -> Locks -> HashedSeqIDsRef -> IO [TestTree]
mkAbsTest cfg ref ids = testSpecs $ it desc $
  absGrep `shouldReturn` ""
  where
    path = takeFileName $ cfgTmpDir cfg
    desc = path ++ " tmpfiles free of absolute paths"
    absArgs = [cfgTmpDir cfg, cfgTmpDir cfg </> "exprs", "-R"]
    absGrep = do
      _ <- runDtr cfg ref ids
      (_, out, err) <- readProcessWithExitCode "grep" absArgs ""
      return $ toGeneric cfg $ out ++ err

runDtr :: DtrConfig -> Locks -> HashedSeqIDsRef -> IO String
runDtr cfg ref ids =  do
  delay 1000000 -- wait 1 second so we don't capture output from tasty
  (out, ()) <- hCapture [stdout, stderr] $ evalFile stdout cfg ref ids
  result <- doesFileExist $ cfgTmpDir cfg </> "vars" </> "result"
  when (not result) (fail out)
  return $ toGeneric cfg out

mkScriptTests :: (FilePath, FilePath, (Maybe FilePath))
              -> DtrConfig -> Locks -> HashedSeqIDsRef -> IO TestTree
mkScriptTests (dtr, gld, mtre) cfg ref ids = do
  absTests <- mkAbsTest cfg' ref ids -- just one, but comes as a list
  return $ testGroup name $ otherTests ++ absTests
  where
    name       = takeFileName dtr
    cfg'       = cfg { cfgScript = Just dtr, cfgTmpDir = (cfgTmpDir cfg </> name) }
    otherTests = [mkTripTest cfg' ref ids, mkOutTest cfg' ref ids gld] ++ genTests
    genTests   = case mtre of
                   Just t  -> [mkTreeTest cfg' ref ids t]
                   Nothing -> []

mkTests :: DtrConfig -> Locks -> HashedSeqIDsRef -> IO TestTree
mkTests cfg ref ids = do
  dtrs <- getTestScripts
  let outs     = map findOutFile  dtrs
      mtrees   = map findTreeFile dtrs
      triples  = zip3 dtrs outs mtrees
      groups   = map mkScriptTests triples
  mkTestGroup cfg ref ids "interpret test scripts" groups
  where
    findOutFile  c = replaceExtension c "out"
    findTreeFile c = if nonDeterministicDtr c
      then Nothing
      else Just $ replaceExtension c "tree"
