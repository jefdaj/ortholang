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
import Detourrr.Core.Types        (RrrConfig(..), HashedSeqIDsRef)
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

nonDeterministicRrr :: FilePath -> Bool
nonDeterministicRrr path = testDir `elem` badDirs
  where
    testDir = (takeFileName . takeDirectory) path
    -- TODO will regular blast be nondeterministic at large scales too?
    badDirs = ["blastcrb", "blastrbh", "blasthits", "plots"] -- TODO blast? blastdb?

getTestScripts :: IO [FilePath]
getTestScripts = do
  testDir  <- getDataFileName "tests"
  testRrrs <- findByExtension [".rrr"] testDir
  return testRrrs

goldenDiff :: String -> FilePath -> IO ByteString -> TestTree
goldenDiff name file action = goldenVsStringDiff name fn file action
  where
    -- this is taken from the Tasty docs
    fn ref new = ["diff", "-u", ref, new]

mkOutTest :: RrrConfig -> Locks -> HashedSeqIDsRef -> FilePath -> TestTree
mkOutTest cfg ref ids gld = goldenDiff desc gld scriptAct
  where
    -- TODO put toGeneric back here? or avoid paths in output altogether?
    scriptAct = runRrr cfg ref ids >>= return . pack -- . toGeneric cfg
    rrr  = takeBaseName gld <.> "rrr"
    desc = rrr ++ " prints expected output"

mkTreeTest :: RrrConfig -> Locks -> HashedSeqIDsRef -> FilePath -> TestTree
mkTreeTest cfg ref ids t = goldenDiff desc t treeAct
  where
    -- Note that Test/Repl.hs also has a matching tree command
    -- TODO refactor them to come from the same fn
    desc = takeFileName t ++ " creates expected tmpfiles"
    sedCmd  = "sed 's/lines\\/.*/lines\\/\\.\\.\\./g'"
    treeCmd = (shell $ "tree -aI '*.lock|*.database|*.log|*.tmp|*.html|lines' | " ++ sedCmd)
                { cwd = Just $ cfgTmpDir cfg }
    treeAct = do
      _ <- runRrr cfg ref ids
      out <- readCreateProcess treeCmd ""
      -- sometimes useful for debugging tests:
      -- writeFile "/tmp/latest.txt" out
      return $ pack $ toGeneric cfg out

-- TODO use safe writes here
mkTripTest :: RrrConfig -> Locks -> HashedSeqIDsRef -> TestTree
mkTripTest cfg ref ids = goldenDiff desc tripShow tripAct
  where
    desc = takeBaseName tripRrr ++ " unchanged by round-trip to file"
    tripRrr   = cfgTmpDir cfg <.> "rrr"
    tripShow  = cfgTmpDir cfg <.> "show"
    tripSetup = do
      scr1 <- parseFileIO cfg ref ids $ fromJust $ cfgScript cfg
      writeScript tripRrr scr1
      withWriteLock ref tripShow $ writeFile tripShow $ show scr1
    -- tripAct = withWriteLock'IO (cfgTmpDir cfg <.> "lock") $ do
    tripAct = do
      -- _    <- withFileLock (cfgTmpDir cfg) tripSetup
      _ <- tripSetup
      scr2 <- parseFileIO cfg ref ids tripRrr
      return $ pack $ show scr2

-- test that no absolute paths snuck into the tmpfiles
mkAbsTest :: RrrConfig -> Locks -> HashedSeqIDsRef -> IO [TestTree]
mkAbsTest cfg ref ids = testSpecs $ it desc $
  absGrep `shouldReturn` ""
  where
    path = takeFileName $ cfgTmpDir cfg
    desc = path ++ " tmpfiles free of absolute paths"
    absArgs = [cfgTmpDir cfg, cfgTmpDir cfg </> "exprs", "-R"]
    absGrep = do
      _ <- runRrr cfg ref ids
      (_, out, err) <- readProcessWithExitCode "grep" absArgs ""
      return $ toGeneric cfg $ out ++ err

runRrr :: RrrConfig -> Locks -> HashedSeqIDsRef -> IO String
runRrr cfg ref ids =  do
  delay 1000000 -- wait 1 second so we don't capture output from tasty
  (out, ()) <- hCapture [stdout, stderr] $ evalFile stdout cfg ref ids
  result <- doesFileExist $ cfgTmpDir cfg </> "vars" </> "result"
  when (not result) (fail out)
  return $ toGeneric cfg out

mkScriptTests :: (FilePath, FilePath, (Maybe FilePath))
              -> RrrConfig -> Locks -> HashedSeqIDsRef -> IO TestTree
mkScriptTests (rrr, gld, mtre) cfg ref ids = do
  absTests <- mkAbsTest cfg' ref ids -- just one, but comes as a list
  return $ testGroup name $ otherTests ++ absTests
  where
    name       = takeFileName rrr
    cfg'       = cfg { cfgScript = Just rrr, cfgTmpDir = (cfgTmpDir cfg </> name) }
    otherTests = [mkTripTest cfg' ref ids, mkOutTest cfg' ref ids gld] ++ genTests
    genTests   = case mtre of
                   Just t  -> [mkTreeTest cfg' ref ids t]
                   Nothing -> []

mkTests :: RrrConfig -> Locks -> HashedSeqIDsRef -> IO TestTree
mkTests cfg ref ids = do
  rrrs <- getTestScripts
  let outs     = map findOutFile  rrrs
      mtrees   = map findTreeFile rrrs
      triples  = zip3 rrrs outs mtrees
      groups   = map mkScriptTests triples
  mkTestGroup cfg ref ids "interpret test scripts" groups
  where
    findOutFile  c = replaceExtension c "out"
    findTreeFile c = if nonDeterministicRrr c
      then Nothing
      else Just $ replaceExtension c "tree"
