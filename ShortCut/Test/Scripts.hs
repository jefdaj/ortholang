module ShortCut.Test.Scripts where

import Prelude hiding (writeFile)

import Control.Concurrent.Thread.Delay (delay)
import Control.Monad              (when)
import Data.ByteString.Lazy.Char8 (pack, ByteString)
import Data.Maybe                 (fromJust)
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Eval         (evalFile)
import ShortCut.Core.Parse        (parseFileIO)
import ShortCut.Core.Paths        (toGeneric)
import ShortCut.Core.Pretty       (writeScript)
import ShortCut.Core.Types        (CutConfig(..), HashedSeqIDsRef)
import ShortCut.Core.Locks        (Locks, withWriteLock)
import ShortCut.Test.Repl         (mkTestGroup)
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

nonDeterministicCut :: FilePath -> Bool
nonDeterministicCut path = testDir `elem` badDirs
  where
    testDir = (takeFileName . takeDirectory) path
    -- TODO will regular blast be nondeterministic at large scales too?
    badDirs = ["blastcrb", "blastrbh", "blasthits", "plots"] -- TODO blast? blastdb?

getTestCuts :: IO [FilePath]
getTestCuts = do
  testDir  <- getDataFileName "tests"
  testCuts <- findByExtension [".cut"] testDir
  return testCuts

goldenDiff :: String -> FilePath -> IO ByteString -> TestTree
goldenDiff name file action = goldenVsStringDiff name fn file action
  where
    -- this is taken from the Tasty docs
    fn ref new = ["diff", "-u", ref, new]

mkOutTest :: CutConfig -> Locks -> HashedSeqIDsRef -> FilePath -> TestTree
mkOutTest cfg ref ids gld = goldenDiff desc gld scriptAct
  where
    -- TODO put toGeneric back here? or avoid paths in output altogether?
    scriptAct = runCut cfg ref ids >>= return . pack -- . toGeneric cfg
    cut  = takeBaseName gld <.> "cut"
    desc = cut ++ " prints expected output"

mkTreeTest :: CutConfig -> Locks -> HashedSeqIDsRef -> FilePath -> TestTree
mkTreeTest cfg ref ids t = goldenDiff desc t treeAct
  where
    -- Note that Test/Repl.hs also has a matching tree command
    -- TODO refactor them to come from the same fn
    desc = takeFileName t ++ " creates expected tmpfiles"
    sedCmd  = "sed 's/lines\\/.*/lines\\/\\.\\.\\./g'"
    treeCmd = (shell $ "tree -aI '*.lock|*.database|*.log|*.tmp|*.html|lines' | " ++ sedCmd)
                { cwd = Just $ cfgTmpDir cfg }
    treeAct = do
      _ <- runCut cfg ref ids
      out <- readCreateProcess treeCmd ""
      -- sometimes useful for debugging tests:
      -- writeFile "/tmp/latest.txt" out
      return $ pack $ toGeneric cfg out

-- TODO use safe writes here
mkTripTest :: CutConfig -> Locks -> HashedSeqIDsRef -> TestTree
mkTripTest cfg ref ids = goldenDiff desc tripShow tripAct
  where
    desc = takeFileName tripCut ++ " unchanged by round-trip to file"
    tripCut   = cfgTmpDir cfg <.> "cut"
    tripShow  = cfgTmpDir cfg <.> "show"
    tripSetup = do
      scr1 <- parseFileIO cfg ref ids $ fromJust $ cfgScript cfg
      writeScript tripCut scr1
      withWriteLock ref tripShow $ writeFile tripShow $ show scr1
    -- tripAct = withWriteLock'IO (cfgTmpDir cfg <.> "lock") $ do
    tripAct = do
      -- _    <- withFileLock (cfgTmpDir cfg) tripSetup
      _ <- tripSetup
      scr2 <- parseFileIO cfg ref ids tripCut
      return $ pack $ show scr2

-- test that no absolute paths snuck into the tmpfiles
mkAbsTest :: CutConfig -> Locks -> HashedSeqIDsRef -> IO [TestTree]
mkAbsTest cfg ref ids = testSpecs $ it desc $
  absGrep `shouldReturn` ""
  where
    path = takeBaseName $ cfgTmpDir cfg
    desc = path ++ " tmpfiles free of absolute paths"
    absArgs = [cfgTmpDir cfg, cfgTmpDir cfg </> "exprs", "-R"]
    absGrep = do
      _ <- runCut cfg ref ids
      (_, out, err) <- readProcessWithExitCode "grep" absArgs ""
      return $ toGeneric cfg $ out ++ err

runCut :: CutConfig -> Locks -> HashedSeqIDsRef -> IO String
runCut cfg ref ids =  do
  delay 1000000 -- wait 1 second so we don't capture output from tasty
  (out, ()) <- hCapture [stdout, stderr] $ evalFile stdout cfg ref ids
  result <- doesFileExist $ cfgTmpDir cfg </> "vars" </> "result"
  when (not result) (fail out)
  return $ toGeneric cfg out

mkScriptTests :: (FilePath, FilePath, (Maybe FilePath))
              -> CutConfig -> Locks -> HashedSeqIDsRef -> IO TestTree
mkScriptTests (cut, gld, mtre) cfg ref ids = do
  absTests <- mkAbsTest cfg' ref ids -- just one, but comes as a list
  return $ testGroup name $ otherTests ++ absTests
  where
    name       = takeFileName cut
    cfg'       = cfg { cfgScript = Just cut, cfgTmpDir = (cfgTmpDir cfg </> name) }
    otherTests = [mkTripTest cfg' ref ids, mkOutTest cfg' ref ids gld] ++ genTests
    genTests   = case mtre of
                   Just t  -> [mkTreeTest cfg' ref ids t]
                   Nothing -> []

mkTests :: CutConfig -> Locks -> HashedSeqIDsRef -> IO TestTree
mkTests cfg ref ids = do
  cuts <- getTestCuts
  let outs     = map findOutFile  cuts
      mtrees   = map findTreeFile cuts
      triples  = zip3 cuts outs mtrees
      groups   = map mkScriptTests triples
  mkTestGroup cfg ref ids "interpret test scripts" groups
  where
    findOutFile  c = replaceExtension c "out"
    findTreeFile c = if nonDeterministicCut c
      then Nothing
      else Just $ replaceExtension c "tree"
