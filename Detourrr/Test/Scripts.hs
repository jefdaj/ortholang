module Detourrr.Test.Scripts where

import Prelude hiding (writeFile)

import Control.Concurrent.Thread.Delay (delay)
import Control.Monad              (when)
import Data.ByteString.Lazy.Char8 (pack, ByteString)
import Data.List                  (isPrefixOf)
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
import System.FilePath            (splitDirectories, joinPath)
import System.FilePath.Posix      (replaceExtension, takeBaseName, takeFileName, (</>), (<.>))
import System.IO                  (stdout, stderr, writeFile)
import System.IO.Silently         (hCapture)
import System.Process             (readCreateProcess, readProcessWithExitCode,
                                   cwd, shell)
import Test.Hspec                 (it)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsStringDiff, findByExtension)
import Test.Tasty.Hspec           (testSpecs, shouldReturn)

nonDeterministicRrr :: FilePath -> Bool
nonDeterministicRrr path = any (\s -> s `isPrefixOf` (takeBaseName path)) badSigns
  where
    -- TODO will regular blast be nondeterministic at large scales too?
    -- TODO make these individual tests, or substrings
    -- TODO eventually replace deterministic or not with custom shell predicates
    badSigns = ["crb_blast", "blastrbh", "blasthits", "plot", "mmseqs"] -- TODO blast? blastdb?

getTestScripts :: IO [FilePath]
getTestScripts = do
  testDir  <- getDataFileName "tests2"
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
    scriptAct = do
      out <- runRrr cfg ref ids
      writeFile ("/tmp" </> takeBaseName gld <.> "out") out
      return $ pack out
    rrr  = takeBaseName gld <.> "rrr"
    desc = rrr ++ " prints expected output"

mkTreeTest :: RrrConfig -> Locks -> HashedSeqIDsRef -> FilePath -> TestTree
mkTreeTest cfg ref ids t = goldenDiff desc t treeAct
  where
    -- Note that Test/Repl.hs also has a matching tree command
    -- TODO refactor them to come from the same fn
    desc = takeBaseName t ++ ".rrr creates expected tmpfiles"
    sedCmd  = "sed 's/lines\\/.*/lines\\/\\.\\.\\./g'"
    treeCmd = (shell $ "tree -aI '*.lock|*.database|*.log|*.tmp|*.html|lines' | " ++ sedCmd)
                { cwd = Just $ cfgTmpDir cfg }
    treeAct = do
      _ <- runRrr cfg ref ids
      out <- readCreateProcess treeCmd ""
      -- sometimes useful for debugging tests:
      -- writeFile ("/tmp" </> takeBaseName t <.> "tree") out
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
  delay 10000 -- wait 0.01 second so we don't capture output from tasty (TODO is that long enough?)
  (out, ()) <- hCapture [stdout, stderr] $ evalFile stdout cfg ref ids
  result <- doesFileExist $ cfgTmpDir cfg </> "vars" </> "result"
  when (not result) (fail out)
  return $ toGeneric cfg out

mkScriptTests :: (FilePath, FilePath, Maybe FilePath)
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
    -- findOutFile  c = takeDirectory c </> replaceExtension c "out"
    findOutFile c = joinPath $ (init $ init $ splitDirectories c)
                      ++ ["stdout", replaceExtension (takeBaseName c) "out"]
    findTreeFile c = if nonDeterministicRrr c
      then Nothing
      else Just $ joinPath $ (init $ init $ splitDirectories c)
                    ++ ["tmpfiles", replaceExtension (takeBaseName c) "tree"]
