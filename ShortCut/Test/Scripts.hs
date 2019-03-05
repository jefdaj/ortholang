module ShortCut.Test.Scripts where

-- import Debug.Trace (trace, traceShow)

import Prelude hiding (writeFile)

import Control.Concurrent.Thread.Delay (delay)
import Control.Monad              (when)
import Data.ByteString.Lazy.Char8 (pack, ByteString)
import Data.List                  (zip4)
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
import System.FilePath.Posix      (takeBaseName, (</>), (<.>))
import System.IO                  (stdout, stderr, writeFile)
import System.IO.Silently         (hCapture)
import System.Process             (readCreateProcess, readProcessWithExitCode,
                                   cwd, shell)
import Test.Hspec                 (it)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsStringDiff, findByExtension)
import Test.Tasty.Hspec           (testSpecs, shouldReturn)

-- TODO remove in favor of shell scripts that test specific properties of tmpfiles
-- nonDeterministicCut :: FilePath -> Bool
-- nonDeterministicCut path = any (\s -> s `isPrefixOf` (takeBaseName path)) badSigns
--   where
--     -- TODO will regular blast be nondeterministic at large scales too?
--     -- TODO make these individual tests, or substrings
--     -- TODO eventually replace deterministic or not with custom shell predicates
--     badSigns = ["crb_blast", "blastrbh", "blasthits", "plot", "mmseqs"] -- TODO blast? blastdb?

knownFailing :: [FilePath]
knownFailing =

  -- TODO test + ask if mmseqs can be compiled on the server
  -- TODO test only when it's installed?
  -- [ "mmseqs_createdb"
  -- , "mmseqs_createdb_all"
  -- , "mmseqs_search"
  -- , "mmseqs_search_db"

  -- TODO get this to run on the server! (can it be done?)
  [ "mmseqs_createdb"
  , "mmseqs_createdb_all"
  , "mmseqs_search"
  , "mmseqs_search_db"
  , "sonicparanoid_basic"

  -- TODO what's up with this? "indirect recursion detected" but only sometimes
  -- , "ncbi_blast_reciprocal_best"

  -- TODO fix replace_each to work with generated lists
  , "orthofinder_orthofinder_sets"

  -- TODO what's up with these?
  , "psiblast_each_pssm"
  , "psiblast_each_pssm"
  , "psiblast_empty_pssms"
  , "psiblast_empty_pssms"
  , "psiblast_map"
  , "psiblast_pssm_all"

  ]

getTestScripts :: FilePath -> IO [FilePath]
getTestScripts testDir = do
  testCuts <- fmap (map takeBaseName) $ findByExtension [".cut"] testDir
  let testCuts' = filter (\p -> not $ p `elem` knownFailing) testCuts
  return testCuts'

goldenDiff :: String -> FilePath -> IO ByteString -> TestTree
goldenDiff name file action = goldenVsStringDiff name fn file action
  where
    -- this is taken from the Tasty docs
    fn ref new = ["diff", "-u", ref, new]

-- TODO use <testdir>/output.txt instead of the raw output?
mkOutTest :: CutConfig -> Locks -> HashedSeqIDsRef -> FilePath -> TestTree
mkOutTest cfg ref ids gld = goldenDiff desc gld scriptAct
  where
    -- TODO put toGeneric back here? or avoid paths in output altogether?
    scriptAct = do
      out <- runCut cfg ref ids
      -- useful for debugging tests or updating the golden files
      -- writeFile ("/tmp" </> takeBaseName gld <.> "txt") out
      return $ pack out
    desc = "prints expected output"

mkTreeTest :: CutConfig -> Locks -> HashedSeqIDsRef -> FilePath -> TestTree
mkTreeTest cfg ref ids t = goldenDiff desc t treeAct
  where
    -- Note that Test/Repl.hs also has a matching tree command
    -- TODO refactor them to come from the same fn
    desc = "creates expected tmpfiles"
    sedCmd  = "sed 's/lines\\/.*/lines\\/\\.\\.\\./g'"
    treeCmd = (shell $ "tree -aI '*.lock|*.database|*.log|*.tmp|*.html|*.show|lines|output.txt' | " ++ sedCmd)
                { cwd = Just $ cfgTmpDir cfg }
    treeAct = do
      _ <- runCut cfg ref ids
      out <- readCreateProcess treeCmd ""
      -- useful for debugging tests or updating the golden files
      -- writeFile ("/tmp" </> takeBaseName t <.> "txt") out
      return $ pack $ toGeneric cfg out

-- TODO use safe writes here
mkTripTest :: CutConfig -> Locks -> HashedSeqIDsRef -> FilePath -> TestTree
mkTripTest cfg ref ids cut = goldenDiff desc tripShow tripAct
  where
    desc = "unchanged by round-trip to file"
    tripShow  = cfgTmpDir cfg </> "round-trip.show"
    tripSetup = do
      scr1 <- parseFileIO cfg ref ids $ fromJust $ cfgScript cfg
      writeScript cut scr1
      withWriteLock ref tripShow $ writeFile tripShow $ show scr1
    -- tripAct = withWriteLock'IO (cfgTmpDir cfg <.> "lock") $ do
    tripAct = do
      -- _    <- withFileLock (cfgTmpDir cfg) tripSetup
      _ <- tripSetup
      scr2 <- parseFileIO cfg ref ids cut
      return $ pack $ show scr2

-- test that no absolute paths snuck into the tmpfiles
mkAbsTest :: CutConfig -> Locks -> HashedSeqIDsRef -> IO [TestTree]
mkAbsTest cfg ref ids = testSpecs $ it desc $
  absGrep `shouldReturn` ""
  where
    desc = "tmpfiles free of absolute paths"
    absArgs = [cfgTmpDir cfg, cfgTmpDir cfg </> "exprs", "-R"]
    absGrep = do
      _ <- runCut cfg ref ids
      (_, out, err) <- readProcessWithExitCode "grep" absArgs ""
      return $ toGeneric cfg $ out ++ err

{- This is more or less idempotent because re-running the same cut multiple
 - times is fast. So it's OK to run it once for each test in a group.
 -}
runCut :: CutConfig -> Locks -> HashedSeqIDsRef -> IO String
runCut cfg ref ids =  do
  delay 100000 -- wait 0.1 second so we don't capture output from tasty (TODO is that long enough?)
  (out, ()) <- hCapture [stdout, stderr] $ evalFile stdout cfg ref ids
  delay 100000 -- wait 0.1 second so we don't capture output from tasty (TODO is that long enough?)
  result <- doesFileExist $ cfgTmpDir cfg </> "vars" </> "result"
  when (not result) (fail out)
  writeFile (cfgTmpDir cfg </> "output" <.> "txt") $ toGeneric cfg out -- for the shell script tests
  return $ toGeneric cfg out

mkScriptTests :: (FilePath, FilePath, FilePath, Maybe FilePath)
              -> CutConfig -> Locks -> HashedSeqIDsRef -> IO TestTree
mkScriptTests (cut, out, tre, mchk) cfg ref ids = do
  absTests   <- mkAbsTest   cfg' ref ids -- just one, but comes as a list
  checkTests <- case mchk of
                  Nothing -> return []
                  Just c  -> mkCheckTest cfg' ref ids c
  let tripTest = mkTripTest cfg' ref ids cut
      outTest  = mkOutTest  cfg' ref ids out -- TODO why is this trying to use the tree files as golden??
      treeTest = mkTreeTest cfg' ref ids tre
      tests    = [tripTest, outTest] ++ absTests ++ [treeTest] ++ checkTests
  return $ testGroup name tests
  where
    name = takeBaseName cut
    cfg' = cfg { cfgScript = Just cut, cfgTmpDir = (cfgTmpDir cfg </> name) }

{- "check scripts" for handling the tricky cases:
 - they get passed the tmpdir as their only argument
 - they should give no output if the tests pass, and print errors otherwise
 - TODO move stdout inside the tmpdir?
 -}
mkCheckTest :: CutConfig -> Locks -> HashedSeqIDsRef -> FilePath -> IO [TestTree]
mkCheckTest cfg ref ids scr = testSpecs $ it desc $ runCheck `shouldReturn` ""
  where
    desc = "output + tmpfiles checked by script"
    runCheck = do
      _ <- runCut cfg ref ids
      (_, out, err) <- readProcessWithExitCode "bash" [scr, cfgTmpDir cfg] ""
      return $ toGeneric cfg $ out ++ err

testFilePath :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
testFilePath base dir ext name = base </> dir </> name <.> ext

findTestFile :: FilePath -> FilePath -> FilePath -> FilePath -> IO (Maybe FilePath)
findTestFile base dir ext name = do
  let path = testFilePath base dir ext name
  exists <- doesFileExist path
  return $ if exists then Just path else Nothing

mkTests :: CutConfig -> Locks -> HashedSeqIDsRef -> IO TestTree
mkTests cfg ref ids = do
  testDir <- getDataFileName "tests"
  names   <- getTestScripts testDir
  mchecks <- mapM (findTestFile testDir "check" "sh") names
  let cuts   = map (testFilePath testDir "scripts"  "cut") names
      outs   = map (testFilePath testDir "stdout"   "txt") names
      trees  = map (testFilePath testDir "tmpfiles" "txt") names
      quads  = zip4 cuts outs trees mchecks
      groups = map mkScriptTests quads
  mkTestGroup cfg ref ids "interpret test scripts" groups
