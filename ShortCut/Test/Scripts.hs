module ShortCut.Test.Scripts where

-- import Prelude hiding (writeFile)

import Control.Concurrent.Thread.Delay (delay)
import Control.Monad              (when)
-- import Data.ByteString.Lazy.Char8 (pack, ByteString) -- TODO use binary?
import qualified Data.ByteString.Lazy  as BL
-- import qualified Data.ByteString.Lazy  as BL -- TODO is this needed?
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.List                  (zip5)
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Eval         (evalFile)
import ShortCut.Core.Parse        (parseFileIO)
import ShortCut.Core.Paths        (toGeneric)
import ShortCut.Core.Util         (justOrDie)
-- import ShortCut.Core.Pretty       (writeScript)
import ShortCut.Core.Types        (CutConfig(..), HashedIDsRef)
import ShortCut.Core.Locks        (Locks, withWriteLock)
import ShortCut.Test.Repl         (mkTestGroup)
import System.Directory           (doesFileExist)
import System.FilePath.Posix      (takeBaseName, (</>), (<.>))
import System.IO                  (stdout, stderr)
import System.IO.Silently         (hCapture)
import System.Process             (readCreateProcess, readProcessWithExitCode,
                                   cwd, shell)
import Test.Hspec                 (it)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsStringDiff, findByExtension, writeBinaryFile)
import Test.Tasty.Hspec           (testSpecs, shouldReturn)

-- these work, but the tmpfiles vary so they require a check script
tmpfilesVary :: [FilePath]
tmpfilesVary =
  [ "crb_blast_each2" -- TODO should this be fixable?
  , "ncbi_blast_reciprocal_best"
  , "blast_hits_best_hits"
  , "sonicparanoid_myco3"
  ]

-- these work, but the stdout varies so they require a check script
stdoutVaries :: [FilePath]
stdoutVaries =
  [ "crb_blast_each2"
  , "ncbi_blast_reciprocal_best" -- TODO should this be fixable?
  , "blast_hits_best_hits"
  ]

-- these generally need work and should be skipped for now :(
badlyBroken :: [FilePath]
badlyBroken =
  -- TODO fix replace_each to work with generated lists
  [ "orthofinder_orthofinder_sets"
  -- TODO what's up with these?
  , "psiblast_each_pssm"
  , "psiblast_each_pssm"
  , "psiblast_empty_pssms"
  , "psiblast_empty_pssms"
  , "psiblast_map"
  , "psiblast_pssm_all"
  ]

getTestScripts :: FilePath -> IO [FilePath]
getTestScripts testDir = fmap (map takeBaseName) $ findByExtension [".cut"] testDir

goldenDiff :: String -> FilePath -> IO BL.ByteString -> TestTree
goldenDiff name file action = goldenVsStringDiff name fn file action
  where
    -- based on the Tasty docs
    fn ref new = ["diff", "--text", "-u", ref, new]

-- TODO use <testdir>/output.txt instead of the raw output?
mkOutTest :: CutConfig -> Locks -> HashedIDsRef -> FilePath -> TestTree
mkOutTest cfg ref ids gld = goldenDiff desc gld scriptAct
  where
    -- TODO put toGeneric back here? or avoid paths in output altogether?
    scriptAct = do
      out <- runCut cfg ref ids
      -- uncomment to update the golden stdout files:
      -- writeFile ("/home/jefdaj/shortcut/tests/stdout" </> takeBaseName gld <.> "txt") out
      return $ B8.pack out
    desc = "prints expected output"

mkTreeTest :: CutConfig -> Locks -> HashedIDsRef -> FilePath -> TestTree
mkTreeTest cfg ref ids t = goldenDiff desc t treeAct
  where
    -- Note that Test/Repl.hs also has a matching tree command
    -- TODO refactor them to come from the same fn
    desc = "creates expected tmpfiles"
    -- TODO add nondeterministic expression + cache dirs here by parsing modules:
    ignores = "-I '*.lock|*.database|*.log|*.tmp|*.html|*.show|lines|output.txt'"
    sedCmd  = "sed 's/lines\\/.*/lines\\/\\.\\.\\./g'"
    treeCmd = "tree -a --charset=ascii " ++ ignores ++ " | " ++ sedCmd
    wholeCmd = (shell treeCmd) { cwd = Just $ cfgTmpDir cfg }
    treeAct = do
      _ <- runCut cfg ref ids
      out <- fmap (toGeneric cfg) $ readCreateProcess wholeCmd ""
      -- uncomment to update golden tmpfile trees:
      -- writeFile ("/home/jefdaj/shortcut/tests/tmpfiles" </> takeBaseName t <.> "txt") out
      return $ B8.pack out

-- TODO use safe writes here
mkTripTest :: CutConfig -> Locks -> HashedIDsRef -> FilePath -> TestTree
mkTripTest cfg ref ids cut = goldenDiff desc tripShow tripAct
  where
    desc = "unchanged by round-trip to file"
    tripShow  = cfgTmpDir cfg </> "round-trip.show"
    tripSetup = do
      scr1 <- parseFileIO cfg ref ids $ justOrDie "failed to get cfgScript in mkTripTest" $ cfgScript cfg
      -- this is useful for debugging
      -- writeScript cut scr1
      withWriteLock ref tripShow $ writeBinaryFile tripShow $ show scr1
    -- tripAct = withWriteLock'IO (cfgTmpDir cfg <.> "lock") $ do
    tripAct = do
      -- _    <- withFileLock (cfgTmpDir cfg) tripSetup
      _ <- tripSetup
      scr2 <- parseFileIO cfg ref ids cut
      return $ B8.pack $ show scr2

-- test that no absolute paths snuck into the tmpfiles
-- TODO sanitize stdout + stderr too when running scripts
mkAbsTest :: CutConfig -> Locks -> HashedIDsRef -> IO [TestTree]
mkAbsTest cfg ref ids = testSpecs $ it desc $
  absGrep `shouldReturn` ""
  where
    desc = "expr files free of absolute paths"
    grepArgs = ["-r", "--exclude=*.out", "--exclude=*.err", "--exclude=*.ini", "--exclude=*.log",
                cfgTmpDir cfg, cfgTmpDir cfg </> "exprs"]
    absGrep = do
      _ <- runCut cfg ref ids
      (_, out, err) <- readProcessWithExitCode "grep" grepArgs ""
      return $ toGeneric cfg $ out ++ err

{- This is more or less idempotent because re-running the same cut multiple
 - times is fast. So it's OK to run it once for each test in a group.
 -}
runCut :: CutConfig -> Locks -> HashedIDsRef -> IO String
runCut cfg ref ids =  do
  delay 100000 -- wait 0.1 second so we don't capture output from tasty (TODO is that long enough?)
  (out, ()) <- hCapture [stdout, stderr] $ evalFile stdout cfg ref ids
  delay 100000 -- wait 0.1 second so we don't capture output from tasty (TODO is that long enough?)
  result <- doesFileExist $ cfgTmpDir cfg </> "vars" </> "result"
  when (not result) (fail out)
  writeBinaryFile (cfgTmpDir cfg </> "output" <.> "txt") $ toGeneric cfg out -- for the shell script tests
  return $ toGeneric cfg out

mkScriptTests :: (FilePath, FilePath, FilePath, FilePath, Maybe FilePath)
              -> CutConfig -> Locks -> HashedIDsRef -> IO TestTree
mkScriptTests (name, cut, out, tre, mchk) cfg ref ids = do
  absTests   <- mkAbsTest   cfg' ref ids -- just one, but comes as a list
  checkTests <- case mchk of
                  Nothing -> return []
                  Just c  -> mkCheckTest cfg' ref ids c
  let tripTest  = mkTripTest cfg' ref ids cut
      outTests  = if (name `elem` stdoutVaries) then [] else [mkOutTest  cfg' ref ids out]
      treeTests = if (name `elem` tmpfilesVary) then [] else [mkTreeTest cfg' ref ids tre]
      tests     = if (name `elem` badlyBroken)
                    then []
                    else [tripTest] ++ outTests ++ absTests ++ treeTests ++ checkTests
  return $ testGroup name tests
  where
    cfg' = cfg { cfgScript = Just cut, cfgTmpDir = (cfgTmpDir cfg </> name) }

{- "check scripts" for handling the tricky cases:
 - they get passed the tmpdir as their only argument
 - they should give no output if the tests pass, and print errors otherwise
 - TODO move stdout inside the tmpdir?
 -}
mkCheckTest :: CutConfig -> Locks -> HashedIDsRef -> FilePath -> IO [TestTree]
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

mkTests :: CutConfig -> Locks -> HashedIDsRef -> IO TestTree
mkTests cfg ref ids = do
  testDir <- getDataFileName "tests"
  exDir   <- getDataFileName "examples"
  names   <- getTestScripts testDir
  exNames <- getTestScripts exDir
  let exNames' = map ("examples_" ++) exNames
  mchecks <- mapM (findTestFile testDir "check" "sh") (names ++ exNames')
  let cuts   = map (testFilePath testDir "scripts"  "cut") names ++
               map (testFilePath exDir "cut-scripts" "cut") exNames
      outs   = map (testFilePath testDir "stdout"   "txt") (names ++ exNames')
      trees  = map (testFilePath testDir "tmpfiles" "txt") (names ++ exNames')
      hepts  = zip5 (names ++ exNames') cuts outs trees mchecks
      groups = map mkScriptTests hepts
  mkTestGroup cfg ref ids "interpret test scripts" groups
