module OrthoLang.Test.Scripts where

-- import Prelude hiding (writeFile)

import Control.Concurrent.Thread.Delay (delay)
import Control.Monad              (when)
-- import Data.ByteString.Lazy.Char8 (pack, ByteString) -- TODO use binary?
import qualified Data.ByteString.Lazy  as BL
-- import qualified Data.ByteString.Lazy  as BL -- TODO is this needed?
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Char                  (toLower)
import Data.List                  (zip5, isPrefixOf)
import Data.List.Split            (splitOn)
import Paths_OrthoLang             (getDataFileName)
import OrthoLang.Core.Eval         (evalFile)
import OrthoLang.Core.Parse        (parseFileIO)
import OrthoLang.Core.Paths        (toGeneric)
import OrthoLang.Core.Util         (justOrDie)
-- import OrthoLang.Core.Pretty       (writeScript)
import OrthoLang.Core.Types        (Config(..), Module(..), IDsRef)
import OrthoLang.Core.Locks        (LocksRef, withWriteLock)
import OrthoLang.Test.Repl         (mkTestGroup)
import OrthoLang.Modules          (modules)
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
  [ "crbblast:crb_blast_each2" -- TODO should this be fixable?
  , "crbblast:crb_blast_two_cyanos"
  , "blasthits:reciprocal_best"
  , "blasthits:best_hits"
  , "sonicparanoid_myco3"
  , "orthogroups:ortholog_in_all"
  , "orthogroups:ortholog_in_any"
  , "orthogroups:ortholog_in_max"
  , "orthogroups:ortholog_in_max_2"
  , "orthogroups:ortholog_in_min"
  , "orthogroups:str_tests" -- TODO this really shouldn't vary
  , "plots:venndiagram"
  , "plots:linegraph"
  , "plots:scatterplot"
  , "plots:histogram"
  ]

-- these work, but the stdout varies so they require a check script
stdoutVaries :: [FilePath]
stdoutVaries =
  [ "crbblast:crb_blast_each2"
  , "blasthits:reciprocal_best" -- TODO should this be fixable?
  , "blasthits:best_hits"
  ]

-- these generally need work and should be skipped for now :(
badlyBroken :: [FilePath]
badlyBroken =
  -- TODO fix replace_each to work with generated lists
  [ "orthofinder:orthofinder_sets" -- TODO is this gone?
  -- TODO what's up with these?
  , "psiblast:psiblast_each_pssm"
  , "psiblast:psiblast_empty_pssms"
  , "psiblast:psiblast_map"
  , "psiblast:psiblast_pssm_all"
  , "sonicparanoid:test1"
  , "sonicparanoid:myco3" -- TODO finish writing module first
  ]

getTestScripts :: FilePath -> Maybe String -> IO [FilePath]
getTestScripts testDir mPrefix = do
  paths <- findByExtension [".ol"] testDir
  let names = map takeBaseName paths
  return $ case mPrefix of
    Nothing -> names
    Just p  -> filter ((p ++ ":") `isPrefixOf`) names

goldenDiff :: String -> FilePath -> IO BL.ByteString -> TestTree
goldenDiff name file action = goldenVsStringDiff name fn file action
  where
    -- based on the Tasty docs
    fn ref new = ["diff", "--text", "-u", ref, new]

-- TODO use <testdir>/output.txt instead of the raw output?
mkOutTest :: Config -> LocksRef -> IDsRef -> FilePath -> TestTree
mkOutTest cfg ref ids gld = goldenDiff desc gld scriptAct
  where
    -- TODO put toGeneric back here? or avoid paths in output altogether?
    scriptAct = do
      out <- runScript cfg ref ids
      -- uncomment to update the golden stdout files:
      -- writeFile ("/home/jefdaj/ortholang/tests/stdout" </> takeBaseName gld <.> "txt") out
      return $ B8.pack out
    desc = "prints expected output"

mkTreeTest :: Config -> LocksRef -> IDsRef -> FilePath -> TestTree
mkTreeTest cfg ref ids t = goldenDiff desc t treeAct
  where
    -- Note that Test/Repl.hs also has a matching tree command
    -- TODO refactor them to come from the same fn
    desc = "creates expected tmpfiles"
    -- TODO add nondeterministic expression + cache dirs here by parsing modules:
    ignores = "-I '*.lock|*.database|*.log|*.tmp|*.html|*.show|lines|output.txt|jobs'"
    sedCmd  = "sed 's/lines\\/.*/lines\\/\\.\\.\\./g'"
    treeCmd = "tree -a --dirsfirst --charset=ascii " ++ ignores ++ " | " ++ sedCmd
    wholeCmd = (shell treeCmd) { cwd = Just $ cfgTmpDir cfg }
    treeAct = do
      _ <- runScript cfg ref ids
      out <- fmap (toGeneric cfg) $ readCreateProcess wholeCmd ""
      -- uncomment to update golden tmpfile trees:
      -- writeFile ("/home/jefdaj/ortholang/tests/tmpfiles" </> takeBaseName t <.> "txt") out
      return $ B8.pack out

-- TODO use safe writes here
mkTripTest :: Config -> LocksRef -> IDsRef -> FilePath -> TestTree
mkTripTest cfg ref ids cut = goldenDiff desc tripShow tripAct
  where
    desc = "unchanged by round-trip to file"
    tripShow  = cfgTmpDir cfg </> "round-trip.show"
    tripSetup = do
      scr1 <- parseFileIO ([], cfg, ref, ids) $ justOrDie "failed to get cfgScript in mkTripTest" $ cfgScript cfg
      -- this is useful for debugging
      -- writeScript cut scr1
      withWriteLock ref tripShow $ writeBinaryFile tripShow $ show scr1
    -- tripAct = withWriteLock'IO (cfgTmpDir cfg <.> "lock") $ do
    tripAct = do
      -- _    <- withFileLock (cfgTmpDir cfg) tripSetup
      _ <- tripSetup
      scr2 <- parseFileIO ([], cfg, ref, ids) cut
      return $ B8.pack $ show scr2

-- test that no absolute paths snuck into the tmpfiles
-- TODO sanitize stdout + stderr too when running scripts
mkAbsTest :: Config -> LocksRef -> IDsRef -> IO [TestTree]
mkAbsTest cfg ref ids = testSpecs $ it desc $
  absGrep `shouldReturn` ""
  where
    desc = "expr files free of absolute paths"
    grepArgs = ["-r", "--exclude=*.out", "--exclude=*.err", "--exclude=*.ini", "--exclude=*.log",
                cfgTmpDir cfg, cfgTmpDir cfg </> "exprs"]
    absGrep = do
      _ <- runScript cfg ref ids
      (_, out, err) <- readProcessWithExitCode "grep" grepArgs ""
      return $ toGeneric cfg $ out ++ err

{- This is more or less idempotent because re-running the same cut multiple
 - times is fast. So it's OK to run it once for each test in a group.
 -}
runScript :: Config -> LocksRef -> IDsRef -> IO String
runScript cfg ref ids =  do
  delay 100000 -- wait 0.1 second so we don't capture output from tasty (TODO is that long enough?)
  (out, ()) <- hCapture [stdout, stderr] $ evalFile ([], cfg, ref, ids) stdout
  delay 100000 -- wait 0.1 second so we don't capture output from tasty (TODO is that long enough?)
  result <- doesFileExist $ cfgTmpDir cfg </> "vars" </> "result"
  when (not result) (fail out)
  writeBinaryFile (cfgTmpDir cfg </> "output" <.> "txt") $ toGeneric cfg out -- for the shell script tests
  return $ toGeneric cfg out

mkScriptTests :: (FilePath, FilePath, FilePath, FilePath, Maybe FilePath)
              -> Config -> LocksRef -> IDsRef -> IO TestTree
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
mkCheckTest :: Config -> LocksRef -> IDsRef -> FilePath -> IO [TestTree]
mkCheckTest cfg ref ids scr = testSpecs $ it desc $ runCheck `shouldReturn` ""
  where
    desc = "output + tmpfiles checked by script"
    runCheck = do
      _ <- runScript cfg ref ids
      (_, out, err) <- readProcessWithExitCode "bash" [scr, cfgTmpDir cfg] ""
      return $ toGeneric cfg $ out ++ err

testFilePath :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
testFilePath base dir ext name = base </> dir </> name <.> ext

findTestFile :: FilePath -> FilePath -> FilePath -> FilePath -> IO (Maybe FilePath)
findTestFile base dir ext name = do
  let path = testFilePath base dir ext name
  exists <- doesFileExist path
  return $ if exists then Just path else Nothing

mkTestsPrefix :: Config -> LocksRef -> IDsRef -> FilePath -> String -> Maybe String -> IO TestTree
mkTestsPrefix cfg ref ids testDir groupName mPrefix = do
  names   <- getTestScripts testDir mPrefix
  mchecks <- mapM (findTestFile testDir "check" "sh") names
  let cuts   = map (testFilePath testDir "scripts"  "ol" ) names
      outs   = map (testFilePath testDir "stdout"   "txt") names
      trees  = map (testFilePath testDir "tmpfiles" "txt") names
      hepts  = zip5 (map removePrefix names) cuts outs trees mchecks
      groups = map mkScriptTests hepts
  mkTestGroup cfg ref ids groupName groups

mkExampleTests :: Config -> LocksRef -> IDsRef -> FilePath -> FilePath -> IO TestTree
mkExampleTests cfg ref ids exDir testDir = do
  names <- getTestScripts exDir Nothing
  let names' = map ("examples:" ++) names
  mchecks <- mapM (findTestFile testDir "check" "sh") names'
  let cuts   = map (testFilePath exDir   "scripts"  "ol" ) names
      outs   = map (testFilePath testDir "stdout"   "txt") names'
      trees  = map (testFilePath testDir "tmpfiles" "txt") names'
      hepts  = zip5 (map removePrefix names) cuts outs trees mchecks
      groups = map mkScriptTests hepts
  mkTestGroup cfg ref ids "examples for demo site" groups

removePrefix :: String -> String
removePrefix = last . splitOn ":"

-- from: https://stackoverflow.com/q/47876071
simplify :: String -> String
simplify = filter (`elem` ['a'..'z']) . map toLower

mkTests :: Config -> LocksRef -> IDsRef -> IO TestTree
mkTests cfg ref ids = do
  testDir <- getDataFileName "tests"
  exDir   <- getDataFileName "examples"
  groups  <- mapM (\mn -> mkTestsPrefix cfg ref ids testDir mn $ Just mn) $ map (simplify . mName) modules
  exGroup <- mkExampleTests cfg ref ids exDir testDir
  return $ testGroup "run test scripts" $ groups ++ [exGroup]
