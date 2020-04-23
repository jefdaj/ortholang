module OrthoLang.Test.Scripts where

import qualified Control.Concurrent.Thread.Delay as D
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Char8      as B8

import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Modules     (modules)
import OrthoLang.Locks (withWriteLockEmpty)
import OrthoLang.Util   (justOrDie, rmAll)

import Control.Monad         (when)
import Data.Char             (toLower)
import Data.List             (zip7, isPrefixOf)
import Data.List.Split       (splitOn)
import Data.List.Utils       (replace)
import OrthoLang.Modules     (modules)
import Paths_OrthoLang       (getDataFileName)
import System.Directory      (doesFileExist, createDirectoryIfMissing)
import System.FilePath.Posix (takeBaseName, (</>), (<.>))
import System.IO             (stdout, stderr)
import System.IO.Silently    (hCapture)
import System.Process        (readProcess, readCreateProcess, readProcessWithExitCode, cwd, shell)
import Test.Hspec            (it)
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.Golden     (goldenVsStringDiff, findByExtension, writeBinaryFile)
import Test.Tasty.Hspec      (testSpecs, shouldReturn)

import qualified Data.Text.Lazy as T
import Text.Pretty.Simple (pShowNoColor)

-- import OrthoLang.Interpreter.Pretty (renderIO)
-- import Text.PrettyPrint.HughesPJClass (pPrint)

-- | These work, but the tmpfiles vary so they require a check script.
tmpfilesVary :: [FilePath]
tmpfilesVary =
  [ "crbblast:crb_blast_each2" -- TODO should this be fixable?
  , "crbblast:crb_blast_two_cyanos"
  , "blasthits:reciprocal_best"
  , "blasthits:best_hits"
  , "sonicparanoid:myco3"
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

-- | These work, but the stdout varies so they require a check script.
stdoutVaries :: [FilePath]
stdoutVaries =
  [ "crbblast:crb_blast_each2"
  , "blasthits:reciprocal_best" -- TODO should this be fixable?
  , "blasthits:best_hits"
  ]

-- | These generally need work and should be skipped for now :(
badlyBroken :: [FilePath]
badlyBroken =
  [ "orthofinder:orthofinder_sets"
  , "blast:blastdb_each"
  , "psiblast:psiblast_each_pssm"
  , "psiblast:psiblast_empty_pssms"
  , "psiblast:psiblast_map"
  , "psiblast:psiblast_pssm_all"
  , "psiblast:compose1"
  , "sonicparanoid:test1"
  , "sonicparanoid:myco3" -- TODO finish writing module first
  ]

mkTestGroup ::  Config -> LocksRef -> IDsRef -> DigestsRef -> String
            -> [Config -> LocksRef -> IDsRef -> DigestsRef -> IO TestTree] -> IO TestTree
mkTestGroup cfg ref ids dRef name trees = do
  let trees' = mapM (\t -> t cfg ref ids dRef) trees
  trees'' <- trees'
  return $ testGroup name trees''

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
mkOutTest :: Config -> LocksRef -> IDsRef -> DigestsRef -> FilePath -> FilePath -> String -> TestTree
mkOutTest cfg ref ids dRef sDir name gld = goldenDiff d gld scriptAct
  where
    -- TODO put toGeneric back here? or avoid paths in output altogether?
    scriptAct = do
      out <- runScript cfg ref ids dRef
      -- uncomment to update the golden stdout files:
      -- writeFile ("tests/stdout" </> takeBaseName gld <.> "txt") out
      return $ B8.pack out
    d = name ++ ".ol prints expected output"

withTmpDirLock :: Config -> LocksRef -> IO a -> IO a
withTmpDirLock cfg ref act = withWriteLockEmpty ref (tmpdir cfg </> "lock") act

-- TODO use this in repl tree tests too
mkTreeTest ::  Config -> LocksRef -> IDsRef -> DigestsRef -> FilePath
           -> (Config -> LocksRef -> IDsRef -> DigestsRef -> IO ())
           -> String -> TestTree
mkTreeTest cfg ref ids dRef name act t = goldenDiff d t (act' >> treeAct)
  where
    act' = act cfg ref ids dRef
    -- Note that Test/Repl.hs also has a matching tree command
    -- TODO refactor them to come from the same fn
    d = name ++ ".ol creates expected tmpfiles"
    -- TODO add nondeterministic expression + cache dirs here by parsing modules:
    ignores = "-I '*.lock|*.database|*.log|*.tmp|*.html|*.show|*.out|lines|output.txt|jobs|out|err'"
    sedCmd  = "sed 's/lines\\/.*/lines\\/\\.\\.\\./g'"
    treeCmd = "tree -a --dirsfirst --charset=ascii " ++ ignores ++ " | " ++ sedCmd
    wholeCmd = (shell treeCmd) { cwd = Just $ tmpdir cfg }
    treeAct = do
      out <- withTmpDirLock cfg ref $ fmap (toGeneric cfg) $ readCreateProcess wholeCmd ""
      -- uncomment to update golden tmpfile trees:
      -- writeFile ("tests/tmpfiles" </> takeBaseName t <.> "txt") out
      return $ B8.pack out

-- TODO use safe writes here
mkTripTest :: Config -> LocksRef -> IDsRef -> DigestsRef -> String -> FilePath -> FilePath -> TestTree
mkTripTest cfg ref ids dRef name cut parse = goldenDiff d parse tripAct
  where
    d = name ++ ".ol unchanged by round-trip to file"
    -- tripShow  = tmpdir cfg </> "round-trip.show"
    tripSetup = do
      scr1 <- parseFileIO modules (emptyScript, cfg, ref, ids, dRef) $
                justOrDie "failed to get script in mkTripTest" $ script cfg
      -- this is useful for debugging
      -- writeScript cut scr1
      writeBinaryFile parse $ T.unpack $ pShowNoColor scr1
    -- tripAct = withWriteLock'IO (tmpdir cfg <.> "lock") $ do
    tripAct = withTmpDirLock cfg ref $ do
      -- _    <- withFileLock (tmpdir cfg) tripSetup
      _ <- tripSetup
      scr2 <- parseFileIO modules (emptyScript, cfg, ref, ids, dRef) cut
      return $ B8.pack $ T.unpack $ pShowNoColor scr2

mkExpandTest :: Config -> LocksRef -> IDsRef -> DigestsRef -> String -> FilePath -> FilePath -> TestTree
mkExpandTest cfg ref ids dRef name cut expand = goldenDiff d expand expAct
  where
    d = name ++ ".ol expands macros as expected"
    expAct = withTmpDirLock cfg ref $ do
      scr <- parseFileIO modules (emptyScript, cfg, ref, ids, dRef) cut
      let scr' = expandMacros modules scr
      txt <- renderIO cfg $ pPrint scr'
      return $ B8.pack txt

mkShareTest :: Config -> LocksRef -> IDsRef -> DigestsRef -> FilePath -> FilePath -> String -> TestTree
mkShareTest cfg ref ids dRef sDir name gld = goldenDiff d gld shareAct
  where
    cfg' = cfg { shared = Just sDir }
    shareAct = do
      _ <- runScript cfg ref ids dRef
      _ <- copyToShared cfg sDir ref
      withTmpDirLock cfg ref $ rmAll [tmpdir cfg] -- to see if it can use the shared one instead
      out <- runScript cfg' ref ids dRef
      return $ B8.pack out
    d = name ++ ".ol re-uses shared tmpfiles"

{-|
Test that no absolute paths snuck into the tmpfiles.

TODO sanitize stdout + stderr too when running scripts
-}
mkAbsTest :: Config -> LocksRef -> IDsRef -> String -> DigestsRef -> FilePath -> IO [TestTree]
mkAbsTest cfg ref ids name dRef sDir = testSpecs $ it d $
  absGrep `shouldReturn` ""
  where
    d = name ++ ".ol expr files free of absolute paths"
    grepArgs = ["-r", "--exclude=*.out", "--exclude=*.err",
                "--exclude=*.ini", "--exclude=*.log",
                tmpdir cfg, tmpdir cfg </> "exprs"]
    absGrep = do
      _ <- runScript cfg ref ids dRef
      (_, out, err) <- withTmpDirLock cfg ref $ readProcessWithExitCode "grep" grepArgs ""
      return $ toGeneric cfg $ out ++ err

copyToShared :: Config -> FilePath -> LocksRef -> IO ()
copyToShared cfg sDir ref = withTmpDirLock cfg ref $ do
  createDirectoryIfMissing True $ sDir </> "cache"
  createDirectoryIfMissing True $ sDir </> "exprs"
  let rsync p = readProcess "rsync" ["-qraz", tmpdir cfg </> p ++ "/", sDir </> p ++ "/"] ""
  mapM_ rsync ["cache", "exprs"]

{-|
This is more or less idempotent because re-running the same cut multiple
times is fast. So it's OK to run it once for each test in a group.
-}
runScript :: Config -> LocksRef -> IDsRef -> DigestsRef -> IO String
runScript cfg ref ids dRef = withTmpDirLock cfg ref $ do
  D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
  (out, ()) <- hCapture [stdout, stderr] $ evalFile modules (emptyScript, cfg, ref, ids, dRef) stdout
  D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
  result <- doesFileExist $ tmpdir cfg </> "vars" </> "result"
  when (not result) (fail out)
  writeBinaryFile (tmpdir cfg </> "output" <.> "txt") $ toGeneric cfg out
  return $ toGeneric cfg out

mkScriptTests
  :: FilePath
  -> (FilePath, FilePath, FilePath, FilePath, FilePath, FilePath, Maybe FilePath)
  -> Config -> LocksRef -> IDsRef -> DigestsRef -> IO TestTree
mkScriptTests sDir (name, cut, parse, expand, out, tre, mchk) cfg ref ids dRef = do
  absTests   <- mkAbsTest  cfg' ref ids name dRef sDir -- just one, but comes as a list
  checkTests <- case mchk of
                  Nothing -> return []
                  Just c  -> mkCheckTest cfg' ref ids dRef sDir name c
  let tripTest  = mkTripTest  cfg' ref ids dRef name cut parse
      expTest   = mkExpandTest  cfg' ref ids dRef name cut expand
      shareTest = mkShareTest cfg' ref ids dRef sDir name out
      runScriptU c r i d = runScript c r i d >> return ()
      outTests  = if (name `elem` stdoutVaries) then [] else [mkOutTest  cfg' ref ids dRef sDir name out]
      treeTests = if (name `elem` tmpfilesVary) then [] else [mkTreeTest cfg' ref ids dRef name runScriptU tre]
      tests     = if (name `elem` badlyBroken)
                     then []
                     else [tripTest, expTest] ++ outTests ++ absTests ++ treeTests ++ checkTests ++ [shareTest]
  return $ testGroup (removePrefix name) tests
  where
    name' = replace ":" "_" name -- ':' messes with BLASTDB paths
    cfg' = cfg { script = Just cut, tmpdir = (tmpdir cfg </> name') }

{-|
"Check scripts" for handling the tricky cases where tmpfile names vary. They
get passed the tmpdir as their only argument, and can inspect it however is
needed. They should give no output if the tests pass, and print errors
otherwise.

TODO move stdout inside the tmpdir?
-}
mkCheckTest :: Config -> LocksRef -> IDsRef -> DigestsRef -> FilePath -> FilePath -> String -> IO [TestTree]
mkCheckTest cfg ref ids dRef sDir name scr = testSpecs $ it d $ runCheck `shouldReturn` ""
  where
    d = name ++ ".ol output + tmpfiles checked by script"
    runCheck = do
      _ <- runScript cfg ref ids dRef -- TODO any reason to reuse ids here?
      (_, out, err) <- withTmpDirLock cfg ref $ readProcessWithExitCode "bash" [scr, tmpdir cfg] ""
      return $ toGeneric cfg $ out ++ err

testFilePath :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
testFilePath base dir ext name = base </> dir </> name <.> ext

findTestFile :: FilePath -> FilePath -> FilePath -> FilePath -> IO (Maybe FilePath)
findTestFile base dir ext name = do
  let path = testFilePath base dir ext name
  exists <- doesFileExist path
  return $ if exists then Just path else Nothing

mkTestsPrefix :: Config -> LocksRef -> IDsRef -> DigestsRef -> FilePath
              -> String -> FilePath -> Maybe String -> IO TestTree
mkTestsPrefix cfg ref ids dRef testDir groupName shareDir mPrefix = do
  names   <- getTestScripts testDir mPrefix
  mchecks <- mapM (findTestFile testDir "check" "sh") names
  let cuts   = map (testFilePath testDir "scripts"  "ol" ) names
      parses = map (testFilePath testDir "parse"    "txt") names
      expands = map (testFilePath testDir "expand"   "txt") names
      outs   = map (testFilePath testDir "stdout"   "txt") names
      trees  = map (testFilePath testDir "tmpfiles" "txt") names
      hepts  = zip7 names cuts parses expands outs trees mchecks
      groups = map (mkScriptTests shareDir) hepts
  mkTestGroup cfg ref ids dRef groupName groups

mkExampleTests :: Config -> LocksRef -> IDsRef -> DigestsRef -> FilePath -> FilePath -> FilePath -> IO TestTree
mkExampleTests cfg ref ids dRef exDir shareDir testDir = do
  names <- getTestScripts exDir Nothing
  let names' = map ("examples:" ++) names
  mchecks <- mapM (findTestFile testDir "check" "sh") names'
  let cuts   = map (testFilePath exDir   "scripts"  "ol" ) names
      parses = map (testFilePath testDir "parse"    "txt") names'
      expands = map (testFilePath testDir "expand"   "txt") names'
      outs   = map (testFilePath testDir "stdout"   "txt") names'
      trees  = map (testFilePath testDir "tmpfiles" "txt") names'
      hepts  = zip7 names cuts parses expands outs trees mchecks
      groups = map (mkScriptTests shareDir) hepts
  mkTestGroup cfg ref ids dRef "examples for demo site" groups

removePrefix :: String -> String
removePrefix = last . splitOn ":"

-- from: https://stackoverflow.com/q/47876071
simplify :: String -> String
simplify = filter (`elem` ['a'..'z']) . map toLower

mkTests :: Config -> LocksRef -> IDsRef -> DigestsRef -> IO TestTree
mkTests cfg ref ids dRef = do
  testDir <- getDataFileName "tests"
  exDir   <- getDataFileName "examples"
  let sDir = tmpdir cfg </> "sharedir"
  groups  <- mapM (\mn -> mkTestsPrefix cfg ref ids dRef testDir mn sDir $ Just mn) $
               map (simplify . mName) modules
  exGroup <- mkExampleTests cfg ref ids dRef exDir sDir testDir
  return $ testGroup "run test scripts" $ groups ++ [exGroup]
