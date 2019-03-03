module ShortCut.Test.Scripts where

import Prelude hiding (writeFile)

import Control.Concurrent.Thread.Delay (delay)
import Control.Monad              (when)
import Data.ByteString.Lazy.Char8 (pack, ByteString)
import Data.List                  (isPrefixOf)
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

-- TODO remove in favor of shell scripts that test specific properties of tmpfiles
nonDeterministicCut :: FilePath -> Bool
nonDeterministicCut path = any (\s -> s `isPrefixOf` (takeBaseName path)) badSigns
  where
    -- TODO will regular blast be nondeterministic at large scales too?
    -- TODO make these individual tests, or substrings
    -- TODO eventually replace deterministic or not with custom shell predicates
    badSigns = ["crb_blast", "blastrbh", "blasthits", "plot", "mmseqs"] -- TODO blast? blastdb?

knownFailing :: [FilePath]
knownFailing =

  -- TODO test + ask if mmseqs can be compiled on the server
  -- TODO test only when it's installed?
  -- [ "mmseqs_createdb"
  -- , "mmseqs_createdb_all"
  -- , "mmseqs_search"
  -- , "mmseqs_search_db"

  [ "sonicparanoid_basic"

  -- TODO get this to run on the server! (can it be done?)
  , "mmseqs_createdb"
  , "mmseqs_createdb_all"
  , "mmseqs_search"
  , "mmseqs_search_db"

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

getTestScripts :: IO [FilePath]
getTestScripts = do
  testDir  <- getDataFileName "tests"
  testCuts <- findByExtension [".cut"] testDir -- TODO remove known failing scripts here
  let testCuts' = filter (\p -> not $ (takeBaseName p) `elem` knownFailing) testCuts
  return testCuts'

goldenDiff :: String -> FilePath -> IO ByteString -> TestTree
goldenDiff name file action = goldenVsStringDiff name fn file action
  where
    -- this is taken from the Tasty docs
    fn ref new = ["diff", "-u", ref, new]

mkOutTest :: CutConfig -> Locks -> HashedSeqIDsRef -> FilePath -> TestTree
mkOutTest cfg ref ids gld = goldenDiff desc gld scriptAct
  where
    -- TODO put toGeneric back here? or avoid paths in output altogether?
    scriptAct = do
      out <- runCut cfg ref ids
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
    treeCmd = (shell $ "tree -aI '*.lock|*.database|*.log|*.tmp|*.html|lines' | " ++ sedCmd)
                { cwd = Just $ cfgTmpDir cfg }
    treeAct = do
      _ <- runCut cfg ref ids
      out <- readCreateProcess treeCmd ""
      -- sometimes useful for debugging tests:
      -- writeFile ("/tmp" </> takeBaseName t <.> "txt") out
      return $ pack $ toGeneric cfg out

-- TODO use safe writes here
mkTripTest :: CutConfig -> Locks -> HashedSeqIDsRef -> TestTree
mkTripTest cfg ref ids = goldenDiff desc tripShow tripAct
  where
    desc = "unchanged by round-trip to file"
    tripCut = cfgTmpDir cfg <.> "cut"
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
    desc = "tmpfiles free of absolute paths"
    absArgs = [cfgTmpDir cfg, cfgTmpDir cfg </> "exprs", "-R"]
    absGrep = do
      _ <- runCut cfg ref ids
      (_, out, err) <- readProcessWithExitCode "grep" absArgs ""
      return $ toGeneric cfg $ out ++ err

runCut :: CutConfig -> Locks -> HashedSeqIDsRef -> IO String
runCut cfg ref ids =  do
  delay 100000 -- wait 0.1 second so we don't capture output from tasty (TODO is that long enough?)
  (out, ()) <- hCapture [stdout, stderr] $ evalFile stdout cfg ref ids
  delay 100000 -- wait 0.1 second so we don't capture output from tasty (TODO is that long enough?)
  result <- doesFileExist $ cfgTmpDir cfg </> "vars" </> "result"
  when (not result) (fail out)
  return $ toGeneric cfg out

mkScriptTests :: (FilePath, FilePath, Maybe FilePath)
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
  cuts <- getTestScripts
  let outs     = map findOutFile  cuts
      mtrees   = map findTreeFile cuts
      triples  = zip3 cuts outs mtrees
      groups   = map mkScriptTests triples
  mkTestGroup cfg ref ids "interpret test scripts" groups
  where
    -- findOutFile  c = takeDirectory c </> replaceExtension c "txt"
    findOutFile c = joinPath $ (init $ init $ splitDirectories c)
                      ++ ["stdout", replaceExtension (takeBaseName c) "txt"]
    findTreeFile c = if nonDeterministicCut c
      then Nothing
      else Just $ joinPath $ (init $ init $ splitDirectories c)
                    ++ ["tmpfiles", replaceExtension (takeBaseName c) "txt"]
