module ShortCut.Test.Scripts where

import Data.ByteString.Lazy.Char8 (pack, ByteString)
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Eval         (evalFile)
import ShortCut.Core.Types        (CutConfig(..))
import ShortCut.Core.Util         (mkTestGroup)
import System.FilePath.Posix      (replaceExtension, takeBaseName, takeDirectory,
                                   takeFileName, (</>), (<.>))
import System.IO.Silently         (silence)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsStringDiff, findByExtension)
import System.Process             (cwd, readCreateProcess, shell)
import Prelude             hiding (writeFile)
import Data.String.Utils          (replace)
import System.IO                  (stdout, writeFile)
import Data.Default.Class         (Default(def))
import qualified Control.Monad.TaggedException as Exception (handle)
import System.IO.LockFile -- TODO only some of it
import ShortCut.Core.Parse            (parseFileIO)
import ShortCut.Core.Pretty       (writeScript)
import Data.Maybe                     (fromJust)

-- TODO get rid of as many of these as possible
nonDeterministicCut :: FilePath -> Bool
nonDeterministicCut path = testDir `elem` badDirs
  where
    testDir = (takeFileName . takeDirectory) path
    badDirs = ["blast", "crb"]

getTestCuts :: IO [FilePath]
getTestCuts = do
  testDir  <- getDataFileName "tests"
  testCuts <- findByExtension [".cut"] testDir
  return testCuts

-- TODO any particular corner cases to be aware of? (what if inturrupted?)
withLock :: CutConfig -> IO () -> IO ()
withLock cfg act = handleException $ withLockFile def started act
  where
    started  = cfgTmpDir cfg <.> "lock"
    handleException = Exception.handle
        $ putStrLn . ("Locking failed with: " ++) . show

goldenDiff :: String -> FilePath -> IO ByteString -> TestTree
goldenDiff name file action = goldenVsStringDiff name fn file action
  where
    -- this is taken from the Tasty docs
    fn ref new = ["diff", "-u", ref, new]

mkScriptTest :: CutConfig -> FilePath -> TestTree
mkScriptTest cfg gld = goldenDiff "result" gld scriptAct
  where
    scriptRes = (cfgTmpDir cfg </> "vars" </> "result")
    scriptAct = do
      runCut cfg
      res <- readFile scriptRes
      return $ pack $ replace (cfgTmpDir cfg) "$TMPDIR" res

mkTreeTest :: CutConfig -> FilePath -> TestTree
mkTreeTest cfg t = goldenDiff "tmpfiles" t treeAct
  where
    treeCmd = (shell $ "tree") { cwd = Just $ cfgTmpDir cfg }
    treeAct = do
      runCut cfg
      out <- readCreateProcess treeCmd ""
      dir <- fmap (reverse . dropWhile (== '/') . reverse) $ getDataFileName ""
      -- TODO shouldn't I never need this anyway?
      return $ pack $ replace dir "$TESTDIR" out

mkTripTest :: CutConfig -> TestTree
mkTripTest cfg = goldenDiff "round-trip" tripShow tripAct
  where
    tripCut   = cfgTmpDir cfg <.> "cut"
    tripShow  = cfgTmpDir cfg <.> "show"
    tripSetup = do
      scr1 <- parseFileIO cfg $ fromJust $ cfgScript cfg
      writeScript tripCut scr1
      writeFile tripShow $ show scr1
    tripAct = do
      withLock cfg tripSetup
      scr2 <- parseFileIO cfg tripCut
      return $ pack $ show scr2

runCut :: CutConfig -> IO ()
runCut cfg = withLock cfg $ silence $ evalFile stdout cfg

-- TODO is the IO return type needed?
mkScriptTests :: (FilePath, FilePath, (Maybe FilePath)) -> CutConfig -> IO TestTree
mkScriptTests (cut, gld, mtre) cfg = return $ testGroup name allTests
  where
    name     = takeBaseName cut
    cfg'     = cfg { cfgScript = Just cut, cfgTmpDir = (cfgTmpDir cfg </> name) }
    allTests = [mkTripTest cfg', mkScriptTest cfg' gld] ++ treeTest
    treeTest = case mtre of
                 Nothing -> []
                 Just t  -> [mkTreeTest cfg' t]

mkTests :: CutConfig -> IO TestTree
mkTests cfg = do
  cuts <- getTestCuts
  let results = map findResFile  cuts
      mtrees  = map findTreeFile cuts
      triples = zip3 cuts results mtrees
      groups  = map mkScriptTests triples
  mkTestGroup cfg "interpret test scripts" groups
  where
    findResFile  c = replaceExtension c "result"
    findTreeFile c = if nonDeterministicCut c
      then Nothing
      else Just $ replaceExtension c "tree"
