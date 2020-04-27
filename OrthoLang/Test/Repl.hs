module OrthoLang.Test.Repl where

-- TODO could the mock repl be implemented more cleanly with Haskeline's Behaviors?

import OrthoLang.Types
import OrthoLang.Modules     (modules)
import OrthoLang.Interpreter (mkRepl, toGeneric, promptArrow)
import Paths_OrthoLang             (getDataFileName)
import OrthoLang.Util         (readFileStrict)
import OrthoLang.Test.Scripts (mkTestGroup, mkTreeTest)

import System.IO.Temp             (emptySystemTempFile)
import Control.Monad.Trans        (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List                  (isPrefixOf)
import Data.List.Utils            (replace)
import System.Directory           (createDirectoryIfMissing, removeFile, copyFile)
import System.FilePath            (splitDirectories, joinPath)
import System.FilePath.Posix      (takeBaseName, replaceExtension, (</>), (<.>))
import System.IO                  (stdout, stderr, withFile, hPutStrLn, IOMode(..), Handle)
import System.IO.Silently         (hCapture_)
import System.Process             (cwd, readCreateProcess, shell)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsString, goldenVsFile, findByExtension)

import System.Console.Haskeline hiding (catch)

mkTests :: Config -> LocksRef -> IDsRef -> DigestsRef -> IO TestTree
mkTests cfg ref ids dRef = mkTestGroup cfg' ref ids dRef d [goldenRepls, goldenReplTrees]
  where
    d = "mock REPL interaction"
    cfg' = cfg {interactive = True}

-- returns just the parts of a pasted REPL session that represent user input
extractPrompted :: String -> String -> [String]
extractPrompted prompt session = inputs
  where
    plines = filter (isPrefixOf prompt) (lines session)
    inputs = map (drop $ length prompt) plines

-- For golden testing of REPL sessions. It takes a string with a line of text
-- to inject, then a prompt string like the regular prompt fn. Only injects one
-- line; map it over a list to simulate more.
-- mockPrompt :: Handle -> String -> String -> ReplM (Maybe String)
mockPrompt :: Handle -> String -> String -> InputT ReplM (Maybe String)
mockPrompt handle stdinStr promptStr = do
  liftIO $ hPutStrLn handle $ promptStr ++ stdinStr
  return $ return stdinStr

-- TODO why did this get messed up??
-- For golden testing the repl. Takes stdin as a string and returns stdout.
-- TODO also capture stderr! Care about both equally here
mockRepl :: [String] -> FilePath -> GlobalEnv -> IO ()
mockRepl stdinLines path st@(_, cfg, ref, ids, dRef) = do
  tmpPath <- emptySystemTempFile "mockrepl"
  withFile tmpPath WriteMode $ \h -> do
    -- putStrLn ("stdin: \"" ++ unlines stdinLines ++ "\"")
    _ <- hCapture_ [stdout, stderr] $ mkRepl modules (map (mockPrompt h) stdinLines) h st
    -- putStrLn $ "stdout: \"" ++ out ++ "\""
    return ()
  out <- readFileStrict ref tmpPath
  writeFile path $ toGeneric cfg out
  removeFile tmpPath
  return ()

-- TODO include goldenTree here too (should pass both at once)
goldenRepl :: Config -> LocksRef -> IDsRef -> DigestsRef -> FilePath -> IO TestTree
goldenRepl cfg ref ids dRef goldenFile = do
  txt <- readFileStrict ref goldenFile -- TODO have to handle unicode here with the new prompt?
  let name   = takeBaseName goldenFile
      d      = "repl output matches " ++ name <.> "txt"
      cfg'   = cfg { tmpdir = (tmpdir cfg </> name) }
      tstOut = tmpdir cfg' <.> "txt"
      stdin  = extractPrompted ("ortholang" ++ promptArrow) txt -- TODO pass the prompt here
      action = mockRepl stdin tstOut (emptyScript, cfg', ref, ids, dRef)
               -- uncomment to update repl golden files:
               -- >> copyFile tstOut ("YOUR_ORTHOLANG_REPO_HERE/tests/repl" </> takeBaseName goldenFile <.> "txt")
  return $ goldenVsFile d goldenFile tstOut action

knownFailing :: [FilePath]
knownFailing =
  [ "repl:fntypes"
  , "repl:glob"
  , "repl:loadlist"
  ]

findGoldenFiles :: IO [FilePath]
findGoldenFiles = do
  testDir  <- getDataFileName "tests/repl"
  txtFiles <- findByExtension [".txt"] testDir
  let txtFiles' = filter (\t -> not $ (takeBaseName t) `elem` knownFailing) txtFiles
  return txtFiles'

goldenRepls :: Config -> LocksRef -> IDsRef -> DigestsRef -> IO TestTree
goldenRepls cfg ref ids dRef = do
  golds <- findGoldenFiles
  let tests = mapM (goldenRepl cfg ref ids dRef) golds
      group = testGroup "prints expected output"
  fmap group tests

goldenReplTree :: Config -> LocksRef -> IDsRef -> DigestsRef -> FilePath -> TestTree
goldenReplTree cfg ref ids dRef ses = mkTreeTest cfg' ref ids dRef name gtAct tre
  where
    tre  = replace "tests/repl" "tests/tmpfiles" ses
    name = takeBaseName ses
    cfg' = cfg { tmpdir = (tmpdir cfg </> name) }
    gtAct _ r i d = do
      txt <- readFileStrict r ses
      let stdin  = extractPrompted promptArrow txt
          tmpOut = tmpdir cfg </> name ++ ".out" -- TODO remove?
      mockRepl stdin tmpOut (emptyScript, cfg, r, i, d)
  
goldenReplTrees :: Config -> LocksRef -> IDsRef -> DigestsRef -> IO TestTree
goldenReplTrees cfg ref ids dRef = do
  txts <- findGoldenFiles
  let tests = map (goldenReplTree cfg ref ids dRef) txts
      group = testGroup "repl creates expected tmpfiles" tests
  return group
