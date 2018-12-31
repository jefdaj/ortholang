module Detourrr.Test.Repl where

-- TODO could the mock repl be implemented more cleanly with Haskeline's Behaviors?

import System.IO.Temp             (emptySystemTempFile)
import Detourrr.Core.Paths        (toGeneric)
import Control.Monad.Trans        (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List                  (isPrefixOf)
import Paths_Detourrr             (getDataFileName)
import Detourrr.Core.Repl         (mkRepl)
import Detourrr.Core.Util         (readFileStrict)
import Detourrr.Core.Types        (CutConfig(..), Locks, ReplM, HashedSeqIDsRef)
import System.Directory           (createDirectoryIfMissing, removeFile) --, copyFile)
import System.FilePath.Posix      (takeBaseName, replaceExtension, (</>), (<.>))
import System.IO                  (stdout, stderr, withFile, hPutStrLn, IOMode(..), Handle)
import System.IO.Silently         (hCapture_)
import System.Process             (cwd, readCreateProcess, shell)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsString, goldenVsFile, findByExtension)

mkTestGroup ::  CutConfig -> Locks -> HashedSeqIDsRef -> String
            -> [CutConfig -> Locks -> HashedSeqIDsRef -> IO TestTree] -> IO TestTree
mkTestGroup cfg ref ids name trees = do
  let trees' = mapM (\t -> t cfg ref ids) trees
  trees'' <- trees'
  return $ testGroup name trees''

mkTests :: CutConfig -> Locks -> HashedSeqIDsRef -> IO TestTree
mkTests cfg ref ids = mkTestGroup cfg ref ids "mock REPL interaction"
                [goldenRepls, goldenReplTrees]

-- TODO have to fix eLine before getting anything out of the repl at all!
--      but can work on splitting the repl files first

-- returns just the parts of a pasted REPL session that represent user input
extractPrompted :: String -> String -> [String]
extractPrompted prompt session = inputs
  where
    plines = filter (isPrefixOf prompt) (lines session)
    inputs = map (drop $ length prompt) plines

-- For golden testing of REPL sessions. It takes a string with a line of text
-- to inject, then a prompt string like the regular prompt fn. Only injects one
-- line; map it over a list to simulate more.
mockPrompt :: Handle -> String -> String -> ReplM (Maybe String)
mockPrompt handle stdinStr promptStr = do
  liftIO $ hPutStrLn handle $ promptStr ++ stdinStr
  return $ return stdinStr

-- For golden testing the repl. Takes stdin as a string and returns stdout.
-- TODO also capture stderr! Care about both equally here
mockRepl :: [String] -> FilePath -> CutConfig -> Locks -> HashedSeqIDsRef -> IO ()
mockRepl stdinLines path cfg ref ids = do
  tmpPath <- emptySystemTempFile "mockrepl"
  withFile tmpPath WriteMode $ \handle -> do
    -- putStrLn ("stdin: '" ++ unlines stdin ++ "'")
    _ <- hCapture_ [stdout, stderr] $ mkRepl (map (mockPrompt handle) stdinLines) handle cfg ref ids
    -- putStrLn $ "stdout: '" ++ out ++ "'"
    return ()
  out <- readFile tmpPath -- TODO have to handle unicode here with the new prompt?
  writeFile path $ toGeneric cfg out
  -- this is sometimes helpful when developing tests:
  -- copyFile tmpPath $ "/tmp" </> takeBaseName path
  removeFile tmpPath
  return ()


-- TODO convert to using goldenVsFile, where the file is the .out created by the mock repl
-- TODO 
--
-- TODO include goldenTree here too (should pass both at once)
goldenRepl :: CutConfig -> Locks -> HashedSeqIDsRef -> FilePath -> IO TestTree
goldenRepl cfg ref ids goldenFile = do
  txt <- readFileStrict ref goldenFile -- TODO have to handle unicode here with the new prompt?
  let name   = takeBaseName goldenFile
      desc   = "repl output matches " ++ name <.> "txt"
      cfg'   = cfg { cfgTmpDir = (cfgTmpDir cfg </> name) }
      -- tstOut = cfgTmpDir cfg' ++ name ++ ".out"
      tstOut = cfgTmpDir cfg' <.> "out"
      stdin  = extractPrompted ">> " txt -- TODO pass the prompt here
      action = mockRepl stdin tstOut cfg' ref ids
  return $ goldenVsFile desc goldenFile tstOut action

goldenRepls :: CutConfig -> Locks -> HashedSeqIDsRef -> IO TestTree
goldenRepls cfg ref ids = do
  tDir  <- getDataFileName "tests/repl"
  golds <- findByExtension [".txt"] tDir
  let tests = mapM (goldenRepl cfg ref ids) golds
      group = testGroup "prints expected output"
  fmap group tests

goldenReplTree :: CutConfig -> Locks -> HashedSeqIDsRef -> FilePath -> IO TestTree
goldenReplTree cfg ref ids ses = do
  txt <- readFileStrict ref ses
  let name   = takeBaseName ses
      desc   = name <.> "txt" ++ " creates expected tmpfiles"
      cfg'   = cfg { cfgTmpDir = (cfgTmpDir cfg </> name) }
      tree   = replaceExtension ses "tree"
      stdin  = extractPrompted ">> " txt
      tmpDir = cfgTmpDir cfg'
      tmpOut = cfgTmpDir cfg </> name ++ ".out"
      cmd    = (shell "tree -aI '*.lock|*.database'") { cwd = Just $ tmpDir }
      action = do
                 _ <- mockRepl stdin tmpOut cfg' ref ids
                 createDirectoryIfMissing True tmpDir
                 out <- readCreateProcess cmd ""
                 return $ pack $ toGeneric cfg out
  return $ goldenVsString desc tree action

goldenReplTrees :: CutConfig -> Locks -> HashedSeqIDsRef -> IO TestTree
goldenReplTrees cfg ref ids = do
  tDir  <- getDataFileName "tests/repl"
  txts  <- findByExtension [".txt"] tDir
  let tests = mapM (goldenReplTree cfg ref ids) txts
      group = testGroup "repl creates expected tmpfiles"
  fmap group tests
