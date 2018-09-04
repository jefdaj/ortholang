module ShortCut.Test.Repl where

-- TODO could the mock repl be implemented more cleanly with Haskeline's Behaviors?

import System.IO.Temp             (emptySystemTempFile)
import ShortCut.Core.Paths        (toGeneric)
import Control.Monad.Trans        (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List                  (isPrefixOf)
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Repl         (mkRepl)
import ShortCut.Core.Util         (readFileStrict)
import ShortCut.Core.Types        (CutConfig(..), Locks, ReplM)
import System.Directory           (createDirectoryIfMissing, removeFile)
import System.FilePath.Posix      (takeBaseName, replaceExtension, (</>), (<.>))
import System.IO                  (stdout, stderr, withFile, hPutStrLn, IOMode(..), Handle)
import System.IO.Silently         (hCapture_)
import System.Process             (cwd, readCreateProcess, shell)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsString, goldenVsFile, findByExtension)

mkTestGroup ::  CutConfig -> Locks -> String
            -> [CutConfig -> Locks -> IO TestTree] -> IO TestTree
mkTestGroup cfg ref name trees = do
  let trees' = mapM (\t -> t cfg ref) trees
  trees'' <- trees'
  return $ testGroup name trees''

mkTests :: CutConfig -> Locks -> IO TestTree
mkTests cfg ref = mkTestGroup cfg ref "mock REPL interaction"
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
mockRepl :: [String] -> FilePath -> CutConfig -> Locks -> IO ()
mockRepl stdinLines path cfg ref = do
  tmpPath <- emptySystemTempFile "mockrepl"
  withFile tmpPath WriteMode $ \handle -> do
    -- putStrLn ("stdin: '" ++ unlines stdin ++ "'")
    _ <- hCapture_ [stdout, stderr] $ mkRepl (map (mockPrompt handle) stdinLines) handle cfg ref
    -- putStrLn $ "stdout: '" ++ out ++ "'"
    return ()
  out <- readFile tmpPath
  writeFile path $ toGeneric cfg out
  removeFile tmpPath
  return ()


-- TODO convert to using goldenVsFile, where the file is the .out created by the mock repl
-- TODO 
--
-- TODO include goldenTree here too (should pass both at once)
goldenRepl :: CutConfig -> Locks -> FilePath -> IO TestTree
goldenRepl cfg ref goldenFile = do
  txt <- readFileStrict ref goldenFile
  let name   = takeBaseName goldenFile
      cfg'   = cfg { cfgTmpDir = (cfgTmpDir cfg </> name) }
      -- tstOut = cfgTmpDir cfg' ++ name ++ ".out"
      tstOut = cfgTmpDir cfg' <.> "out"
      stdin  = extractPrompted ">> " txt -- TODO pass the prompt here
      action = mockRepl stdin tstOut cfg' ref
  return $ goldenVsFile name goldenFile tstOut action

goldenRepls :: CutConfig -> Locks -> IO TestTree
goldenRepls cfg ref = do
  tDir  <- getDataFileName "tests/repl"
  golds <- findByExtension [".txt"] tDir
  let tests = mapM (goldenRepl cfg ref) golds
      group = testGroup "prints expected output"
  fmap group tests

goldenReplTree :: CutConfig -> Locks -> FilePath -> IO TestTree
goldenReplTree cfg ref ses = do
  txt <- readFileStrict ref ses
  let name   = takeBaseName ses
      cfg'   = cfg { cfgTmpDir = (cfgTmpDir cfg </> name) }
      tree   = replaceExtension ses "tree"
      stdin  = extractPrompted ">> " txt
      tmpDir = cfgTmpDir cfg'
      tmpOut = cfgTmpDir cfg </> name ++ ".out"
      cmd    = (shell "tree -aI '*.lock|*.database'") { cwd = Just $ tmpDir }
      action = do
                 _ <- mockRepl stdin tmpOut cfg' ref
                 createDirectoryIfMissing True tmpDir
                 out <- readCreateProcess cmd ""
                 return $ pack $ toGeneric cfg out
  return $ goldenVsString name tree action

goldenReplTrees :: CutConfig -> Locks -> IO TestTree
goldenReplTrees cfg ref = do
  tDir  <- getDataFileName "tests/repl"
  txts  <- findByExtension [".txt"] tDir
  let tests = mapM (goldenReplTree cfg ref) txts
      group = testGroup "creates expected tmpfiles"
  fmap group tests
