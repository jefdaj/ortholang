module ShortCut.Test.Repl where

import Control.Monad.Trans        (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List                  (isPrefixOf)
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Repl         (mkRepl)
import ShortCut.Core.Types        (CutConfig(..), ReplM)
import ShortCut.Core.Util         (mkTestGroup)
import System.Directory           (createDirectoryIfMissing)
import System.FilePath.Posix      (takeBaseName, replaceExtension, (</>))
import System.IO                  (stdout, stderr, withFile, hPutStrLn, IOMode(..), Handle)
import System.IO.Silently         (hCapture_)
import System.Process             (cwd, readCreateProcess, shell)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsString, goldenVsFile, findByExtension)

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "mock REPL interaction"
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
mockRepl :: [String] -> FilePath -> CutConfig -> IO ()
mockRepl stdinLines path cfg = withFile path WriteMode $ \handle -> do
  -- putStrLn ("stdin: '" ++ unlines stdin ++ "'")
  _ <- hCapture_ [stdout, stderr] $ mkRepl (map (mockPrompt handle) stdinLines) (\s -> liftIO $ hPutStrLn handle s) cfg -- TODO write the print-to-file fn
  -- putStrLn $ "stdout: '" ++ out ++ "'"
  return ()


-- TODO convert to using goldenVsFile, where the file is the .out created by the mock repl
-- TODO 
--
-- TODO include goldenTree here too (should pass both at once)
goldenRepl :: CutConfig -> FilePath -> IO TestTree
goldenRepl cfg goldenFile = do
  txt <- readFile goldenFile
  let name   = takeBaseName goldenFile
      cfg'   = cfg { cfgTmpDir = (cfgTmpDir cfg </> name) }
      tstOut = cfgTmpDir cfg' ++ name ++ ".out"
      stdin  = extractPrompted "shortcut >> " txt -- TODO pass the prompt here
      action = mockRepl stdin tstOut cfg'
  return $ goldenVsFile name goldenFile tstOut action

goldenRepls :: CutConfig -> IO TestTree
goldenRepls cfg = do
  tDir  <- getDataFileName "tests/repl"
  golds <- findByExtension [".txt"] tDir
  let tests = mapM (goldenRepl cfg) golds
      group = testGroup "print expected responses"
  fmap group tests

goldenReplTree :: CutConfig -> FilePath -> IO TestTree
goldenReplTree cfg ses = do
  txt <- readFile ses
  let name   = takeBaseName ses
      cfg'   = cfg { cfgTmpDir = (cfgTmpDir cfg </> name) }
      tree   = replaceExtension ses "tree"
      stdin  = extractPrompted "shortcut >> " txt
      tmpDir = cfgTmpDir cfg'
      tmpOut = cfgTmpDir cfg </> name ++ ".out"
      cmd    = (shell "tree") { cwd = Just $ tmpDir }
      action = do
                 _ <- mockRepl stdin tmpOut cfg'
                 createDirectoryIfMissing True tmpDir
                 out <- readCreateProcess cmd ""
                 return $ pack out
  return $ goldenVsString name tree action

goldenReplTrees :: CutConfig -> IO TestTree
goldenReplTrees cfg = do
  tDir  <- getDataFileName "tests/repl"
  txts  <- findByExtension [".txt"] tDir
  let tests = mapM (goldenReplTree cfg) txts
      group = testGroup "create expected tmpfiles"
  fmap group tests
