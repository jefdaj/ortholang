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
import System.IO                  (stdout, stderr)
import System.IO.Silently         (hCapture_)
import System.Process             (cwd, readCreateProcess, shell)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsString, findByExtension)

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "Repl" [goldenRepls, goldenReplTrees]

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
mockPrompt :: String -> String -> ReplM (Maybe String)
mockPrompt stdinStr promptStr = do
  liftIO $ putStrLn $ promptStr ++ stdinStr
  return $ return stdinStr

-- For golden testing the repl. Takes stdin as a string and returns stdout.
-- TODO also capture stderr! Care about both equally here
mockRepl :: [String] -> CutConfig -> IO String
mockRepl stdin cfg = do
  -- putStrLn ("stdin: '" ++ unlines stdin ++ "'")
  out <- hCapture_ [stdout, stderr] $ mkRepl (map mockPrompt stdin) cfg
  -- putStrLn $ "stdout: '" ++ out ++ "'"
  return out

-- TODO include goldenTree here too (should pass both at once)
goldenRepl :: CutConfig -> FilePath -> IO TestTree
goldenRepl cfg path = do
  txt <- readFile path
  let name   = takeBaseName path
      cfg'   = cfg { cfgTmpDir = (cfgTmpDir cfg </> name) }
      stdin  = extractPrompted "shortcut >> " txt -- TODO pass the prompt here
      action = fmap pack $ mockRepl stdin cfg'
  return $ goldenVsString name path action

goldenRepls :: CutConfig -> IO TestTree
goldenRepls cfg = do
  tDir  <- getDataFileName "ShortCut/Test/repl"
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
      stdin  = extractPrompted "shortcut >> " txt -- TODO pass prompt here
      tmpDir = cfgTmpDir cfg'
      cmd    = (shell "tree") { cwd = Just $ tmpDir }
      action = do
                 mockRepl stdin cfg'
                 createDirectoryIfMissing True tmpDir
                 out <- readCreateProcess cmd ""
                 return $ pack out
  return $ goldenVsString name tree action

goldenReplTrees :: CutConfig -> IO TestTree
goldenReplTrees cfg = do
  tDir  <- getDataFileName "ShortCut/Test/repl"
  txts  <- findByExtension [".txt"] tDir
  let tests = mapM (goldenReplTree cfg) txts
      group = testGroup "create expected tmpfiles"
  fmap group tests
