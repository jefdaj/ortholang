module ShortCut.Test.Repl where

import Control.Monad.Trans        (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
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
import Text.Regex                 (mkRegex, splitRegex)

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "Repl" [goldenRepls, goldenReplTrees]

-- TODO have to fix eLine before getting anything out of the repl at all!
--      but can work on splitting the repl files first

-- from stackoverflow.com/questions/40297001
-- TODO move to Util.hs?
splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

-- Extract text the user typed from a pasted REPL session (golden string)
-- TODO use a function like lineStartsWith and take the prompt as an argument?
extractStdin :: String -> [String]
extractStdin txt = tail $ map fst split
  where
    regex = mkRegex "^shortcut\\ >>\\ "
    split = map (splitAtFirst '\n') $ splitRegex regex txt

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
mockRepl stdin cfg = hCapture_ [stdout, stderr] $ mkRepl (map mockPrompt stdin) cfg

-- TODO include goldenTree here too (should pass both at once)
goldenRepl :: CutConfig -> FilePath -> IO TestTree
goldenRepl cfg path = do
  txt <- readFile path
  let name   = takeBaseName path
      cfg'   = cfg { cfgTmpDir = (cfgTmpDir cfg </> name) }
      stdin  = extractStdin txt
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
      stdin  = extractStdin txt
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
