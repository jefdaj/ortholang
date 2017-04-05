module ShortCut.Test.Repl where

import Control.Monad.Trans        (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Repl         (repl)
import ShortCut.Core.Types        (CutConfig, ReplM)
import ShortCut.Core.Util         (mkTestGroup)
import System.FilePath.Posix      (takeBaseName)
import System.IO.Silently         (capture_)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsString, findByExtension)
import Text.Regex                 (mkRegex, splitRegex)

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "Repl" [goldenRepls]

-- TODO have to fix eLine before getting anything out of the repl at all!
--      but can work on splitting the repl files first

-- from stackoverflow.com/questions/40297001
-- TODO move to Util.hs?
splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

-- Parse a pasted REPL session (golden string) into initial prompt, text the
-- user typed, and text printed back by shortcut.
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
mockRepl stdin cfg = capture_ $ repl (map mockPrompt stdin) cfg

goldenRepl :: CutConfig -> FilePath -> IO TestTree
goldenRepl cfg path = do
  txt <- readFile path
  let name   = takeBaseName path
      stdin  = extractStdin txt
      action = fmap pack $ mockRepl stdin cfg
  return $ goldenVsString name path action

goldenRepls :: CutConfig -> IO TestTree
goldenRepls cfg = do
  tDir  <- getDataFileName "ShortCut/Test/sessions"
  golds <- findByExtension [".golden"] tDir
  let tests = mapM (goldenRepl cfg) golds
      group = testGroup "reproduce test sessions"
  fmap group tests
