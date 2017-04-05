module ShortCut.Core.Repl.Tests where

import Data.ByteString.Lazy.Char8 (pack)
import Paths_ShortCut          (getDataFileName)
import ShortCut.Core.Repl    (repl')
import ShortCut.Core.Types   (CutConfig, Repl, print)
import Control.Monad.State.Lazy  (liftIO) -- TODO import from somewhere more normal
import ShortCut.Core.Util    (mkTestGroup)
import System.FilePath.Posix (takeBaseName)
import System.IO.Silently    (capture_)
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.Golden     (goldenVsString, findByExtension)
import Text.Regex            (mkRegex, splitRegex)

import System.Environment
import System.IO hiding (print)
import Prelude hiding (print)

-- import Debug.Trace -- TODO remove

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
-- TODO only keep stdin, since stdout isn't being used later?
-- extractStdin :: String -> ([String], [String])
-- extractStdin txt = (stdout, stdin)
-- TODO factor out txt?
-- TODO use a function like lineStartsWith and take the prompt as an argument?
extractStdin :: String -> [String]
-- extractStdin txt = map fst $ map (splitAtFirst '\n') $ tail $ splitRegex regex txt
extractStdin txt = tail $ map fst split
  where
    regex = mkRegex "^shortcut\\ >>\\ "
    split = map (splitAtFirst '\n') $ splitRegex regex txt
    -- prompt = fst (head split) ++ "\n" ++ snd (head split) ++ "shortcut >> "
    -- stdin  = map fst split
    -- stdout = prompt : map (++ "shortcut >> ") (init $ map snd $ tail split)

-- This is for pasting the actual output together for comparison with the
-- golden string. This should be True: txt == joinSession $ extractStdin txt
-- TODO wait, you don't need to recreate the whole file!
--      just concat and match the stdouts dumbass
--      they should be available from capture
-- joinSession :: ([String], [String]) -> String
-- joinSession (stdout, stdin) =
--   concat $ map (\(i,o) -> i ++ o) $ zip stdout (map (++ "\n") stdin)

-- TODO Do you still need this for anything?
-- http://stackoverflow.com/a/19956888

-- This is for golden testing of REPL sessions. It takes a string with a line
-- of text to inject, then a prompt string like the regular prompt fn. Only
-- injects one line; map it over a list to simulate more.
mockPrompt :: String -> String -> Repl (Maybe String)
mockPrompt stdinStr promptStr = do
  print $ promptStr ++ stdinStr
  return $ return stdinStr

-- For golden testing the repl. Takes stdin as a string and returns stdout.
-- mockRepl :: [String] -> CutConfig -> IO ()
-- mockRepl stdin cfg = repl' (map mockPrompt stdin) cfg
mockRepl :: [String] -> CutConfig -> IO String
mockRepl stdin cfg = capture_ $ repl' (map mockPrompt stdin) cfg
  -- where
    -- TODO remove this if it's printing things in duplicate...
    --      and if not, figure out why the other print still isn't working!
    -- fn s1 s2 = mockPrompt s1 s2

goldenRepl :: CutConfig -> FilePath -> IO TestTree
goldenRepl cfg path = do
  txt <- readFile path
  let name   = takeBaseName path
      stdin  = extractStdin txt
      action = fmap pack $ mockRepl stdin cfg
  return $ goldenVsString name path action

goldenRepls :: CutConfig -> IO TestTree
goldenRepls cfg = do
  tDir  <- getDataFileName "ShortCut/Core/Repl/tests"
  golds <- findByExtension [".golden"] tDir
  let tests = mapM (goldenRepl cfg) golds
      group = testGroup "reproduce test sessions"
  fmap group tests
