module ShortCut.Core.Repl.Tests where

import ShortCut.Core.Types (CutConfig)
import ShortCut.Core.Util  (mkTestGroup)
import System.IO.Silently  (capture_)
import Test.Tasty          (TestTree)
import Text.Regex          (mkRegex, splitRegex)

import System.Environment
import System.IO

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "Repl" []

-- TODO have to fix eLine before getting anything out of the repl at all!
--      but can work on splitting the repl files first

-- from stackoverflow.com/questions/40297001
-- TODO move to Util.hs?
splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

-- Parse a pasted REPL session (golden string) into initial prompt, text the
-- user typed, and text printed back by shortcut.
splitSession :: String -> ([String], [String])
splitSession txt = (stdout, stdin)
  where
    split  = map (splitAtFirst '\n') $ splitRegex (mkRegex "^shortcut\\ >>\\ ") txt
    prompt = fst (head split) ++ "\n" ++ snd (head split) ++ "shortcut >> "
    stdin  = map fst $ tail split
    stdout = prompt : map (++ "shortcut >> ") (init $ map snd $ tail split)

-- This is for pasting the actual output together for comparison with the
-- golden string. This should be True: txt == joinSession $ splitSession txt
-- TODO wait, you don't need to recreate the whole file!
--      just concat and match the stdouts dumbass
--      they should be available from capture
-- joinSession :: ([String], [String]) -> String
-- joinSession (stdout, stdin) =
--   concat $ map (\(i,o) -> i ++ o) $ zip stdout (map (++ "\n") stdin)
joinStdout :: [String] -> String
joinStdout = concat

-- TODO shit, gotta convert to bytestring
-- TODO should you be approaching the input by mocking getInputT? maybe pass it the [String]?
-- goldenRepl = goldenVsString

-- TODO AH, HERE'S HOW TO REALLY DO IT! ALL THE PIECES ARE HERE:
-- MODIFY THE REPL OR LOOP FUNCTIONS TO TAKE A PARAMETER FOR GETLINE, AND USE THAT TO SEND LINES
-- http://stackoverflow.com/a/19956888
-- (the second part of that might make comparing the whole strings easy after all!)
-- USE CAPTURE_ TO GET STDOUT, THEN CONCAT IT TOGETHER
-- ALSO CONCAT TOGETHER THE GOLDEN STDOUT
-- COMPARE THEM WITH GOLDENVSSTRING
-- OF COURSE, A MAP FUNCTION TO DO THAT WITH ALL THE TESTS

-- TODO then, the plan is:
-- * paste entire repl sessions in "golden" repl files
-- * split them into alternating blocks of stdin, stdout
-- * send stdin to repl and capture stdout
-- * join back together and require that it matches the first string
-- * set tmpdir, but script is Nothing for now
