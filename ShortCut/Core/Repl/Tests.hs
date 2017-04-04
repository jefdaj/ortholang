module ShortCut.Core.Repl.Tests where

import ShortCut.Core.Types (CutConfig)
import ShortCut.Core.Util  (mkTestGroup)
import Test.Tasty          (TestTree)
import Text.Regex          (mkRegex, splitRegex)

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "Repl" []

-- TODO have to fix eLine before getting anything out of the repl at all!
--      but can work on splitting the repl files first

-- from stackoverflow.com/questions/40297001
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
joinSession :: ([String], [String]) -> String
joinSession (stdout, stdin) =
  concat $ map (\(i,o) -> i ++ o) $ zip stdout (map (++ "\n") stdin)

-- TODO then, the plan is:
-- * paste entire repl sessions in "golden" repl files
-- * split them into alternating blocks of stdin, stdout
-- * send stdin to repl and capture stdout
-- * join back together and require that it matches the first string
-- * set tmpdir, but script is Nothing for now
