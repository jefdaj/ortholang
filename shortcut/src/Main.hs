module Main where

import ShortCut.Repl (repl)
-- import ShortCut.Interpret -- TODO if given a file, run iFile + eval on it

main:: IO ()
main = repl

-- TODO add Docopt and write a docstring for arg handling
-- TODO add flags to separately parse, check, and eval files for testing
-- TODO add tests to the main source tree
-- TODO add a compiler option to bake tests in with a flag?
-- TODO if no commands given, enter the REPL
