module ShortCut.Test.Repl where

import Control.Monad.Trans        (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Repl         (mkRepl)
import ShortCut.Core.Types        (CutConfig(..), ReplM)
import ShortCut.Core.Util         (mkTestGroup)
import System.FilePath.Posix      (takeBaseName, replaceExtension)
import System.IO.Silently         (capture_)
import System.IO.Silently         (silence)
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
mockRepl stdin cfg = capture_ $ mkRepl (map mockPrompt stdin) cfg

-- TODO include goldenTree here too (should pass both at once)
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
  golds <- findByExtension [".txt"] tDir
  let tests = mapM (goldenRepl cfg) golds
      group = testGroup "print expected responses"
  fmap group tests

goldenReplTree :: CutConfig -> FilePath -> IO TestTree
goldenReplTree cfg txt = do
  txt <- readFile txt
  let name   = takeBaseName txt
      tree   = replaceExtension txt "tree"
      stdin  = extractStdin txt
      cmd    = (shell "tree") { cwd = Just $ cfgTmpDir cfg }
      action = do
                 silence $ mockRepl stdin cfg
                 out <- readCreateProcess cmd ""
                 return $ pack out
  putStrLn $ "on txt " ++ txt
  putStrLn $ "looking for " ++ tree
  return $ goldenVsString name tree action

goldenReplTrees :: CutConfig -> IO TestTree
goldenReplTrees cfg = do
  tDir  <- getDataFileName "ShortCut/Test/sessions"
  txts  <- findByExtension [".txt"] tDir
  let tests = mapM (goldenReplTree cfg) txts
      group = testGroup "create expected tmpfiles"
  fmap group tests


-- -- Line goldenScript, except it tests that the proper tree of tmpfiles was
-- -- created instead of the proper result.  Note that the tree file is unrelated
-- -- to the TestTree.
-- -- TODO ensure that tree is installed, or use a more basic command!
-- goldenTree :: CutConfig -> FilePath -> FilePath -> TestTree
-- goldenTree cfg cut tre = goldenVsString name tre act
--   where
--     name = takeBaseName cut
--     cfg' = cfg { cfgScript = Just cut, cfgTmpDir = (cfgTmpDir cfg </> name) }
--     cmd  = (shell "tree") { cwd = Just $ cfgTmpDir cfg' }
--     act  = do
--              silence $ evalFile cfg'
--              out <- readCreateProcess cmd ""
--              -- txt <- readFile tre
--              -- putStrLn txt
--              return $ pack out
-- 
-- -- TODO shit, something about the result numbers is nondeterministic; need to fix to pass these!
-- --      probably because it needs to not rely on the path *to* the shortcut dir, only inside it
-- goldenTrees :: CutConfig -> IO TestTree
-- goldenTrees cfg = do
--   tDir <- getDataFileName "ShortCut/Test/sessions"
--   gFiles <- findByExtension [".tree"] tDir
--   let cuts   = map (\s -> replaceExtension s "txt") gFiles
--       gTests = map (\(s,g) -> goldenTree cfg s g) (zip cuts gFiles)
--   return $ testGroup "produce expected tmpfiles" gTests
