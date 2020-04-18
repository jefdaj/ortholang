module OrthoLang.Test.Repl where

-- TODO could the mock repl be implemented more cleanly with Haskeline's Behaviors?

import OrthoLang.Core
import Paths_OrthoLang             (getDataFileName)
import OrthoLang.Util         (readFileStrict)

import System.IO.Temp             (emptySystemTempFile)
import Control.Monad.Trans        (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List                  (isPrefixOf)
import System.Directory           (createDirectoryIfMissing, removeFile) --, copyFile)
import System.FilePath            (splitDirectories, joinPath)
import System.FilePath.Posix      (takeBaseName, replaceExtension, (</>), (<.>))
import System.IO                  (stdout, stderr, withFile, hPutStrLn, IOMode(..), Handle)
import System.IO.Silently         (hCapture_)
import System.Process             (cwd, readCreateProcess, shell)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden          (goldenVsString, goldenVsFile, findByExtension)

import System.Console.Haskeline hiding (catch)

mkTestGroup ::  Config -> LocksRef -> IDsRef -> DigestsRef -> String
            -> [Config -> LocksRef -> IDsRef -> DigestsRef -> IO TestTree] -> IO TestTree
mkTestGroup cfg ref ids dRef name trees = do
  let trees' = mapM (\t -> t cfg ref ids dRef) trees
  trees'' <- trees'
  return $ testGroup name trees''

mkTests :: Config -> LocksRef -> IDsRef -> DigestsRef -> IO TestTree
mkTests cfg ref ids dRef = mkTestGroup cfg ref ids dRef "mock REPL interaction"
                [goldenRepls, goldenReplTrees]

-- returns just the parts of a pasted REPL session that represent user input
extractPrompted :: String -> String -> [String]
extractPrompted prompt session = inputs
  where
    plines = filter (isPrefixOf prompt) (lines session)
    inputs = map (drop $ length prompt) plines

-- For golden testing of REPL sessions. It takes a string with a line of text
-- to inject, then a prompt string like the regular prompt fn. Only injects one
-- line; map it over a list to simulate more.
-- mockPrompt :: Handle -> String -> String -> ReplM (Maybe String)
mockPrompt :: Handle -> String -> String -> InputT ReplM (Maybe String)
mockPrompt handle stdinStr promptStr = do
  liftIO $ hPutStrLn handle $ promptStr ++ stdinStr
  return $ return stdinStr

-- TODO why did this get messed up??
-- For golden testing the repl. Takes stdin as a string and returns stdout.
-- TODO also capture stderr! Care about both equally here
mockRepl :: [String] -> FilePath -> GlobalEnv -> IO ()
mockRepl stdinLines path st@(_, cfg, ref, ids, dRef) = do
  tmpPath <- emptySystemTempFile "mockrepl"
  withFile tmpPath WriteMode $ \handle -> do
    -- putStrLn ("stdin: \"" ++ unlines stdinLines ++ "\"")
    _ <- hCapture_ [stdout, stderr] $ mkRepl (map (mockPrompt handle) stdinLines) handle st
    -- putStrLn $ "stdout: \"" ++ out ++ "\""
    return ()
  out <- readFileStrict ref tmpPath
  writeFile path $ toGeneric cfg out
  removeFile tmpPath
  return ()

-- TODO include goldenTree here too (should pass both at once)
goldenRepl :: Config -> LocksRef -> IDsRef -> DigestsRef -> FilePath -> IO TestTree
goldenRepl cfg ref ids dRef goldenFile = do
  txt <- readFileStrict ref goldenFile -- TODO have to handle unicode here with the new prompt?
  let name   = takeBaseName goldenFile
      desc   = "repl output matches " ++ name <.> "txt"
      cfg'   = cfg { cfgTmpDir = (cfgTmpDir cfg </> name) }
      tstOut = cfgTmpDir cfg' <.> "txt"
      stdin  = extractPrompted ("ortholang" ++ promptArrow) txt -- TODO pass the prompt here
      action = mockRepl stdin tstOut (emptyScript, cfg', ref, ids, dRef)
               -- uncomment to update repl golden files:
               -- >> copyFile tstOut ("/home/jefdaj/ortholang/tests/repl" </> takeBaseName goldenFile <.> "txt")
  return $ goldenVsFile desc goldenFile tstOut action

knownFailing :: [FilePath]
knownFailing =
  [ "repl_fntypes"
  , "repl_glob"
  , "repl_loadlist"
  ]

findGoldenFiles :: IO [FilePath]
findGoldenFiles = do
  testDir  <- getDataFileName "tests/repl"
  txtFiles <- findByExtension [".txt"] testDir
  let txtFiles' = filter (\t -> not $ (takeBaseName t) `elem` knownFailing) txtFiles
  return $ filter (("repl_" `isPrefixOf`) . takeBaseName) $ txtFiles'

goldenRepls :: Config -> LocksRef -> IDsRef -> DigestsRef -> IO TestTree
goldenRepls cfg ref ids dRef = do
  golds <- findGoldenFiles
  let tests = mapM (goldenRepl cfg ref ids dRef) golds
      group = testGroup "prints expected output"
  fmap group tests

goldenReplTree :: Config -> LocksRef -> IDsRef -> DigestsRef -> FilePath -> IO TestTree
goldenReplTree cfg ref ids dRef ses = do
  txt <- readFileStrict ref ses
  let name   = takeBaseName ses
      desc   = name <.> "txt" ++ " creates expected tmpfiles"
      cfg'   = cfg { cfgTmpDir = (cfgTmpDir cfg </> name) }
      -- tree   = replaceExtension (takeDi) "txt"
      tree   = joinPath $ (init $ init $ splitDirectories ses)
                      ++ ["tmpfiles", replaceExtension (takeBaseName ses) "txt"]
      stdin  = extractPrompted promptArrow txt
      tmpDir = cfgTmpDir cfg'
      tmpOut = cfgTmpDir cfg </> name ++ ".out"
      cmd    = (shell "tree -aI '*.lock|*.database'") { cwd = Just $ tmpDir }
      action = do
                 _ <- mockRepl stdin tmpOut (emptyScript, cfg', ref, ids, dRef)
                 createDirectoryIfMissing True tmpDir
                 out <- readCreateProcess cmd ""
                 -- uncomment to update golden repl trees
                 -- writeFile ("/home/jefdaj/ortholang/tests/tmpfiles" </> takeBaseName ses <.> "txt") $ toGeneric cfg out
                 return $ pack $ toGeneric cfg out
  return $ goldenVsString desc tree action

goldenReplTrees :: Config -> LocksRef -> IDsRef -> DigestsRef -> IO TestTree
goldenReplTrees cfg ref ids dRef = do
  txts <- findGoldenFiles
  let tests = mapM (goldenReplTree cfg ref ids dRef) txts
      group = testGroup "repl creates expected tmpfiles"
  fmap group tests
