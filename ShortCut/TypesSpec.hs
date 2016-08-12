module ShortCut.TypesSpec where

import ShortCut.Types
import Test.Hspec
import Control.Monad.Trans (liftIO)
import Data.List        (isSuffixOf)
import System.Directory (getDirectoryContents)
import System.FilePath  (combine)

loadExamples :: String -> IO [String]
loadExamples suffix = do
  let exDir = "examples"
  paths <- getDirectoryContents exDir
  let paths' = map (combine exDir) $ filter (isSuffixOf suffix) paths
  mapM readFile paths'

readASTs :: IO [ParsedScript]
readASTs = do
  asts <- loadExamples ".ast"
  return $ map read asts

spec :: Spec
spec = do
  describe "readASTs" $
    it "reads handwritten example ASTs" $ do
      asts <- liftIO readASTs
      (length asts) `shouldBe` 1
