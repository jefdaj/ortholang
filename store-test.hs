module Main where

import Data.Store
import Data.Map.Strict as M
import Data.ByteString as BS

test :: M.Map String String
test = M.fromList
  [ ("seqid_one", "1")
  , ("seqid_two", "2")
  , ("seqid_three", "3")
  , ("seqid_four", "4")
  , ("seqid_five", "5")
  ]

save :: FilePath -> M.Map String String -> IO ()
save path m = BS.writeFile path $ encode m

load :: FilePath -> IO (Map String String)
load path = BS.readFile path >>= decodeIO

main :: IO ()
main = do
  Prelude.putStrLn "testing save..."
  save "test.map" test
  Prelude.putStrLn "testing load..."
  m2 <- load "test.map"
  print m2
  return ()
