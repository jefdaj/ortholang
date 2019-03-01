module ShortCut.Core.Random where

import ShortCut.Core.Types
import System.Random

-- TODO any reason not to hardcode this?
initialRandomSeed :: RandomSeed
initialRandomSeed = RandomSeed $ show $ mkStdGen 0

-- takes one random seed and gives you a list of some more
-- used to implement the repeat function
unfoldRandomSeed :: RandomSeed -> Int -> [RandomSeed]
unfoldRandomSeed (RandomSeed s) n
  | n <  0 = error "can't make a negative-length list of random seeds"
  | n == 0 = []
  | otherwise = (RandomSeed $ show first) : unfoldRandomSeed (RandomSeed $ show rest) (n-1)
    where
      gen = (read s) :: StdGen
      (first, rest) = split gen
