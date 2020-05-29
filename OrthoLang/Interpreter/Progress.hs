{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module OrthoLang.Interpreter.Progress
  where

import Development.Shake
import Control.Concurrent
-- import Data.Foldable
import qualified System.Progress as P
-- import Data.IORef
-- import Data.Monoid
-- import Control.Monad

import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import System.Time.Utils (renderSecs)

import GHC.Conc                   (getNumProcessors)




-- TODO how to update one last time at the end?
-- sample is in milliseconds (1000 = a second)
updateLoop :: Int -> IO a -> IO b
updateLoop delay updateFn = do
  threadDelay delay
  _ <- updateFn
  updateLoop delay updateFn

updateProgress :: P.Meter' EvalProgress -> IO Progress -> IO ()
updateProgress pm iosp = do
  -- putStrLn "updating progress"
  Progress{..} <- iosp -- this is weird, but the types check!
  let d = countBuilt + countSkipped + 1
      t = countBuilt + countSkipped + countTodo
  update <- getCurrentTime
  P.modifyMeter pm (\ep -> ep {epDone = d, epTotal = t, epUpdate = update})
  -- unless (d >= t) $ do
    -- threadDelay $ 1000 * 100
    -- updateProgress pm iosp
  -- return ()

completeProgress :: P.Meter' EvalProgress -> Action ()
completeProgress pm = do
  liftIO $ P.modifyMeter pm (\ep2 -> ep2 {epDone = epTotal ep2})
  -- Exit _ <- command [] "sync" [] -- why is this needed?
  return ()

data EvalProgress = EvalProgress
  { epTitle   :: String

  -- together these two let you get duration,
  -- and epUpdate alone seeds the progress bar animation (if any)
  , epUpdate  :: UTCTime
  , epStart   :: UTCTime

  , epDone    :: Int
  , epTotal   :: Int
  , epThreads :: Int
  , epWidth   :: Int
  , epArrowHead :: Char
  , epArrowShaft :: Char
  }

-- TODO hey should the state just be a Progress{..} itself? seems simpler + more flexible
renderProgress :: EvalProgress -> String
renderProgress EvalProgress{..} = unwords $ [epTitle, "[" ++ arrow ++ "]"] ++ [fraction, time]

  -- this would add a delay before showing the bar for short operations:
  -- (round $ diffUTCTime epUpdate epStart) < (5 :: Int) || epDone == 0 = ""
  -- otherwise = unwords $ [epTitle, "[" ++ arrow ++ "]"] ++ [fraction, time]

  where
    -- details  = if epDone >= epTotal then [] else [fraction]
    time = renderTime epStart epUpdate
    fraction = "[" ++ working ++ "/" ++ show epTotal ++ "]" -- TODO skip fraction when done
    firstTask = min epTotal $ epDone + 1
    lastTask  = min epTotal $ epDone + epThreads
    threads  = if firstTask == lastTask then 1 else lastTask - firstTask + 1
    working  = show firstTask ++ if threads < 2 then "" else "-" ++ show lastTask
    arrowWidth = epWidth - length epTitle - length time - length fraction
    arrowFrac  = ((fromIntegral epDone) :: Double) / (fromIntegral epTotal)
    arrow = if epStart == epUpdate then replicate arrowWidth ' '
            else renderBar arrowWidth threads arrowFrac epArrowShaft epArrowHead

renderTime :: UTCTime -> UTCTime -> String
renderTime start update = renderSecs $ round $ diffUTCTime update start

renderBar :: Int -> Int -> Double -> Char -> Char -> String
renderBar total _ fraction _ _ | fraction == 0 = replicate total ' '
renderBar total nThreads fraction shaftChar headChar = shaft ++ heads ++ blank
  where
    -- hl a = map (\(i, c) -> if (updates + i) `mod` 15 == 0 then '-' else c) $ zip [1..] a
    len     = ceiling $ fraction * fromIntegral total
    shaft   = replicate len shaftChar
    heads   = replicate nThreads headChar
    blank   = replicate (total - len - nThreads - 1) ' '
