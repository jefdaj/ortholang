{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

{- ShortCut code is interpreted in phases, but client code shouldn't need to
 - care about that, so this module wraps them in a simplified interface. It
 - just holds whatever [i]nterpret functions the Repl and ShortCut modules use
 - for now rather than any comprehensive API.
 -}

-- TODO ghc --make MyBuildSystem -rtsopts -with-rtsopts=-I0
-- TODO -j (is that done already?)
-- TODO --flush=N

-- TODO should there be an iLine function that tries both expr and assign?
-- TODO create Eval.hs again and move [e]val functions there? might be clearer
--      but then again interpret and eval are kind of the same thing here right?
--      it's either interpret or compile + eval

module ShortCut.Core.Eval
  ( eval
  , evalFile
  , evalScript
  )
  where

import Development.Shake
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import ShortCut.Core.Types
import ShortCut.Core.Pretty (renderIO)
-- import ShortCut.Core.Config (debug)

-- import Control.Applicative ((<>))
import Control.Retry
-- import qualified Data.Map as M
-- import Data.List (isPrefixOf)

import Control.Exception.Safe         (catchAny)
import Data.Maybe                     (maybeToList, isJust, fromMaybe)
import ShortCut.Core.Compile.Basic    (compileScript)
import ShortCut.Core.Parse            (parseFileIO)
import ShortCut.Core.Pretty           (prettyNum)
import ShortCut.Core.Paths            (CutPath, toCutPath, fromCutPath)
import ShortCut.Core.Locks            (withReadLock')
import ShortCut.Core.Sanitize         (unhashIDs, unhashIDsFile)
import ShortCut.Core.Actions          (readLits, readPaths)
import ShortCut.Core.Util             (trace)
import System.IO                      (Handle)
import System.FilePath                ((</>), takeFileName)
import Data.IORef                     (readIORef)
import Control.Monad                  (when)
import GHC.Conc (numCapabilities)
-- import System.Directory               (createDirectoryIfMissing)
-- import Control.Concurrent.Thread.Delay (delay)

import Control.Concurrent
-- import Data.Foldable
import qualified System.Progress as P
-- import Data.IORef
-- import Data.Monoid
-- import Control.Monad

import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import System.Time.Utils (renderSecs)

-- TODO what's an optimal number?
numInterpreterThreads :: Int
numInterpreterThreads = max 1 $ numCapabilities - 1

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
  Exit _ <- command [] "sync" [] -- why is this needed?
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
-- TODO put back renderProgress EvalProgress{..} | epUpdates <  3 = ""
renderProgress EvalProgress{..} = unwords $ [epTitle, "[" ++ arrow ++ "]"] ++ [fraction, time]
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

-- TODO use hashes + dates to decide which files to regenerate?
-- alternatives tells Shake to drop duplicate rules instead of throwing an error
myShake :: CutConfig -> P.Meter' EvalProgress -> Int -> Rules () -> IO ()
myShake cfg pm delay rules = do
  -- ref <- newIORef (return mempty :: IO Progress)
  let shakeOpts = shakeOptions
        { shakeFiles     = cfgTmpDir cfg
        -- , shakeVerbosity = if isJust (cfgDebug cfg) then Chatty else Quiet
        , shakeVerbosity = Quiet
        , shakeThreads   = numInterpreterThreads
        , shakeReport    = [cfgTmpDir cfg </> "profile.html"] ++ maybeToList (cfgReport cfg)
        , shakeAbbreviations = [(cfgTmpDir cfg, "$TMPDIR"), (cfgWorkDir cfg, "$WORKDIR")]
        , shakeChange    = ChangeModtimeAndDigest -- TODO test this
        -- , shakeCommandOptions = [EchoStdout True]
        , shakeProgress = updateLoop delay . updateProgress pm
        -- , shakeShare = cfgShare cfg -- TODO why doesn't this work?
        -- , shakeCloud = ["localhost"] -- TODO why doesn't this work?
        -- , shakeLineBuffering = True
        -- , shakeStaunch = True -- TODO is this a good idea?
        -- , shakeColor = True
        -- TODO shakeShare to implement shared cache on the demo site!
        }

  (shake shakeOpts . alternatives) rules
  -- removeIfExists $ cfgTmpDir cfg </> ".shake.lock"
  --
{- This seems to be separately required to show the final result of eval.
 - It can't be moved to Pretty.hs either because that causes an import cycle.
 -
 - TODO is there a way to get rid of it?
 - TODO rename prettyContents? prettyResult?
 - TODO should this actually open external programs
 - TODO idea for sets: if any element contains "\n", just add blank lines between them
 - TODO clean this up!
 -}
prettyResult :: CutConfig -> Locks -> CutType -> CutPath -> Action Doc
prettyResult _ _ Empty  _ = return $ text "[]"
prettyResult cfg ref (ListOf t) f
  | t `elem` [str, num] = do
    lits <- readLits cfg ref $ fromCutPath cfg f
    let lits' = if t == str
                  then map (\s -> text $ "\"" ++ s ++ "\"") lits
                  else map prettyNum lits
    return $ text "[" <> sep ((punctuate (text ",") lits')) <> text "]"
  | otherwise = do
    paths <- readPaths cfg ref $ fromCutPath cfg f
    pretties <- mapM (prettyResult cfg ref t) paths
    return $ text "[" <> sep ((punctuate (text ",") pretties)) <> text "]"
prettyResult cfg ref (ScoresOf _)  f = do
  s <- liftIO (defaultShow cfg ref $ fromCutPath cfg f)
  return $ text s
prettyResult cfg ref t f = liftIO $ fmap showFn $ (tShow t cfg ref) f'
  where
    showFn = if t == num then prettyNum else text
    f' = fromCutPath cfg f

-- run the result of any of the c* functions, and print it
-- (only compileScript is actually useful outside testing though)
-- TODO rename `runRules` or `runShake`?
-- TODO require a return type just for showing the result?
-- TODO take a variable instead?
-- TODO add a top-level retry here? seems like it would solve the read issues
eval :: Handle -> CutConfig -> Locks -> HashedIDsRef -> CutType -> Rules ResPath -> IO ()
eval hdl cfg ref ids rtype p = do
  start <- getCurrentTime
  let ep = EvalProgress
             { epTitle = takeFileName $ fromMaybe "shortcut" $ cfgScript cfg
             , epStart  = start
             , epUpdate = start
             , epDone    = 0
             , epTotal   = 0
             , epThreads = numInterpreterThreads
             , epWidth = fromMaybe 100 $ cfgWidth cfg
             , epArrowShaft = '—'
             , epArrowHead = '▶'
             }
      delay = 1000000 -- in microseconds
      pOpts = P.Progress
                { progressDelay = delay
                , progressHandle = hdl
                , progressInitial = ep
                , progressRender = if cfgNoProg cfg then (const "") else renderProgress
                }
  if isJust (cfgDebug cfg)
    then ignoreErrors $ eval' delay pOpts p
    else retryIgnore  $ eval' delay pOpts p
  where
    -- This isn't as bad as it sounds. It just prints an error message instead
    -- of crashing the rest of the program. The error will still be visible.
    ignoreErrors fn = catchAny fn (\e -> putStrLn ("error! " ++ show e))

    -- This one is as bad as it sounds, so remove it when able! It's the only
    -- way I've managed to solve the occasional "openFile" lock conflicts.
    -- TODO at least log when a retry happens for debugging
    -- TODO ask Niel if individual actions can be retried instead
    -- TODO could always fork Shake to put it in if needed too
    limitedBackoff = exponentialBackoff 50 <> limitRetries 5
    retryIgnore fn = ignoreErrors
                   $ recoverAll limitedBackoff
                   $ report fn

    -- Reports how many failures so far and runs the main fn normally
    -- TODO putStrLn rather than debug?
    report fn status = case rsIterNumber status of
      0 -> fn
      n -> trace "core.eval.eval" ("error! eval failed " ++ show n ++ " times") fn

    eval' delay pOpts rpath = P.withProgress pOpts $ \pm -> myShake cfg pm delay $ do
      (ResPath path) <- rpath
      want ["eval"]
      "eval" ~> do
        alwaysRerun
        actionRetry 9 $ need [path] -- TODO is this done automatically in the case of result?
        {- if --interactive, print the short version of a result
         - if --output, save the full result (may also be --interactive)
         - if neither, print the full result
         - TODO move this logic to the top level?
         -}
        -- ids' <- liftIO $ readIORef ids
        when (cfgInteractive cfg) (printShort cfg ref ids pm rtype path)
        completeProgress pm
        case cfgOutFile cfg of
          Just out -> writeResult cfg ref ids (toCutPath cfg path) out
          Nothing  -> when (not $ cfgInteractive cfg) (printLong cfg ref ids pm path)
        return ()

writeResult :: CutConfig -> Locks -> HashedIDsRef -> CutPath -> FilePath -> Action ()
writeResult cfg ref idsref path out = do
  -- liftIO $ putStrLn $ "writing result to '" ++ out ++ "'"
  unhashIDsFile cfg ref idsref path out

-- TODO what happens when the txt is a binary plot image?
printLong :: CutConfig -> Locks -> HashedIDsRef -> P.Meter' EvalProgress -> FilePath -> Action ()
printLong _ ref idsref pm path = do
  ids <- liftIO $ readIORef idsref
  txt <- withReadLock' ref path $ readFile' path
  let txt' = unhashIDs False ids txt
  liftIO $ P.putMsgLn pm ("\n" ++ txt')

printShort :: CutConfig -> Locks -> HashedIDsRef -> P.Meter' EvalProgress -> CutType -> FilePath -> Action ()
printShort cfg ref idsref pm rtype path = do
  ids <- liftIO $ readIORef idsref
  res  <- prettyResult cfg ref rtype $ toCutPath cfg path
  -- liftIO $ putStrLn $ show ids
  -- liftIO $ putStrLn $ "rendering with unhashIDs (" ++ show (length $ M.keys ids) ++ " keys)..."
  -- TODO fix the bug that causes this to remove newlines after seqids:
  res' <- fmap (unhashIDs False ids) $ liftIO $ renderIO cfg res -- TODO why doesn't this handle a str.list?
  liftIO $ P.putMsgLn pm res'
  -- liftIO $ putStrLn $ "done rendering with unhashIDs"

-- TODO get the type of result and pass to eval
evalScript :: Handle -> CutState -> IO ()
evalScript hdl s@(as, c, ref, ids) = case lookupResult as of
  Nothing  -> putStrLn "no result variable during eval. that's not right!"
  Just res -> eval hdl c ref ids (typeOf res) (compileScript s $ ReplaceID Nothing)

evalFile :: Handle -> CutConfig -> Locks -> HashedIDsRef -> IO ()
evalFile hdl cfg ref ids = case cfgScript cfg of
  Nothing  -> putStrLn "no script during eval. that's not right!"
  Just scr -> do
    s <- parseFileIO cfg ref ids scr -- TODO just take a CutState?
    evalScript hdl (s, cfg, ref, ids)
