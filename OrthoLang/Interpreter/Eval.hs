{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

{- OrthoLang code is interpreted in phases, but client code shouldn't need to
 - care about that, so this module wraps them in a simplified interface. It
 - just holds whatever [i]nterpret functions the Repl and OrthoLang modules use
 - for now rather than any comprehensive API.
 -}

-- TODO ghc --make MyBuildSystem -rtsopts -with-rtsopts=-I0
-- TODO -j (is that done already?)
-- TODO --flush=N

-- TODO should there be an iLine function that tries both expr and assign?
-- TODO create Eval.hs again and move [e]val functions there? might be clearer
--      but then again interpret and eval are kind of the same thing here right?
--      it's either interpret or compile + eval

module OrthoLang.Interpreter.Eval
  ( eval
  , evalFile
  , evalScript
  )
  where

import OrthoLang.Debug
import OrthoLang.Errors (oneLineShakeErrors)
import Development.Shake
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import OrthoLang.Types
import OrthoLang.Interpreter.Expand (expandMacros)
import OrthoLang.Interpreter.Pretty (renderIO)
-- import OrthoLang.Config (debug)

-- import Control.Applicative ((<>))
import qualified Data.HashMap.Strict as M
import Data.Dynamic (Dynamic, toDyn, dynTypeRep)
import Data.Typeable (TypeRep)
-- import Data.List (isPrefixOf)

import Data.Maybe                     (maybeToList, isJust, fromMaybe, fromJust)
import OrthoLang.Interpreter.Compile         (compileScript, rExpr, newRules)
import OrthoLang.Interpreter.Parse            (parseFileIO)
import OrthoLang.Interpreter.Pretty           (prettyNum)
import OrthoLang.Interpreter.Paths            (Path, toPath, fromPath)
import OrthoLang.Locks            (withReadLock')
import OrthoLang.Interpreter.Sanitize         (unhashIDs, unhashIDsFile)
import OrthoLang.Interpreter.Actions          (readLits, readPaths)
-- import OrthoLang.Util             (ignoreErrors)
import System.IO                      (Handle)
import System.FilePath                ((</>), takeFileName)
import Data.IORef                     (readIORef)
import Control.Monad                  (when)
-- import System.Directory               (createDirectoryIfMissing)
-- import Control.Concurrent.Thread.Delay (delay)
-- import Control.Retry          (rsIterNumber)
import Control.Exception.Safe (catchAny)

import Control.Concurrent
-- import Data.Foldable
import qualified System.Progress as P
-- import Data.IORef
-- import Data.Monoid
-- import Control.Monad

import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import System.Time.Utils (renderSecs)

import GHC.Conc                   (getNumProcessors)

-- import qualified Data.Text.Lazy as T
-- import Text.Pretty.Simple (pShowNoColor)

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
renderProgress EvalProgress{..}
  | (round $ diffUTCTime epUpdate epStart) < (5 :: Int) || epDone == 0 = ""
  | otherwise = unwords $ [epTitle, "[" ++ arrow ++ "]"] ++ [fraction, time]
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
myShake :: Config -> LocksRef -> IDsRef -> DigestsRef
        -> P.Meter' EvalProgress -> Int -> Rules () -> IO ()
myShake cfg ref ids dr pm delay rules = do
  -- ref <- newIORef (return mempty :: IO Progress)
  nproc <- getNumProcessors
  let tDir = tmpdir cfg
      shakeOpts = shakeOptions
        { shakeFiles     = tDir
        , shakeVerbosity = Quiet -- TODO can you make it  but sent only to the logfile?
        , shakeThreads   = nproc
        , shakeReport    = [tDir </> "profile.html"] ++ maybeToList (report cfg)
        , shakeAbbreviations = [(tDir, "$TMPDIR"), (workdir cfg, "$WORKDIR")]
        , shakeChange    = ChangeModtimeAndDigestInput
        -- , shakeCommandOptions = [EchoStdout True]
        , shakeProgress = updateLoop delay . updateProgress pm
        -- , shakeShare = sharedir cfg -- TODO why doesn't this work?
        -- , shakeCloud = ["localhost"] -- TODO why doesn't this work?
        -- , shakeLineBuffering = True
        -- , shakeStaunch = True -- TODO is this a good idea?
        -- , shakeColor = True
        -- TODO shakeShare to implement shared cache on the demo site!
        , shakeExtra = shakeEnv cfg ref ids dr

        -- This prints annoying errors whenever a file is accessed unexpectedly
        -- TODO remove ignore patterns as you solve them
        -- TODO is there a difference between relative and absolute paths as shake keys?
        , shakeLint = Just LintFSATrace
        , shakeLintInside =
            [ tDir </> "exprs"
            , tDir </> "cache"
            ]
        , shakeLintIgnore =
            [ "//cache/bin/*"
            , "//cache/blastdbget/dblist.txt"
            , "//cache/concat/*.txt"
            , "//cache/curl/*"
            , "//cache/each//args"
            , "//cache/hmmsearch/result"
            , "//cache/lines/*.txt"
            , "//cache/load/*.f*a"
            , "//cache/load/*.f*a.ids"
            , "//cache/load/*.gbk"
            , "//cache/load/*.str.list"
            , "//cache/makeblastdb//result*"
            , "//cache/map//result"
            , "//cache/map//result.tmp"
            , "//cache/mmseqs/createdb//result*"
            , "//cache/mmseqs/search/*.mmseqs2db.*"
            , "//cache/mmseqs/search/tmp"
            , "//cache/orthogroups/*.txt"
            , "//cache/plots/*_lists.txt"
            , "//cache/plots/*_names.txt"
            , "//cache/split_faa/result.tmp"
            , "//cache/split_faa_each/result.tmp"
            , "//exprs/best_hits//result"
            , "//exprs/blast*_db//out"
            , "//exprs/concat_f*a//result"
            , "//exprs/diamond_blast*//result"
            , "//exprs/diamond_makedb//result"
            , "//exprs/extract_ids//result"
            , "//exprs/extract_queries//tmp"
            , "//exprs/extract_scored//tmp"
            , "//exprs/extract_scores//tmp"
            , "//exprs/extract_seqs//result"
            , "//exprs/extract_targets//tmp"
            , "//exprs/filter_*//result"
            , "//exprs/gbk_to_*_rawids//result"
            , "//exprs/hmmbuild//result"
            , "//exprs/hmmsearch//result"
            , "//exprs/list//result"
            , "//exprs/load_*//*.f*"
            , "//exprs/megablast_db//out"
            , "//exprs/muscle//result"
            , "//exprs/ortholog_*//result"
            , "//exprs/psiblast*//result"
            , "//exprs/psiblast*//result.tmp"
            , "//exprs/range_integers//result"
            , "//exprs/reciprocal_best//result"
            , "//exprs/tblast*//out"
            , "//exprs/translate//result"
            ]
        }

  oneLineShakeErrors "core.eval.myShake" $ (shake shakeOpts . alternatives) rules
  -- removeIfExists $ tmpdir cfg </> ".shake.lock"
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
prettyResult :: Config -> LocksRef -> Type -> Path -> Action Doc
prettyResult _ _ Empty  _ = return $ text "[]"
prettyResult cfg ref (ListOf t) f
  | t `elem` [str, num] = do
    lits <- readLits "core.eval.prettyResult" $ fromPath loc cfg f
    let lits' = if t == str
                  then map (\s -> text $ "\"" ++ s ++ "\"") lits
                  else map prettyNum lits
    return $ text "[" <> sep ((punctuate (text ",") lits')) <> text "]"
  | otherwise = do
    paths <- readPaths "core.eval.prettyResult" $ fromPath loc cfg f
    pretties <- mapM (prettyResult cfg ref t) paths
    return $ text "[" <> sep ((punctuate (text ",") pretties)) <> text "]"
  where
    loc = "core.eval.eval.prettyResult"
prettyResult cfg ref (ScoresOf _)  f = do
  s <- liftIO (defaultShow cfg ref $ fromPath loc cfg f)
  return $ text s
  where
    loc = "core.eval.eval.prettyResult"

-- TODO case for EncodedAs here, and later redesign this as a typeclass
prettyResult cfg ref (EncodedAs e _)  f = liftIO $ fmap text $ enShow e cfg ref f'
  where
    f' = fromPath loc cfg f
    loc = "core.eval.eval.prettyResult"

prettyResult cfg ref t f = liftIO $ fmap showFn $ (tShow t cfg ref) f'
  where
    showFn = if t == num then prettyNum else text
    f' = fromPath loc cfg f
    loc = "core.eval.eval.prettyResult"

-- run the result of any of the c* functions, and print it
-- (only compileScript is actually useful outside testing though)
-- TODO rename `runRules` or `runShake`?
-- TODO require a return type just for showing the result?
-- TODO take a variable instead?
-- TODO add a top-level retry here? seems like it would solve the read issues
eval :: Handle -> Config -> LocksRef -> IDsRef -> DigestsRef -> Type -> Rules ResPath -> IO ()
eval hdl cfg ref ids dr rtype p = do
  start <- getCurrentTime
  nproc <- getNumProcessors
  let ep = EvalProgress
             { epTitle = takeFileName $ fromMaybe "ortholang" $ script cfg
             , epStart  = start
             , epUpdate = start
             , epDone    = 0
             , epTotal   = 0
             , epThreads = nproc
             , epWidth = fromMaybe 80 $ termcolumns cfg
             , epArrowShaft = '—'
             , epArrowHead = '▶'
             }
      delay = 100000 -- in microseconds
      pOpts = P.Progress
                { progressDelay = delay
                , progressHandle = hdl
                , progressInitial = ep
                , progressRender = if progressbar cfg then (const "") else renderProgress
                }
  eval' delay pOpts p -- TODO ignoreErrors again?
  where
    eval' delay pOpts rpath = P.withProgress pOpts $ \pm -> myShake cfg ref ids dr pm delay $ do
      let loc = "core.eval.eval.eval'"
      newRules
      (ResPath path) <- rpath
      want ["eval"]
      "eval" ~> do
        alwaysRerun
        need ["reloadids"] -- this re-loads any existing cache/load/*.ids files
        -- TODO remove retry after newrules are finished
        actionRetry 9 $ do
          need [path] -- TODO is this done automatically in the case of result?
        {- if --interactive, print the short version of a result
         - if --output, save the full result (may also be --interactive)
         - if neither, print the full result
         - TODO move this logic to the top level?
         -}
        -- ids' <- liftIO $ readIORef ids
        when (interactive cfg) (printShort cfg ref ids pm rtype path)
        completeProgress pm
        case outfile cfg of
          Just out -> writeResult cfg ref ids (toPath loc cfg path) out
          Nothing  -> when (not $ interactive cfg)
                        -- TODO printLong should work more like printShort but no line limit?
                        -- (printLong cfg ref ids pm rtype path)
                        (printShort cfg ref ids pm rtype path)

shakeEnv :: Config -> LocksRef -> IDsRef -> DigestsRef -> M.HashMap TypeRep Dynamic
shakeEnv cfg lRef iRef dRef =
  M.fromList $ map (\v -> (dynTypeRep v, v))
    [ toDyn cfg
    , toDyn lRef
    , toDyn iRef
    , toDyn dRef
    ]

writeResult :: Config -> LocksRef -> IDsRef -> Path -> FilePath -> Action ()
writeResult cfg ref idsref path out = do
  -- liftIO $ putStrLn $ "writing result to \"" ++ out ++ "\""
  -- (dRef :: DigestsRef) <- fmap fromJust $ getShakeExtra
  -- dMap <- liftIO $ readIORef dRef
  -- liftIO $ putStrLn $ "here are all the current ids:\n" ++ T.unpack (pShowNoColor dMap)
  unhashIDsFile path out

-- TODO what happens when the txt is a binary plot image?
-- TODO is this where the diamond makedb error comes in?
-- TODO have to tShow this right?
printLong :: Config -> LocksRef -> IDsRef -> P.Meter' EvalProgress -> Type -> FilePath -> Action ()
printLong _ ref idsref pm _ path = do
  ids <- liftIO $ readIORef idsref
  txt <- withReadLock' path $ readFile' path
  let txt' = unhashIDs True ids txt
  liftIO $ P.putMsgLn pm ("\n" ++ txt')

printShort :: Config -> LocksRef -> IDsRef -> P.Meter' EvalProgress -> Type -> FilePath -> Action ()
printShort cfg ref idsref pm rtype path = do
  ids <- liftIO $ readIORef idsref
  let loc = "core.eval.printShort"
  res <- prettyResult cfg ref rtype $ toPath loc cfg path
  -- liftIO $ putStrLn $ show ids
  -- liftIO $ putStrLn $ "rendering with unhashIDs (" ++ show (length $ M.keys ids) ++ " keys)..."
  -- TODO fix the bug that causes this to remove newlines after seqids:
  -- liftIO $ putStrLn $ "here are all the current ids:\n" ++ T.unpack (pShowNoColor ids)
  res' <- fmap (unhashIDs False ids) $ liftIO $ renderIO cfg res -- TODO why doesn't this handle a str.list?
  liftIO $ P.putMsgLn pm res'
  -- liftIO $ putStrLn $ "done rendering with unhashIDs"

-- TODO get the type of result and pass to eval
evalScript :: Handle -> GlobalEnv -> IO ()
evalScript hdl (scr, c, ref, ids, dRef) =
  let scr'  = expandMacros c scr
      scr'' = trace "ortholang.core.eval.evalScript" ("after macro expansion: " ++ unlines (map show scr')) scr'
      res = case lookupResult scr'' of
              Nothing -> fromJust $ lookupResult $ ensureResult scr''
              Just r  -> r
  in eval hdl c ref ids dRef (typeOf res) (compileScript $ seq scr'' scr'')

-- TODO should there be a new idsref for this? how about digestsref?
evalFile :: GlobalEnv -> Handle -> IO ()
evalFile st@(_, cfg, ref, ids, dRef) hdl = case script cfg of
  Nothing  -> putStrLn "no script during eval. that's not right!"
  Just scr -> do
    s <- parseFileIO st scr -- TODO just take a GlobalEnv?
    evalScript hdl (s, cfg, ref, ids, dRef)
