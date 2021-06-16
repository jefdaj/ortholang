{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


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

import OrthoLang.Errors (oneLineShakeErrors)
import Development.Shake
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import OrthoLang.Types
import OrthoLang.Interpreter.Progress (EvalProgress(..), myShakeProgress, initProgress, completeProgress)
import OrthoLang.Script (expandMacros)
-- import OrthoLang.Interpreter.Pretty (renderIO)
import OrthoLang.Interpreter.Config (os)

-- import Control.Applicative ((<>))
import qualified Data.HashMap.Strict as M
import Data.Dynamic (Dynamic, toDyn, dynTypeRep)
import Data.Typeable (TypeRep)
-- import Data.List (isPrefixOf)

import Data.Maybe                     (maybeToList)
import OrthoLang.Interpreter.Compile         (compileScript, newRules)
import OrthoLang.Interpreter.Parse            (parseFileIO)
import OrthoLang.Interpreter.Paths            (Path, toPath, fromPath, toGeneric)
import OrthoLang.Locks            (withReadLock, withReadLock')
import OrthoLang.Interpreter.Sanitize         (unhashIDs, unhashIDsFile)
import OrthoLang.Interpreter.Actions          (readLits, readPaths)
import OrthoLang.Util             (ignoreErrors, resolveSymlinks)
import System.IO                      (Handle)
import System.FilePath                ((</>))
import Data.IORef                     (readIORef)
-- import Control.Monad                  (when)
-- import System.Directory               (createDirectoryIfMissing)
-- import Control.Concurrent.Thread.Delay (delay)
-- import Control.Retry          (rsIterNumber)
-- import Control.Exception.Safe (catchAny)

-- import Control.Concurrent
-- import Data.Foldable
import qualified System.Progress as P
-- import Data.IORef
-- import Data.Monoid
-- import Control.Monad

-- import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
-- import System.Time.Utils (renderSecs)

import System.Process (readProcess)
import Data.Time (UTCTime(..), getCurrentTime, Day(..))

import GHC.Conc                   (getNumProcessors)

getToday :: IO Day
getToday = do
  (UTCTime today _) <- getCurrentTime
  return today

-- import qualified Data.Text.Lazy as T
-- import Text.Pretty.Simple (pShowNoColor)

-- TODO use hashes + dates to decide which files to regenerate?
-- alternatives tells Shake to drop duplicate rules instead of throwing an error
myShake :: [Module] -> Config -> LocksRef -> IDsRef -> DigestsRef
        -> P.Meter' EvalProgress -> Rules () -> IO ()
myShake mods cfg ref ids dr pm rules = do
  -- ref <- newIORef (return mempty :: IO Progress)
  nproc <- getNumProcessors
  day <- getToday
  let tDir = tmpdir cfg
      shakeOpts = shakeOptions
        { shakeFiles     = tDir
        , shakeVerbosity = Quiet -- TODO can you make it  but sent only to the logfile?
        , shakeThreads   = nproc
        , shakeReport    = (tDir </> "profile.html") : maybeToList (report cfg)
        , shakeAbbreviations = [(tDir, "$TMPDIR"), (workdir cfg, "$WORKDIR")]
        , shakeChange    = ChangeModtimeAndDigestInput
        -- , shakeCommandOptions = [EchoStdout True]
        , shakeProgress = myShakeProgress pm
        -- , shakeShare = sharedir cfg -- TODO why doesn't this work?
        -- , shakeCloud = ["localhost"] -- TODO why doesn't this work?
        -- , shakeLineBuffering = True
        -- , shakeStaunch = True -- TODO is this a good idea?
        -- , shakeColor = True
        -- TODO shakeShare to implement shared cache on the demo site!
        , shakeExtra = shakeEnv mods cfg ref ids dr day

        -- This prints annoying errors whenever a file is accessed unexpectedly
        -- TODO remove ignore patterns as you solve them
        -- TODO is there a difference between relative and absolute paths as shake keys?
        -- fsatrace not available on mac
        -- , shakeLint = if os == "linux" then (Just LintFSATrace) else Nothing
        --
        -- and either way, we don't want it on for releases:
        -- , shakeLint = Just LintFSATrace
        , shakeLint = Nothing
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
            , "//cache/each//result"
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
            , "//exprs/extract_queries//result"
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
            , "//exprs/range_*//result"
            , "//exprs/reciprocal_best//result"
            , "//exprs/tblast*//out"
            , "//exprs/translate//result"
            , "//exprs/makeblastdb_*_all//result"
            , "//exprs/makeblastdb_*_all_each//result"
            , "//exprs/*blast*_db//result"
            , "//exprs/blastp_db//out"
            , "//exprs/blastp_db//result"
            , "//exprs/*blast*_db_each//result"
            , "//exprs/length//result"
            ]
        }

  oneLineShakeErrors "interpreter.eval.myShake" $ (shake shakeOpts . alternatives) rules
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
    lits <- readLits "interpreter.eval.prettyResult" $ fromPath loc cfg f
    let lits' = if t == str
                  then map (\s -> text $ "\"" ++ s ++ "\"") lits
                  else map prettyNum lits
    return $ text "[" <> sep (punctuate (text ",") lits') <> text "]"
  | otherwise = do
    paths <- readPaths "interpreter.eval.prettyResult" $ fromPath loc cfg f
    pretties <- mapM (prettyResult cfg ref t) paths
    return $ text "[" <> sep (punctuate (text ",") pretties) <> text "]"
  where
    loc = "interpreter.eval.eval.prettyResult"
prettyResult cfg ref (ScoresOf _)  f = do
  s <- liftIO (defaultShow cfg ref $ fromPath loc cfg f)
  return $ text s
  where
    loc = "interpreter.eval.eval.prettyResult"

-- TODO case for EncodedAs here, and later redesign this as a typeclass
prettyResult cfg ref (EncodedAs e _)  f = liftIO $ text <$> enShow e cfg ref f'
  where
    f' = fromPath loc cfg f
    loc = "interpreter.eval.eval.prettyResult"

prettyResult cfg ref Untyped path = do
  let loc = "interpreter.eval.eval.prettyResult"
  path' <- liftIO $ resolveSymlinks Nothing $ fromPath loc cfg path
  out <- fmap init $ liftIO $ withReadLock ref path' $ readProcess "file" [path'] []
  return $ text $ "Untyped file " ++ toGeneric cfg out

prettyResult cfg ref t f = liftIO $ showFn <$> tShow t cfg ref f'
  where
    showFn = if t == num then prettyNum else text
    f' = fromPath loc cfg f
    loc = "interpreter.eval.eval.prettyResult"

-- run the result of any of the c* functions, and print it
-- (only compileScript is actually useful outside testing though)
-- TODO rename `runRules` or `runShake`?
-- TODO require a return type just for showing the result?
-- TODO take a variable instead?
-- TODO add a top-level retry here? seems like it would solve the read issues
eval :: [Module] -> Handle -> Config -> LocksRef -> IDsRef -> DigestsRef -> Type -> Rules ResPath -> IO ()
eval mods hdl cfg ref ids dr rtype p = do
  po <- initProgress cfg hdl
  ignoreErrors $ eval' po p -- TODO ignoreErrors again?
  where
    eval' o rpath = P.withProgress o $ \pm -> myShake mods cfg ref ids dr pm $ do
      newRules mods
      (ResPath path) <- rpath
      let loc = "interpreter.eval.eval.eval'"
      -- runs the actual script, links vars/result to hashed output
      "eval" ~> do
        alwaysRerun
        need ["reloadids"]
        -- actionRetry 9 $ need [path]
        need [path] -- TODO need'?
        completeProgress pm -- TODO only when doing the progressbar?
      -- writes unhashed output to outfile
      "outfile" ~> case outfile cfg of
        Nothing  -> return ()
        Just out -> need ["eval"] >> writeResult cfg ref ids (toPath loc cfg path) out
      -- prints unhashed output
      "head" ~> (alwaysRerun >> need ["eval"] >> printShort cfg ref ids pm rtype path)
      "cat"  ~> (alwaysRerun >> need ["eval"] >> printLong  cfg ref ids pm rtype path)
      want $ case outfile cfg of
               Nothing -> if interactive cfg then ["head"] else ["cat"]
               Just _  -> if interactive cfg then ["outfile", "head"] else ["outfile"]

shakeEnv :: [Module] -> Config -> LocksRef -> IDsRef -> DigestsRef -> Day -> M.HashMap TypeRep Dynamic
shakeEnv mods cfg lRef iRef dRef day =
  M.fromList $ map (\v -> (dynTypeRep v, v))
    [ toDyn mods
    , toDyn cfg
    , toDyn lRef
    , toDyn iRef
    , toDyn dRef
    , toDyn day
    ]

writeResult :: Config -> LocksRef -> IDsRef -> Path -> FilePath -> Action ()
writeResult cfg ref idsref path out = do
  -- liftIO $ putStrLn $ "writing result to \"" ++ out ++ "\""
  -- (dRef :: DigestsRef) <- fmap fromJust $ getShakeExtra
  -- dMap <- liftIO $ readIORef dRef
  -- liftIO $ putStrLn $ "here are all the current ids:\n" ++ T.unpack (pShowNoColor dMap)
  unhashIDsFile path out
  trackWrite [out] -- TODO ' version?

-- TODO what happens when the txt is a binary plot image?
-- TODO is this where the diamond makedb error comes in?
-- TODO have to tShow this right?
printLong :: Config -> LocksRef -> IDsRef -> P.Meter' EvalProgress -> Type -> FilePath -> Action ()
printLong _ ref idsref pm _ path = do
  ids <- liftIO $ readIORef idsref
  txt <- withReadLock' path $ readFile' path
  let txt' = unhashIDs True ids txt
  liftIO $ P.putMsg pm txt'

printShort :: Config -> LocksRef -> IDsRef -> P.Meter' EvalProgress -> Type -> FilePath -> Action ()
printShort cfg ref idsref pm rtype path = do
  ids <- liftIO $ readIORef idsref
  let loc = "interpreter.eval.printShort"
  res <- prettyResult cfg ref rtype $ toPath loc cfg path
  -- liftIO $ putStrLn $ show ids
  -- liftIO $ putStrLn $ "rendering with unhashIDs (" ++ show (length $ M.keys ids) ++ " keys)..."
  -- TODO fix the bug that causes this to remove newlines after seqids:
  -- liftIO $ putStrLn $ "here are all the current ids:\n" ++ T.unpack (pShowNoColor ids)
  res' <- fmap (unhashIDs False ids) $ liftIO $ renderIO cfg res -- TODO why doesn't this handle a str.list?
  liftIO $ P.putMsgLn pm $ res' ++ "\n"
  -- liftIO $ putStrLn $ "done rendering with unhashIDs"

-- TODO get the type of result and pass to eval
evalScript :: [Module] -> Handle -> GlobalEnv -> IO ()
evalScript mods hdl (scr, c, ref, ids, dRef) =
  let scr' = expandMacros mods scr c dRef
      -- res  = sResult scr'
      -- as'  = sAssigns scr'
      -- as'' = trace "ortholang.core.eval.evalScript" ("after macro expansion: " ++ unlines (map show as')) as'
      -- res = case lookupResultAssign of
              -- Nothing -> fromJust $ lookupResultAssign $ ensureResult as''
              -- Just r  -> r
  in case sResult scr' of
    Nothing -> return () -- TODO print something?
    Just re -> eval mods hdl c ref ids dRef (typeOf re) $ compileScript scr'

-- TODO should there be a new idsref for this? how about digestsref?
evalFile :: [Module] -> GlobalEnv -> Handle -> IO ()
evalFile mods st@(_, cfg, ref, ids, dRef) hdl = case script cfg of
  Nothing  -> putStrLn "no script during eval. that's not right!"
  Just scr -> do
    s <- parseFileIO mods st scr -- TODO just take a GlobalEnv?
    evalScript mods hdl (s, cfg, ref, ids, dRef)
