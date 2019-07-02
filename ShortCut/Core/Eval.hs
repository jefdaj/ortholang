{-# LANGUAGE FlexibleContexts #-}

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
  , evalIntermediateExpr
  )
  where

import Development.Shake
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import ShortCut.Core.Types
import ShortCut.Core.Pretty (renderIO)
import ShortCut.Core.Config (debug)

-- import Control.Applicative ((<>))
import Control.Retry
-- import qualified Data.Map as M

-- import Debug.Trace

import Control.Exception.Safe         (catchAny)
import Data.Maybe                     (maybeToList)
import ShortCut.Core.Compile.Basic    (compileScript, rExpr)
import ShortCut.Core.Parse            (parseFileIO)
import ShortCut.Core.Pretty           (prettyNum)
import ShortCut.Core.Paths            (CutPath, toCutPath, fromCutPath, exprPath)
import ShortCut.Core.Locks            (withReadLock')
import ShortCut.Core.Sanitize         (unhashIDs, unhashIDsFile)
import ShortCut.Core.Actions          (readLits, readPaths)
import System.IO                      (Handle, hPutStrLn)
import System.FilePath                ((</>))
import Data.IORef                     (readIORef)
import Control.Monad                  (when)
-- import Control.Concurrent.Thread.Delay (delay)

-- TODO use hashes + dates to decide which files to regenerate?
-- alternatives tells Shake to drop duplicate rules instead of throwing an error
myShake :: CutConfig -> Rules () -> IO ()
myShake cfg rules = do
  (shake myOpts . alternatives) rules
  -- removeIfExists $ cfgTmpDir cfg </> ".shake.lock"
  where
    myOpts = shakeOptions
      { shakeFiles     = cfgTmpDir cfg
      , shakeVerbosity = if cfgDebug cfg then Chatty else Quiet
      , shakeThreads   = 0 -- TODO make customizable? increase?
      , shakeReport    = [cfgTmpDir cfg </> "profile.html"] ++ maybeToList (cfgReport cfg)
      , shakeAbbreviations = [(cfgTmpDir cfg, "$TMPDIR"), (cfgWorkDir cfg, "$WORKDIR")]
      -- , shakeChange    = ChangeModtimeAndDigest -- TODO test this
      -- , shakeCommandOptions = [EchoStdout True]
      -- , shakeProgress = progressSimple
      -- , shakeLineBuffering = True
      -- , shakeStaunch = True
      -- , shakeColor = True
      -- TODO shakeShare to implement shared cache on the demo site!
      }

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
eval hdl cfg ref ids rtype = if cfgDebug cfg
  then ignoreErrors . eval'
  else retryIgnore  . eval'
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
      n -> debug cfg ("error! eval failed " ++ show n ++ " times") fn

    eval' rpath = myShake cfg $ do
      (ResPath path) <- rpath
      want ["eval"]
      "eval" ~> do
        alwaysRerun
        actionRetry 9 $ need [path] -- TODO is this done automatically in the case of result?
        ids' <- liftIO $ readIORef ids
        {- if --interactive, print the short version of a result
         - if --output, save the full result (may also be --interactive)
         - if neither, print the full result
         - TODO move this logic to the top level?
         -}
        when (cfgInteractive cfg) (printShort cfg ref ids' hdl rtype path)
        case cfgOutFile cfg of
          Just out -> writeResult cfg ref ids' (toCutPath cfg path) out
          Nothing  -> when (not $ cfgInteractive cfg) (printLong cfg ref ids' hdl path)

writeResult :: CutConfig -> Locks -> HashedIDs -> CutPath -> FilePath -> Action ()
writeResult cfg ref ids path out = unhashIDsFile cfg ref ids path out

-- TODO what happens when the txt is a binary plot image?
printLong :: CutConfig -> Locks -> HashedIDs -> Handle -> FilePath -> Action ()
printLong cfg ref ids hdl path = do
  txt <- withReadLock' ref path $ readFile' path
  let txt' = unhashIDs False ids txt
  liftIO $ hPutStrLn hdl txt'

printShort :: CutConfig -> Locks -> HashedIDs -> Handle -> CutType -> FilePath -> Action ()
printShort cfg ref ids hdl rtype path = do
  res  <- prettyResult cfg ref rtype $ toCutPath cfg path
  -- liftIO $ putStrLn $ show ids
  -- liftIO $ putStrLn $ "rendering with unhashIDs (" ++ show (length $ M.keys ids) ++ " keys)..."
  -- TODO fix the bug that causes this to remove newlines after seqids:
  res' <- fmap (unhashIDs False ids) $ liftIO $ renderIO cfg res -- TODO why doesn't this handle a str.list?
  liftIO $ hPutStrLn hdl res'
  -- liftIO $ putStrLn $ "done rendering with unhashIDs"

{- A hacky (attempted) solution to the inability to use results of function
 - calls as inputs to the repeat* and replace* functions: pass the raw
 - expression into the Action monad and separately evaluate it inside. Will it
 - work? Sounds plausible... No rules or anything needed here I think.
 - TODO do i need to pass the handle in from above?
 - TODO factor out the common code with eval above?
 -}
evalIntermediateExpr :: CutState -> CutExpr -> IO CutPath
evalIntermediateExpr st@(_, cfg, _, _) expr = do
  myShake cfg $ do
    (ExprPath path) <- rExpr st expr
    want ["evalIntermediateExpr"]
    "evalIntermediateExpr" ~> do
      alwaysRerun
      actionRetry 9 $ need [path]
  return $ exprPath st expr

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
