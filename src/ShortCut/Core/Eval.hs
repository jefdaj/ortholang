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
  )
  where

import Development.Shake
import Text.PrettyPrint.HughesPJClass

import ShortCut.Core.Types
import ShortCut.Core.Pretty (renderIO)
import ShortCut.Core.Config (debug)

import Control.Retry

import Control.Exception.Enclosed     (catchAny)
import Data.Maybe                     (maybeToList)
import ShortCut.Core.Compile.Basic    (compileScript)
import ShortCut.Core.Parse            (parseFileIO)
import ShortCut.Core.Pretty           (prettyNum)
import ShortCut.Core.Paths            (CutPath, toCutPath, fromCutPath)
-- import ShortCut.Core.Locks            (withReadLock')
import ShortCut.Core.Actions          (readLits, readPaths)
import System.IO                      (Handle, hPutStrLn)
-- import Data.IORef                     (IORef)

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
      , shakeReport    = maybeToList $ cfgReport cfg
      -- , shakeChange    = ChangeModtimeAndDigest -- TODO test this
      -- , shakeCommandOptions = [EchoStdout True]
      -- , shakeProgress = progressSimple
      -- , shakeLineBuffering = False
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
  s <- liftIO (defaultShow ref $ fromCutPath cfg f)
  return $ text s
prettyResult cfg ref t f = liftIO $ fmap showFn $ (tShow t ref) f'
  where
    showFn = if t == num then prettyNum else text
    f' = fromCutPath cfg f

-- run the result of any of the c* functions, and print it
-- (only compileScript is actually useful outside testing though)
-- TODO rename `runRules` or `runShake`?
-- TODO require a return type just for showing the result?
-- TODO take a variable instead?
-- TODO add a top-level retry here? seems like it would solve the read issues
eval :: Handle -> CutConfig -> Locks -> CutType -> Rules ResPath -> IO ()

-- TODO put this back once done debugging (duplicates everything annoyingly)
-- eval hdl cfg ref rtype = retryIgnore . eval'

eval hdl cfg ref rtype = ignoreErrors . eval'
  where
    -- This isn't as bad as it sounds. It just prints an error message instead
    -- of crashing the rest of the program but the error will be visible.
    ignoreErrors fn = catchAny fn (\e -> putStrLn $ "error! " ++ show e)

    -- This one is as bad as it sounds, so remove it when able! It's the only
    -- way I've managed to solve the occasional "openFile" lock conflicts.
    -- TODO at least log when a retry happens for debugging
    -- TODO ask Niel if individual actions can be retried instead
    -- TODO could always fork Shake to put it in if needed too
    retryIgnore fn = ignoreErrors $ recoverAll (limitRetries 4) $ report fn

    -- Reports how many failures so far and runs the main fn normally
    -- TODO debug rather than putStrLn?
    report fn status = case rsIterNumber status of
      0 -> fn
      n -> debug cfg ("error! eval failed " ++ show n ++ " times") fn

    eval' rpath = myShake cfg $ do
      (ResPath path) <- rpath
      want ["eval"]
      "eval" ~> do
        alwaysRerun
        need [path] -- TODO is this done automatically in the case of result?
        res  <- prettyResult cfg ref rtype $ toCutPath cfg path
        res' <- liftIO $ renderIO cfg res
        liftIO $ hPutStrLn hdl res'

-- TODO get the type of result and pass to eval
evalScript :: Handle -> CutState -> IO ()
evalScript hdl s@(as,c,ref) = case lookup (CutVar "result") as of
  Nothing  -> putStrLn "no result variable. that's not right!"
  Just res -> eval hdl c ref (typeOf res) (compileScript s Nothing)

evalFile :: Handle -> CutConfig -> Locks -> IO ()
evalFile hdl cfg ref = case cfgScript cfg of
  Nothing  -> putStrLn "no script. that's not right!"
  Just scr -> do
    s <- parseFileIO cfg ref scr -- TODO just take a CutState?
    evalScript hdl (s,cfg,ref)
