{-# LANGUAGE FlexibleContexts #-}

{- ShortCut code is interpreted in three phases: parse, check, and eval. But
 - client code shouldn't need to care about that, so this module wraps them in
 - a simplified interface. It just holds whatever [i]nterpret functions the
 - Repl and ShortCut modules use for now rather than any comprehensive API.
 -}

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

import Debug.Trace

import Development.Shake

import Control.Exception.Enclosed (catchAny)
import Data.Maybe                 (fromJust)
import ShortCut.Core.Compile      (compileScript)
import ShortCut.Core.Parse        (parseFile)
import ShortCut.Core.Types
import ShortCut.Core.Pretty       (prettyResult)
import Text.PrettyPrint.HughesPJClass (render)

-- TODO use hashes + dates to decide which files to regenerate?
-- alternatives tells Shake to drop duplicate rules instead of throwing an error
myShake :: CutConfig -> Rules () -> IO ()
myShake cfg = shake myOpts . alternatives
  where
    myOpts = shakeOptions
      { shakeFiles     = cfgTmpDir cfg
      , shakeVerbosity = Chatty -- TODO get from cfg
      , shakeThreads   = 0    -- set to number of processors
      -- , shakeCommandOptions = [EchoStdout True]
      -- , shakeReport    = ["_shortcut/report.html"]
      -- , shakeChange = ChangeModtimeAndDigest
      -- , shakeProgress = progressSimple
      -- , shakeLineBuffering = False
      }

-- run the result of any of the c* functions, and print it
-- (only compileScript is actually useful outside testing though)
-- TODO rename `runRules` or `runShake`?
-- TODO require a return type just for showing the result?
-- TODO take a variable instead?
eval :: CutConfig -> CutType -> Rules FilePath -> IO ()
eval cfg rtype = ignoreErrors . eval'
  where
    ignoreErrors fn = catchAny fn (\e -> putStrLn $ "error! " ++ show e)
    eval' rpath = myShake cfg $ do
      path <- rpath
      want ["eval"]
      "eval" ~> do
        alwaysRerun
        need [trace ("path: " ++ path) path] -- TODO is this done automatically in the case of result?
        liftIO $ do
          res <- prettyResult cfg rtype path
          putStrLn $ render (trace ("res: " ++ show res) res)

-- TODO get the type of result and pass to eval
evalScript :: CutConfig -> CutScript -> IO ()
evalScript c s = eval c rtn $ compileScript c s
  where
    res = trace ("s: " ++ show s) (fromJust $ lookup (CutVar "result") s)
    rtn = typeOf res

evalFile :: CutConfig -> IO ()
evalFile cfg = do
  f <- parseFile cfg $ fromJust $ cfgScript cfg -- TODO something safer!
  case f of
    Left  e -> fail $ "oh no! " ++ show e -- TODO better errors
    Right s -> evalScript cfg s
