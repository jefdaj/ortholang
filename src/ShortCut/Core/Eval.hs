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
import ShortCut.Core.Types

import Control.Exception.Enclosed     (catchAny)
import Data.Maybe                     (fromJust, maybeToList)
import ShortCut.Core.Compile.Basic          (compileScript)
import ShortCut.Core.Parse            (parseFileIO)
import ShortCut.Core.Pretty           (prettyResult)
import Text.PrettyPrint.HughesPJClass (render)
import System.IO                (Handle, hPutStrLn)

-- TODO use hashes + dates to decide which files to regenerate?
-- alternatives tells Shake to drop duplicate rules instead of throwing an error
myShake :: CutConfig -> Rules () -> IO ()
myShake cfg = shake myOpts . alternatives
  where
    myOpts = shakeOptions
      { shakeFiles     = cfgTmpDir cfg
      , shakeVerbosity = if cfgDebug cfg then Chatty else Quiet
      , shakeThreads   = 0
      , shakeReport    = maybeToList $ cfgReport cfg
      -- , shakeChange    = ChangeModtimeAndDigest -- TODO test this
      -- , shakeCommandOptions = [EchoStdout True]
      -- , shakeProgress = progressSimple
      -- , shakeLineBuffering = False
      }

-- run the result of any of the c* functions, and print it
-- (only compileScript is actually useful outside testing though)
-- TODO rename `runRules` or `runShake`?
-- TODO require a return type just for showing the result?
-- TODO take a variable instead?
eval :: Handle -> CutConfig -> CutType -> Rules ResPath -> IO ()
eval hdl cfg rtype = ignoreErrors . eval'
  where
    ignoreErrors fn = catchAny fn (\e -> putStrLn $ "error! " ++ show e)
    eval' rpath = myShake cfg $ do
      (ResPath path) <- rpath
      want ["eval"]
      "eval" ~> do
        alwaysRerun
        need [path] -- TODO is this done automatically in the case of result?
        liftIO $ do
          res <- prettyResult cfg rtype path
          hPutStrLn hdl $ render res

-- TODO get the type of result and pass to eval
evalScript :: Handle -> CutState -> IO ()
evalScript hdl s@(as,c) = eval hdl c rtn $ compileScript s Nothing
  where
    res = fromJust $ lookup (CutVar "result") as
    rtn = typeOf res

evalFile :: Handle -> CutConfig -> IO ()
evalFile hdl cfg = do
  s <- parseFileIO cfg $ fromJust $ cfgScript cfg -- TODO something safer!
  evalScript hdl (s,cfg)
