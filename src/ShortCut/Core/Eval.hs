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
import Data.Maybe                     (maybeToList)
import ShortCut.Core.Compile.Basic    (compileScript)
import ShortCut.Core.Parse            (parseFileIO)
import ShortCut.Core.Pretty           (prettyNum)
import ShortCut.Core.Paths            (CutPath, toCutPath, fromCutPath)
import ShortCut.Core.Actions          (readLits, readPaths)
import Text.PrettyPrint.HughesPJClass (render)
import System.IO                      (Handle, hPutStrLn)
import Text.PrettyPrint.HughesPJClass

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
      , shakeThreads   = 0
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
prettyResult :: CutConfig -> CutType -> CutPath -> Action Doc
prettyResult _ Empty  _ = return $ text "[]"
prettyResult cfg (ListOf t) f
  | t `elem` [str, num] = do
    lits <- readLits cfg $ fromCutPath cfg f
    let lits' = if t == str
                  then map (\s -> text $ "\"" ++ s ++ "\"") lits
                  else map prettyNum lits
    return $ text "[" <> sep ((punctuate (text ",") lits')) <> text "]"
  | otherwise = do
    paths <- readPaths cfg $ fromCutPath cfg f
    pretties <- mapM (prettyResult cfg t) paths
    return $ text "[" <> sep ((punctuate (text ",") pretties)) <> text "]"
prettyResult cfg t f = liftIO $ fmap showFn $ (tShow t) (fromCutPath cfg f)
  where
    showFn = if t == num then prettyNum else text

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
        res <- prettyResult cfg rtype $ toCutPath cfg path
        liftIO $ hPutStrLn hdl $ render res

-- TODO get the type of result and pass to eval
evalScript :: Handle -> CutState -> IO ()
evalScript hdl s@(as,c) = case lookup (CutVar "result") as of
  Nothing  -> putStrLn "no result variable. that's not right!"
  Just res -> eval hdl c (typeOf res) (compileScript s Nothing)

evalFile :: Handle -> CutConfig -> IO ()
evalFile hdl cfg = case cfgScript cfg of
  Nothing  -> putStrLn "no script. that's not right!"
  Just scr -> do
    s <- parseFileIO cfg scr
    evalScript hdl (s,cfg)
