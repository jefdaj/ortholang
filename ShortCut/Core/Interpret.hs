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

module ShortCut.Core.Interpret
  ( CutExpr(..)
  , iStatement
  , iExpr
  , iFile -- TODO have the CLI call this
  , eval
  , cScript
  , isExpr
  , pAssign
  , eFile
  )
  where

import Text.Parsec (ParseError)
import Development.Shake
import ShortCut.Core.Compile
import ShortCut.Core.Parse
import ShortCut.Core.Types
-- import Control.Exception          (throwIO, catch, )
import Control.Exception.Enclosed (catchAny)
-- import Control.Monad.IO.Class     (MonadIO)
-- import Control.Monad.State        (MonadState)
-- import Control.Monad.State        (get, put)
import Data.Either                (isRight)
-- import Data.List                  (isInfixOf)
-- import Data.List.Utils            (delFromAL)
-- import System.Directory           (removeFile)
-- import System.IO.Error            (isDoesNotExistError)
import Data.Maybe (fromJust)
-- import Debug.Trace

isExpr :: CutScript -> String -> Bool
isExpr script line = isRight $ runParseM pExpr script line

-- TODO make this return the "result" assignment directly?
iExpr :: CutScript -> String -> Either ParseError CutExpr
iExpr = runParseM pExpr

iStatement :: CutScript -> String -> Either ParseError CutAssign
iStatement = runParseM pStatement

-- Even if the line is a one-off expression, it gets assigned to "result"
-- iLine :: CutScript -> String -> Either ParseError CutAssign
-- iLine scr line = if isAssignment scr line
--   then case iExpr scr line of
--     Left  err  -> Left err
--     Right expr -> Right (CutVar "result", expr)
--   else iStatement scr line

iScript :: String -> Either ParseError CutScript
iScript = runParseM pScript []

-- TODO could generalize to other parsers/checkers like above for testing
-- TODO is it OK that all the others take an initial script but not this?
-- TODO should we really care what the current script is when loading a new one?
iFile :: FilePath -> IO (Either ParseError CutScript)
iFile path = readFile path >>= (\s -> return $ iScript s)
-- iFile path = readFile path >>= return . iScript

-- TODO this should be called iFile right, and the other one goes away?
eFile :: CutConfig -> IO ()
eFile cfg = do
  f <- iFile $ fromJust $ cfgScript cfg -- TODO something safer!
  case f of
    Left  e -> fail $ "oh no! " ++ show e
    Right s -> eval cfg $ cScript cfg s

-- TODO use hashes + dates to decide which files to regenerate?
-- alternatives tells Shake to drop duplicate rules instead of throwing an error
myShake :: CutConfig -> Rules () -> IO ()
myShake cfg = shake myOpts . alternatives
  where
    myOpts = shakeOptions
      { shakeFiles     = cfgTmpDir cfg
      , shakeVerbosity = Quiet -- TODO get from cfg
      , shakeThreads   = 0    -- set to number of processors
      -- , shakeCommandOptions = [EchoStdout True]
      -- , shakeReport    = ["_shortcut/report.html"]
      -- , shakeChange = ChangeModtimeAndDigest
      -- , shakeProgress = progressSimple
      -- , shakeLineBuffering = False
      }

-- run the result of any of the c* functions, and print it
-- (only cScript is actually useful outside testing though)
-- TODO should this be part of `interpret`?
-- TODO rename `runRules` or `runShake`?
eval :: CutConfig -> Rules FilePath -> IO ()
eval cfg = ignoreErrors . eval'
  where
    ignoreErrors fn = catchAny fn (\e -> putStrLn $ "error! " ++ show e)
    eval' rpath = myShake cfg $ do
      path <- rpath
      want ["eval"]
      "eval" ~> do
        alwaysRerun
        -- TODO show the var rather than the actual file contents
        str' <- readFile' path
        -- putQuiet $ "\n" ++ str
        liftIO $ putStr str'

-- TODO should this go in Interpret.hs? Types.hs?
-- removeIfExists :: FilePath -> IO ()
-- removeIfExists fileName = removeFile fileName `catch` handleExists
--   where handleExists e
--           | isDoesNotExistError e = return ()
--           | otherwise = throwIO e
