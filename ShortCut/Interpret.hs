{-# LANGUAGE FlexibleContexts #-}

{- ShortCut code is interpreted in three phases: parse, check, and eval. But
 - client code shouldn't need to care about that, so this module wraps them in
 - a simplified interface. It just holds whatever [i]nterpret functions the
 - Repl and Main modules use for now rather than any comprehensive API.
 -}

module ShortCut.Interpret
  ( CutError(..)
  , TypedExpr(..)
  , iAssign
  , iExpr
  , iFile -- TODO have the CLI call this
  , eval
  , cScript
  , isAssignment
  , putAssign
  )
  where

import Development.Shake
import ShortCut.Interpret.Compile
import ShortCut.Interpret.Parse
import ShortCut.Types
import Control.Exception          (throwIO, catch, )
import Control.Exception.Enclosed (catchAny)
import Control.Monad.IO.Class     (MonadIO)
import Control.Monad.RWS.Lazy     (get, put)
import Control.Monad.State        (MonadState)
import Data.Either                (isRight)
import Data.List                  (isInfixOf)
import Data.List.Utils            (delFromAL)
import System.Directory           (removeFile)
import System.IO.Error            (isDoesNotExistError)
import Control.Monad.Reader       (ask)

isAssignment :: String -> Bool
isAssignment line = isRight $ regularParse pVarEq line

interpret :: Parser t -> (t -> CutM b) -> CutConfig
          -> TypedScript -> String -> Either CutError b
interpret parser checker cfg script str = do
  parsed <- parseWithEof parser str
  let (checked, _, _) = runCutM (checker parsed) cfg script
  checked

iExpr :: CutConfig -> TypedScript -> String -> Either CutError TypedExpr
iExpr = interpret pExpr tExpr

iAssign :: CutConfig -> TypedScript -> String -> Either CutError TypedAssign
iAssign = interpret pAssign tAssign

-- TODO remove? nah, will be used for overall CLI parsing a file
iScript :: CutConfig -> TypedScript -> String -> Either CutError TypedScript
iScript = interpret pScript tScript

-- TODO could generalize to other parsers/checkers like above for testing
-- TODO is it OK that all the others take an initial script but not this?
iFile :: CutConfig -> FilePath -> IO (Either CutError TypedScript)
iFile cfg path = readFile path >>= (\s -> return $ iScript cfg [] s)

-- TODO use hashes + dates to decide which files to regenerate?
-- alternatives tells Shake to drop duplicate rules instead of throwing an error
myShake :: Rules () -> IO ()
myShake = shake myOpts . alternatives
  where
    myOpts = shakeOptions
      { shakeFiles     = "_shortcut"
      , shakeVerbosity = Quiet -- TODO configure with a command line flag?
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
eval :: Rules FilePath -> IO ()
eval = ignoreErrors . eval'
  where
    ignoreErrors fn = catchAny fn (\e -> putStrLn $ "error! " ++ show e)
    eval' rpath = myShake $ do
      path <- rpath
      want ["eval"]
      "eval" ~> do
        alwaysRerun
        -- TODO show the var rather than the actual file contents
        str <- readFile' path
        -- putQuiet $ "\n" ++ str
        liftIO $ putStr str

containsKey :: (Eq a) => [(a,b)] -> a -> Bool
containsKey lst key = isInfixOf [key] $ map fst lst

-- the Bool specifies whether to continue if the variable exists already
-- note that it will always continue if only the *file* exists,
-- because that might just be left over from an earlier program run
putAssign' :: MonadState CutState m => Bool -> TypedAssign -> m FilePath
putAssign' force (v@(VarName var), expr) = do
  s <- get
  let path = namedTmp v expr
  if s `containsKey` v && not force
    then error $ "Variable '" ++ var ++ "' used twice"
    else do
      put $ delFromAL s v ++ [(v,expr)]
      return path

-- TODO remove? refactor?
putAssign :: (MonadIO m, MonadState CutState m) =>  TypedAssign -> m ()
putAssign a = putAssign' True a >>= \f -> liftIO $ removeIfExists f

-- TODO should this go in Interpret.hs? Types.hs?
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
