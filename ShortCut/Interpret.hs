{- ShortCut code is interpreted in three phases: parse, check, and eval. But
 - client code shouldn't need to care about that, so this module wraps them in
 - a simplified interface. It just holds whatever [i]nterpret functions the
 - Repl and Main modules use for now rather than any comprehensive API.
 -}

module ShortCut.Interpret
  ( Typed(..)
  , TypedVar(..)
  , ShortCutError(..)
  , TypedExpr(..)
  , iAssign
  , iExpr
  , iFile -- TODO have the CLI call this
  , eExpr
  , isAssignment
  )
  where

import Control.Exception.Enclosed (catchAny)
import Data.Either                (isRight)
import Data.List.Utils            (delFromAL)

import Development.Shake
import ShortCut.Interpret.Compile
import ShortCut.Interpret.Parse
import ShortCut.Monads
import ShortCut.Types

isAssignment :: String -> Bool
isAssignment line = isRight $ regularParse pVarEq line

interpret :: Parser t -> (t -> CheckM b)
          -> TypedScript -> String -> Either ShortCutError b
interpret parser checker script str = do
  parsed <- parseWithEof parser str
  let (checked, _, _) = runCheckM (checker parsed) [] script
  checked

iExpr :: TypedScript -> String -> Either ShortCutError TypedExpr
iExpr = interpret pExpr tExpr

iAssign :: TypedScript -> String -> Either ShortCutError TypedAssign
iAssign = interpret pAssign tAssign

-- TODO remove? nah, will be used for overall CLI parsing a file
iScript :: TypedScript -> String -> Either ShortCutError TypedScript
iScript = interpret pScript tScript

-- TODO could generalize to other parsers/checkers like above for testing
-- TODO is it OK that all the others take an initial script but not this?
iFile :: FilePath -> IO (Either ShortCutError TypedScript)
iFile path = readFile path >>= (\s -> return $ iScript [] s)

-- TODO use hashes + dates to decide which files to regenerate?
-- alternatives tells Shake to drop duplicate rules instead of throwing an error
myShake :: Rules () -> IO ()
myShake = shake myOpts . alternatives
  where
    myOpts = shakeOptions
      { shakeFiles     = "_shortcut"
      , shakeReport    = ["_shortcut/report.html"]
      , shakeVerbosity = Loud -- TODO configure with a command line flag?
      , shakeThreads   = 0    -- set to number of processors
      -- , shakeChange = ChangeModtimeAndDigest
      -- , shakeCommandOptions = [EchoStdout True]
      -- , shakeProgress = progressSimple
      -- , shakeLineBuffering = False
      }

-- run the result of any of the c* functions, and print it
-- (only cScript is actually useful outside testing though)
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
        putQuiet $ "\n" ++ str

-- runs an expression and names it "result"
-- TODO how to handle if the var isn't in the script??
-- TODO hook the logs + configs together?
-- TODO only evaluate up to the point where the expression they want, or all?
-- TODO show the return type too (make a show instance for TypedExpr?)
eExpr :: String -> ReplM ()
eExpr line = do
  scr <- getScript
  case iExpr scr line of
    Left  err -> message $ show err
    Right expr -> do
      let res  = TypedVar "result"
          scr' = delFromAL scr res ++ [(res,expr)]
      liftIO $ eval $ cScript res scr'
