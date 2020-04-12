{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
An attempt to structure the various runtime errors OrthoLang is already subject
to. Based on the standard 'Control.Exception' Haddocks, as well as
Shake's 'Development.Shake.Internal.Errors'. 

TODO remove the hierarchy any just have a bunch of individual errors?
     decide after putting in the handlers whether you need to pass some through anywhere

TODO declare errors in the same files they're used in so you have access to the same types?

They should be 'Control.Exception.throw'n using the most specific class
possible. Then you can 'Control.Exception.catch' them using any superclass up
through 'Control.Exception.SomeException':

@
λ: :m OrthoLang.Errors
λ: import Control.Exception

λ: throw (Mistake ["this is a hint", "another hint"]) \`catch\` \\e -> putStrLn $ show (e :: SomeOLException)
Programming mistake! Please report it to Jeff Johnson:
  this is a hint
  another hint

λ: throw (NoSuchVar "bob") \`catch\` \\e -> putStrLn $ show (e :: SomeParserException)
no such var: 'bob'

λ: throw (ScriptFailed "test.sh" "test.log" 3) \`catch\` \\e -> putStrLn (show (e :: SomeEvalException))
test.sh failed with exit code 3. See test.log for details.

λ: throw (MissingDigests "$TMPDIR/exprs/some/path/to/result" ["328904723", "023470uh"])
  \`catch\` \\e -> print (e :: SomeOLException)
Missing digests needed for '$TMPDIR/exprs/some/path/to/result':
  328904723
  023470uh
@

If none of the superclasses match, you get an actual error:

@
λ: throw NoSuchVar \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: SomeReplException))
*** Exception: NoSuchVar

λ: throw NoSuchVar \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: IOException))
*** Exception: NoSuchVar
@
-}

module OrthoLang.Errors
  (

  -- * Generic exceptions
    Mistake(..)

  -- * Parser exceptions
  , NoSuchVar(..)
  -- TODO type errors (a hierarchy?)

  -- * Compiler exceptions
  , MissingDigests(..) -- TODO should this count as an eval exception?
  , PathLitMixup(..)
  -- TODO type errors (a hierarchy?)

  -- * Repl exceptions
  -- TODO no such function/module/type/encoding

  -- * Eval exceptions
  , ScriptFailed(..)
  , simplifyShakeErrors
  -- , ShakeError(..)
  -- TODO lock errors
  -- TODO no such file/dir
  -- TODO permissions error

  -- * Implementation details
  , SomeOLException(..)
  , SomeParserException(..)
  , SomeCompilerException(..)
  , SomeReplException(..)
  , SomeEvalException(..)

  )
  where

import OrthoLang.Debug (warn)
import qualified Development.Shake as S

import Control.Exception.Safe (Exception, SomeException, fromException, toException, catch)
import Data.Typeable     (Typeable, cast)

------------------------
-- generic exceptions --
------------------------

data SomeOLException = forall e . Exception e => SomeOLException e
  deriving Typeable

instance Exception SomeOLException

instance Show SomeOLException where
  show (SomeOLException e) = show e

olToException :: Exception e => e -> SomeException
olToException = toException . SomeOLException

olFromException :: Exception e => SomeException -> Maybe e
olFromException x = do
  SomeOLException a <- fromException x
  cast a

data Mistake = Mistake [String]

instance Show Mistake where
  show (Mistake hints) = unlines $ "Programming mistake! Please report it to Jeff Johnson:" : hints'
    where
      hints' = map ("  " ++) hints

instance Exception Mistake where
  toException   = olToException
  fromException = olFromException

-----------------------
-- parser exceptions --
-----------------------

data SomeParserException = forall e . Exception e => SomeParserException e

instance Show SomeParserException where
  show (SomeParserException e) = show e

instance Exception SomeParserException where
  toException   = olToException
  fromException = olFromException

parserToException :: Exception e => e -> SomeException
parserToException = toException . SomeParserException

parserFromException :: Exception e => SomeException -> Maybe e
parserFromException x = do
  SomeParserException a <- fromException x
  cast a

data NoSuchVar = NoSuchVar String

instance Show NoSuchVar where
  show (NoSuchVar var) = "No such var: '" ++ var ++ "'"

instance Exception NoSuchVar where
  toException   = parserToException
  fromException = parserFromException

-------------------------
-- compiler exceptions --
-------------------------

data SomeCompilerException = forall e . Exception e => SomeCompilerException e

instance Show SomeCompilerException where
  show (SomeCompilerException e) = show e

instance Exception SomeCompilerException where
  toException   = olToException
  fromException = olFromException

compilerToException :: Exception e => e -> SomeException
compilerToException = toException . SomeCompilerException

compilerFromException :: Exception e => SomeException -> Maybe e
compilerFromException x = do
  SomeCompilerException a <- fromException x
  cast a

-- TODO Path/PathDigest instead of FilePath/String? if it works without an import cycle
data MissingDigests = MissingDigests FilePath [String]

instance Show MissingDigests where
  show (MissingDigests path digests) =
    "Missing digests needed for '" ++ path ++ "':\n" ++ unlines (map ("  " ++) digests)

instance Exception MissingDigests where
  toException   = compilerToException
  fromException = compilerFromException

data PathLitMixup = PathLitMixup String

instance Exception PathLitMixup where
  toException   = compilerToException
  fromException = compilerFromException

instance Show PathLitMixup where
  show (PathLitMixup s) = s

---------------------
-- repl exceptions --
---------------------

data SomeReplException = forall e . Exception e => SomeReplException e

instance Show SomeReplException where
  show (SomeReplException e) = show e

instance Exception SomeReplException where
  toException   = olToException
  fromException = olFromException

replToException :: Exception e => e -> SomeException
replToException = toException . SomeReplException

replFromException :: Exception e => SomeException -> Maybe e
replFromException x = do
  SomeReplException a <- fromException x
  cast a

---------------------
-- eval exceptions --
---------------------

-- TODO are there any that aren't also shake exeptions? if not, remove this

data SomeEvalException = forall e . Exception e => SomeEvalException e

instance Show SomeEvalException where
  show (SomeEvalException e) = show e

instance Exception SomeEvalException where
  toException   = olToException
  fromException = olFromException

evalToException :: Exception e => e -> SomeException
evalToException = toException . SomeEvalException

evalFromException :: Exception e => SomeException -> Maybe e
evalFromException x = do
  SomeEvalException a <- fromException x
  cast a

data ScriptFailed = ScriptFailed String FilePath Int

instance Show ScriptFailed where
  show (ScriptFailed name logfile code) =
    name ++ " failed with exit code " ++ show code ++ ". See " ++ logfile ++ " for details."

instance Exception ScriptFailed where
  toException   = evalToException
  fromException = evalFromException

-- TODO how to wrap shake exceptions?
-- TODO ScriptFailed needs to be a subset of shake exceptions
--      probably need to provide a function that catches them and re-throws one of my exceptions?
-- TODO write the long shake error to a logfile and print single-line description of it
--      does that require a separate catch-and-re-throw function to do the writing?
-- data SomeEvalException = SomeEvalException S.ShakeException

simplifyShakeErrors :: IO () -> IO ()
simplifyShakeErrors io = io `catch` \(e :: S.ShakeException) -> do
  warn "errors.simplifyShakeErrors" $ show e
  print $ "error while building '" ++ S.shakeExceptionTarget e ++ "':"
  print $ S.shakeExceptionInner e
  -- TODO also print a smaller error message

-- instance Show ShakeError where
--   show (ShakeError e) = "Shake failed: " ++ firstLine ++ "... See ortholang.log for details."
--     where
--       firstLine = takeWhile (/= '\n') $ show e

-- instance Exception ShakeError where
--   toException   = evalToException
--   fromException = evalFromException
