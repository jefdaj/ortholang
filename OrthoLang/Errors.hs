{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

{-|
An attempt to structure the various runtime errors OrthoLang is already subject
to. Based on the standard 'Control.Exception' Haddocks, as well as
Shake's 'Development.Shake.Internal.Errors'. 

They should be 'Control.Exception.throw'n using the most specific class
possible. Then you can 'Control.Exception.catch' them using any superclass up
through 'Control.Exception.SomeException':

@
λ: throw (Mistake ["this is a hint", "another hint"]) \`catch\` \\e -> putStrLn $ show (e :: SomeOLException)
Programming mistake! Please report it to Jeff Johnson:
this is a hint
another hint

λ: throw (NoSuchVar "bob") \`catch\` \\e -> putStrLn $ show (e :: SomeParserException)
no such var: 'bob'

λ: throw (ScriptFailed "test.sh" "test.log" 3) \`catch\` \\e -> putStrLn (show (e :: SomeEvalException))
test.sh failed with exit code 3. Check test.log for details.

λ: throw (MissingDigest "aTestAction" "$TMPDIR\/exprs\/whatever\/result" "230478234")
  \`catch\` \\e -> putStrLn $ show (e :: SomeCompilerException)
Missing digest '230478234', which is needed by aTestAction to build '$TMPDIR/exprs/whatever/result'

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
  , MissingDigest(..)
  -- TODO type errors (a hierarchy?)

  -- * Repl exceptions
  -- TODO no such function/module/type/encoding

  -- * Eval exceptions
  , ScriptFailed(..)
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

import Control.Exception (Exception, SomeException, fromException, toException, ErrorCall(..))
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
  show (Mistake hints) = unlines $ "Programming mistake! Please report it to Jeff Johnson:" : hints

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
data MissingDigest = MissingDigest String FilePath String

instance Show MissingDigest where
  show (MissingDigest act path dig) =
    "Missing digest '" ++ dig ++ "', which is needed by " ++ act ++ " to build '" ++ path ++ "'"

instance Exception MissingDigest where
  toException   = compilerToException
  fromException = compilerFromException

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
    name ++ " failed with exit code " ++ show code ++
            ". Check " ++ logfile ++ " for details."

instance Exception ScriptFailed where
  toException   = evalToException
  fromException = evalFromException
