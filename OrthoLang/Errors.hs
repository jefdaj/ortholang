{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

{-|
An attempt to structure the various runtime errors OrthoLang is already subject
to. Based on the standard 'Control.Exception' Haddocks, as well as
Shake's 'Development.Shake.Internal.Errors'. 

They should be 'Control.Exception.throw'n using the most specific class
possible. Then you can 'Control.Exception.catch' them using any superclass up to
'Control.Exception.SomeException'. Tou get \"Caught ...\" if the class matches,
or \"*** Exception: ...\" if not:

@
*Main> throw NoSuchVar \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: NoSuchVar))
Caught NoSuchVar
*Main> throw NoSuchVar \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: SomeParserException))
Caught NoSuchVar
*Main> throw NoSuchVar \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: SomeOLException))
Caught NoSuchVar
*Main> throw NoSuchVar \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: SomeException))
Caught NoSuchVar
@

@
*Main> throw NoSuchVar \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: SomeReplException))
*** Exception: NoSuchVar
*Main> throw NoSuchVar \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: IOException))
*** Exception: NoSuchVar
@
-}

module OrthoLang.Errors
  (

  -- * Base class (TODO not exported?)
    SomeOLException(..)
  , olToException
  , olFromException
  , pleaseReport

  -- * Parser exeptions
  , SomeParserException(..)
  , parserToException
  , parserFromException
  , NoSuchVar(..)
  -- TODO type errors (a hierarchy?)

  -- * Compiler exeptions
  , SomeCompilerException(..)
  , compilerToException
  , compilerFromException
  -- TODO type errors (a hierarchy?)

  -- * Repl exeptions
  , SomeReplException(..)
  , replToException
  , replFromException
  -- TODO no such function/module/type/encoding

  -- * Eval exeptions
  , SomeEvalException(..)
  , evalToException
  , evalFromException
  -- TODO lock errors
  -- TODO no such file/dir
  -- TODO permissions error
  -- TODO script failed

  )
  where

import Control.Exception (Exception, SomeException, fromException, toException, ErrorCall(..))
import Data.Typeable     (Typeable, cast)

----------------
-- base class --
----------------

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

pleaseReport :: String -> SomeException
pleaseReport msg = toException $ ErrorCall $ unlines ["Coding error! Please report it to Jeff:", msg]

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

data NoSuchVar = NoSuchVar
    deriving Show

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
