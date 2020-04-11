{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

{-|
An attempt to structure the various runtime errors OrthoLang is already subject
to. Based on the standard 'Control.Exception' Haddocks, as well as
Shake's 'Development.Shake.Internal.Errors'. 

They should be 'Control.Exception.throw'n using the most specific class
possible. Then you can 'Control.Exception.catch' them using any superclass up to
'Control.Exception.SomeException':

@
*Main> throw NoSuchVar \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: NoSuchVar))
Caught NoSuchVar
*Main> throw NoSuchVar \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: SomeParseException))
Caught NoSuchVar
*Main> throw NoSuchVar \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: SomeOLException))
Caught NoSuchVar
*Main> throw NoSuchVar \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: SomeException))
Caught NoSuchVar
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

  -- * Parse exeptions
  , SomeParseException(..)
  , parseToException
  , parseFromException
  , NoSuchVar(..)

  -- * Repl exeptions
  , SomeReplException(..)
  , replToException
  , replFromException

  )
  where

import Control.Exception
import Data.Typeable

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

----------------------
-- parse exceptions --
----------------------

data SomeParseException = forall e . Exception e => SomeParseException e

instance Show SomeParseException where
    show (SomeParseException e) = show e

instance Exception SomeParseException where
    toException   = olToException
    fromException = olFromException

parseToException :: Exception e => e -> SomeException
parseToException = toException . SomeParseException

parseFromException :: Exception e => SomeException -> Maybe e
parseFromException x = do
    SomeParseException a <- fromException x
    cast a

data NoSuchVar = NoSuchVar
    deriving Show

instance Exception NoSuchVar where
    toException   = parseToException
    fromException = parseFromException

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
