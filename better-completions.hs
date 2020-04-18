module Main where

{-
I'm finally getting around to defeating my old demon, the Haskeline InputT
monad! Disentangling stuff from the rest of the Repl is too hard though, so
I'll start by mocking up what I want with some of the same types. It should
allow access to GlobalEnv via get (StateT), or maybe ask (ReaderT with IORef).
Probably ask though since the Repl uses StateT already.

I'll start from the working example in the Haddocks, then try to make it get
completions changed during the program run.
-}

import System.Console.Haskeline
import Control.Monad.State.Strict -- note: no MonadException instance for Lazy!

-- list of words to complete, which we should be able to add to from inside the repl
type MyState = [String]

type MyMonad = StateT MyState IO

loop :: InputT MyMonad ()
loop = do
   minput <- getInputLine "% "
   case minput of
       Nothing -> return ()
       Just "quit" -> return ()
       Just input -> do
         outputStrLn $ "words: " ++ show (words input)
         loop

main :: IO ()
main = evalStateT (runInputT defaultSettings loop) []
