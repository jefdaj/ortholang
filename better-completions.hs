module Main where

{-
I'm finally getting around to defeating my old demon, the Haskeline InputT
monad! Disentangling stuff from the rest of the Repl is too hard though, so
I'll start by mocking up what I want with some of the same types. It should
allow access to GlobalEnv via get (StateT), or maybe ask (ReaderT with IORef).
Probably ask though since the Repl uses StateT already.
-}

import System.Console.Haskeline
import Control.Monad.State.Strict -- note: no MonadException instance for Lazy!
import Data.List (nub, isPrefixOf)

-- list of words to complete, which we can add to from inside the repl
type MyState = [String]

type MyMonad = StateT MyState IO

myComplete :: CompletionFunc MyMonad
myComplete = completeWord Nothing " " listWords

listWords :: String -> MyMonad [Completion]
listWords s = do
  ws <- get
  let ws' = filter (s `isPrefixOf`) ws
      cs  = map simpleCompletion ws'
  return cs

mySettings :: Settings MyMonad
mySettings = setComplete myComplete defaultSettings

loop :: InputT MyMonad ()
loop = do
   minput <- getInputLine "% "
   case minput of
       Nothing -> return ()
       Just "quit" -> return ()
       Just input -> do
         oldWords <- lift $ get
         let newWords = nub $ oldWords ++ words input
         outputStrLn $ "state: " ++ show newWords
         lift $ put newWords
         loop

main :: IO ()
main = evalStateT (runInputT mySettings loop) []
