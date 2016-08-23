import Control.Monad.Identity
import System.Console.Haskeline
import Text.Parsec

-- dummy types similar to the ShortCut ones
type CutState  = [String]
type ParserT m = ParsecT String CutState m
type Parser    = ParserT Identity

-- attempts at the Repl

-- runInputT defaultSettings :: MonadException m => InputT m a -> m a

-- exampleLoop :: InputT IO ()
-- exampleMain :: IO ()
-- exampleMain = runInputT defaultSettings loop
-- outputStrLn :: MonadIO m => String -> InputT m ()

-- runParserT :: Stream s m t => ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)
--            :: parser of a -> initial state -> name -> string -> either error or a

{- So, ParsecT or InputT on the outside? Information really flows from the IO
 - part, which is on the right.  So probably it's just right -> left? ParsecT
 - (InputT IO) The other way, InputT (ParsecT IO), seems wrong.
 -}

type    Repl1 a =                   ParserT (InputT IO) a
newtype Repl2 a = Repl { unRepl2 :: ParserT (InputT IO) a } 

runRepl :: Repl1 a -> CutState -> IO (Either ParseError a)
-- runRepl r s = runParser pAssign s (runInputT defaultSettings $ unRepl r)
runRepl = undefined

{- Hold up, it's obvious: these two monads don't need to be connected at all!
 - Just run runParsecT as a pure computation inside the loop after handling
 - input-related stuff.
 -}

