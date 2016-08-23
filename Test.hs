import ShortCut.Types
import ShortCut.Interpret (pAssign)
import System.Console.Haskeline
-- import Control.Monad.Trans.Maybe
import Text.Parsec hiding (runParser)

newtype Repl1 a = Repl { unRepl1 :: ParserT (InputT IO) a } 
type Repl2 a = ParserT (InputT IO) a

-- runRepl :: Repl a -> CutState -> IO (Either ParseError a)
-- runRepl r s = runParser pAssign s (runInputT defaultSettings $ unRepl r)
