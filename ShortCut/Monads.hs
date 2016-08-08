{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, FlexibleContexts #-}

module ShortCut.Monads where

-- TODO test the putAssign variants!

import ShortCut.Types

import Control.Exception         (throwIO, catch, )
import Control.Monad.Except      (throwError, MonadError, ExceptT, runExceptT)
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Control.Monad.Identity    (Identity, mzero)
import Control.Monad.RWS.Lazy    (RWST, runRWS, runRWST, get, put, ask)
import Control.Monad.Reader      (MonadReader)
import Control.Monad.State       (MonadState)
import Control.Monad.Trans       (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Writer      (MonadWriter)
import Data.List                 (isInfixOf)
import Data.List.Utils           (delFromAL)
import ShortCut.Compile          (namedTmp)
import System.Console.Haskeline  (InputT, runInputT, defaultSettings, getInputLine)
import System.Directory          (removeFile)
import System.IO.Error           (isDoesNotExistError)

-----------------
-- check monad --
-----------------

-- TODO look into the RWS-specific stuff in Control.Monad.Trans.Except:
--      (Monoid w, MonadError e m) => MonadError e (RWST r w s m) etc.

type CheckConfig = [(String, String)]
type CheckLog    = [String]
type CheckState  = TypedScript

newtype CheckT m a = CheckT
  { unCheckT :: ExceptT ShortCutError (RWST CheckConfig CheckLog CheckState m) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError  ShortCutError
    , MonadReader CheckConfig
    , MonadWriter CheckLog
    , MonadState  CheckState
    )

type CheckM a = CheckT Identity a

class (MonadReader CheckConfig m, MonadState CheckState m
      ,MonadError ShortCutError m) => MonadCheck m where
  askConfig :: m CheckConfig
  getScript :: m CheckState
  putScript :: CheckState -> m ()
  getExpr   :: TypedVar -> m (Maybe TypedExpr)
  putAssign :: TypedAssign -> m ()
  throw     :: ShortCutError -> m a

-- TODO is this instance right? if not, that could be causing my issues!
--      looks like it has the right signature though, and two lifts makes sense
instance MonadTrans CheckT where
  lift = CheckT . lift . lift

instance (Monad m) => MonadCheck (CheckT m) where
  askConfig = ask
  getScript = get
  putScript = put
  getExpr v = getScript >>= \s -> return $ lookup v s
  putAssign a = putAssign' False a >> return ()
  throw     = throwError

containsKey :: (Eq a) => [(a,b)] -> a -> Bool
containsKey lst key = isInfixOf [key] $ map fst lst

-- the Bool specifies whether to continue if the variable exists already
-- note that it will always continue if only the *file* exists,
-- because that might just be left over from an earlier program run
putAssign' :: MonadCheck m => Bool -> TypedAssign -> m FilePath
putAssign' force (v@(TypedVar var), e@(TypedExpr r _)) = do
  s <- getScript
  let path = namedTmp r v
  if s `containsKey` v && not force
    then error $ "Variable '" ++ var ++ "' used twice"
    else do
      putScript $ delFromAL s v ++ [(v,e)]
      return path

runCheckM :: CheckM a -> CheckConfig -> CheckState
          -> (Either ShortCutError a, CheckState, CheckLog)
runCheckM = runRWS . runExceptT . unCheckT

runCheckT :: CheckT m a -> CheckConfig -> CheckState
          -> m (Either ShortCutError a, CheckState, CheckLog)
runCheckT = runRWST . runExceptT . unCheckT

----------------
-- Repl monad --
----------------

newtype ReplM a = ReplM { unReplM :: MaybeT (CheckT (InputT IO)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader CheckConfig
    , MonadWriter CheckLog
    , MonadState  CheckState
    , MonadError  ShortCutError
    )

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- TODO how can I prevent duplicating this code?
-- warning: be careful when trying; last time I ended up with a bug that caused
--          infinite loops because they called themselves
-- TODO try without the class at all because it's just not worth it right now!
-- TODO or, try putting maybet inside checkt so you don't have to lift it
-- TODO or, try making use of lift from CheckT's MonadTrans instance
-- TODO or, try using liftMaybe again
-- TODO could using both mtl and transformers be an issue?
instance MonadCheck ReplM where
  askConfig = ask
  getScript = get
  putScript = put
  putAssign a = putAssign' True a >>= \f -> liftIO $ removeIfExists f
  getExpr v = getScript >>= \s -> return $ lookup v s
  throw     = throwError

runReplM :: ReplM a -> CheckConfig -> CheckState
         -> IO (Either ShortCutError (Maybe a), CheckState, CheckLog)
runReplM r c s = runInputT defaultSettings $ runCheckT (runMaybeT $ unReplM r) c s

prompt :: String -> ReplM (Maybe String)
prompt = ReplM . lift . lift . getInputLine

message :: String -> ReplM ()
message str = liftIO $ putStrLn str

quit :: ReplM a
quit = ReplM mzero
