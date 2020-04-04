module OrthoLang.Debug
  (

  -- * debugging
    trace
  , warn
  , error
  , traceShow
  , debug
  , time
  , log
  , traceP

  )
  where

import Prelude hiding (error, log)

import qualified Debug.Trace     as DT -- TODO remove
import qualified Control.Logging as L
import qualified Data.Text       as T

import Text.PrettyPrint.HughesPJClass

error :: String -> String -> a
error suffix msg = L.errorSL' (T.pack $ "ortholang." ++ suffix) (T.pack msg)

trace :: String -> String -> a -> a
trace suffix msg = L.traceSL (T.pack $ "ortholang." ++ suffix) (T.pack msg)

-- TODO deduplicate with the one in Locks.hs
warn :: String -> String -> IO ()
warn suffix msg = L.warnS' (T.pack $ "ortholang." ++ suffix) (T.pack msg)

traceShow :: Show a => String -> a -> a
traceShow suffix = L.traceShowSL (T.pack $ "ortholang." ++ suffix)

debug :: String -> String -> IO ()
debug suffix msg = L.debugS (T.pack $ "ortholang." ++ suffix) (T.pack msg)

-- TODO rearrange imports so you can make a debugA :: ActionR () too

time :: String -> String -> IO a -> IO a
time suffix msg act = L.timedDebugEndS (T.pack $ "ortholang." ++ suffix)
                                       (T.pack msg) act

log :: String -> String -> IO ()
log fnName msg = debug fnName msg

-- TODO take Text instead?
traceP :: (Pretty a, Show b) => String -> a -> b -> b
traceP name expr path = L.traceSL' (T.pack $ "core.paths." ++ name) (T.pack msg) path
  where
    ren = render $ pPrint expr
    msg = "\"" ++ ren ++ "' -> " ++ show path -- TODO include types?
