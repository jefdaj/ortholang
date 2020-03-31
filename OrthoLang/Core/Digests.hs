module OrthoLang.Core.Digests
  (

  -- * Generate path digests
    exprPathDigest
  , exprDigest
  , exprDigests
  , scriptDigests
  , decodeNewRulesDeps

  -- * Internal utilities
  , listExprs
  , listScriptExprs
  , listDigestsInPath

  )
  where

import OrthoLang.Core.Types

import Prelude hiding (log)
import qualified Data.Map.Strict as M
import qualified OrthoLang.Util as U

import Control.Monad              (when)
import Data.Maybe                 (catMaybes)
import Development.Shake.FilePath (makeRelative, splitPath)
import OrthoLang.Core.Paths       (toPath, exprPath, bop2fun)
import OrthoLang.Util        (digest, trace, traceShow)
import Data.IORef (readIORef)


log :: String -> String -> IO ()
log fnName msg = U.debug fnName msg

-- TODO err function

exprPathDigest :: Path -> PathDigest
exprPathDigest = PathDigest . digest

exprDigest :: Config -> Script -> Expr -> DigestMap
exprDigest cfg scr expr = traceShow "core.paths.exprDigest" res
  where
    p = exprPath cfg scr expr
    dKey = PathDigest $ digest p
    res = M.singleton dKey (typeOf expr, p)

exprDigests :: Config -> Script -> [Expr] -> DigestMap
exprDigests cfg scr exprs = M.unions $ map (exprDigest cfg scr) $ concatMap listExprs exprs

scriptDigests :: Config -> Script -> DigestMap
scriptDigests cfg scr = exprDigests cfg scr $ listScriptExprs scr

{-|
"Flatten" (or "unfold"?) an expression into a list of it + subexpressions.

TODO is there a better word for this, or a matching typeclass?
-}
listExprs :: Expr -> [Expr]
listExprs e@(Lit _ _ _) = [e]
listExprs e@(Ref _ _ _ _) = [e]
listExprs e@(Bop _ _ _ _ e1 e2) = e : concatMap listExprs [bop2fun e, e1, e2] -- TODO remove e?
listExprs e@(Fun _ _ _ _ es   ) = e : concatMap listExprs es
listExprs e@(Lst _ _ _   es   ) = e : concatMap listExprs es
listExprs e@(Com _) = [e] -- TODO is this right?

listScriptExprs :: Script -> [Expr]
listScriptExprs scr = concatMap listExprs $ map snd scr

-- insertNewRulesDigest :: GlobalEnv -> Expr -> IO ()
-- insertNewRulesDigest st@(_, cfg, _, idr) expr
--   = traceD "insertNewRulesDigest" st expr
--   $ atomicModifyIORef' idr
--   $ \h@(IDs {hExprs = ids}) -> (h {hExprs = M.insert eDigest (eType, ePath) ids}, ())
--   where
--     eType   = typeOf expr
--     ePath   = exprPath cfg scr expr
--     eDigest = exprPathDigest ePath

-- TODO what monad should this be in?
-- TODO encode lookup failure as Maybe? it indicates a programmer error though, not user error
-- TODO take an ExprPath
-- TODO remove any unneccesary path components before lookup, and count the necessary ones
-- TODO is drop 2 a safe enough way to remove 'result' and repeat salt from the ends of the paths?
-- TODO better split function
decodeNewRulesDeps :: Config -> DigestsRef -> ExprPath
                   -> IO (Type, [Type], [Path])
decodeNewRulesDeps cfg dRef (ExprPath out) = do
  log "decodeNewRulesDeps" $ "out: " ++ show out
  dMap <- readIORef dRef
  let dKeys  = listDigestsInPath cfg out
      dVals  = catMaybes $ map (\k -> M.lookup k dMap) dKeys
      dVals' = trace "ortholang.core.types.decodeNewRulesDeps" ("\"" ++ out ++ "' -> " ++ show dVals) dVals
      dTypes = map fst dVals'
      dPaths = map snd dVals'
      oKey   = exprPathDigest $ toPath cfg out
      Just (oType, _) = M.lookup oKey dMap
  log "decodeNewRulesDeps" $ "dKeys: " ++ show dKeys
  log "decodeNewRulesDeps" $ "dTypes: " ++ show dTypes
  log "decodeNewRulesDeps" $ "dVals': " ++ show dVals'
  log "decodeNewRulesDeps" $ "dPaths: " ++ show dPaths
  log "decodeNewRulesDeps" $ "oKey: " ++ show oKey
  when (length dVals /= length dKeys) $ do
    -- TODO err function here
    log "decodeNewRulesDeps" $ "failed to decode path: " ++ out
    log "decodeNewRulesDeps" $ unlines $ "dMap when lookup failed:\n" : (map show $ M.toList dMap)
    error $ "failed to decode path: \"" ++ out ++ "\""
  return (oType, dTypes, dPaths)

-- TODO hey, is it worth just looking up every path component to make it more robust?
listDigestsInPath :: Config -> FilePath -> [PathDigest]
listDigestsInPath cfg
  = map PathDigest
  . reverse
  . drop 2
  . reverse
  . drop 2
  . dropWhile (/= "exprs")
  . map (filter (/= '/'))
  . splitPath
  . makeRelative (cfgTmpDir cfg)
