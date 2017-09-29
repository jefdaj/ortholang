module ShortCut.Core.Paths3
  -- 
  ( CutPath
  , toCutPath
  , fromCutPath
  -- cache dirs
  , cacheDir
  -- tmpfiles
  , exprPath
  , exprPathExplicit
  )
  where

import Path (parseAbsFile, fromAbsFile)
import ShortCut.Core.Types -- (CutConfig)
import ShortCut.Core.Util (lookupVar, digest)
import ShortCut.Core.Debug (debug, debugPath)
import Data.String.Utils          (replace)
import Data.Maybe (fromJust)
import Development.Shake.FilePath ((</>), (<.>))
import Data.List                  (intersperse)

--------------
-- cutpaths --
--------------

-- TODO move to Types.hs once settled
newtype CutPath = CutPath FilePath deriving (Eq, Show)

-- Replace current absolute paths with generic placeholders that won't change
-- when the tmpDir is moved later or whatever.
-- TODO rewrite with a more elegant [(fn, string)] if there's time
toGeneric :: CutConfig -> String -> String
toGeneric cfg txt = replace (cfgTmpDir  cfg) "$TMPDIR"
                  $ replace (cfgWorkDir cfg) "$WORKDIR"
                  $ txt

-- Replace generic path placeholders with current paths
-- TODO rewrite with a more elegant [(fn, string)] if there's time
fromGeneric :: CutConfig -> String -> String
fromGeneric cfg txt = replace "$TMPDIR"  (cfgTmpDir  cfg)
                    $ replace "$WORKDIR" (cfgWorkDir cfg)
                    $ txt

toCutPath :: CutConfig -> FilePath -> CutPath
toCutPath cfg = CutPath . toGeneric cfg . normalize
  where
    normalize = fromAbsFile . fromJust . parseAbsFile

fromCutPath :: CutConfig -> CutPath -> FilePath
fromCutPath cfg (CutPath path) = fromGeneric cfg path

----------------
-- cache dirs --
----------------

cacheDir :: CutConfig -> String -> CutPath
cacheDir cfg modName = toCutPath cfg path
  where
    path = cfgTmpDir cfg </> "cache" </> modName

-- TODO cacheDirUniq or Explicit?

--------------
-- tmpfiles --
--------------

-- This is just a convenience used in exprPath
-- TODO rename hSomething?
argHashes :: CutState -> CutExpr -> [String]
argHashes s@(scr,_) (CutRef _ _ _ v) = argHashes s $ lookupVar v scr
argHashes _ (CutLit  _ _     v    ) = [digest v]
argHashes s (CutFun  _ _ _ _ es   ) = map (digest . exprPath s) es
argHashes s (CutBop  _ _ _ _ e1 e2) = map (digest . exprPath s) [e1, e2]
argHashes s (CutList _ _ _   es   ) = [digest $ map (digest . exprPath s) es]

-- TODO rename to tmpPath?
exprPath :: CutState -> CutExpr -> CutPath
exprPath s@(scr, _) (CutRef _ _ _ v) = exprPath s $ lookupVar v scr
exprPath s@(_, cfg) expr = debugPath cfg "exprPath" expr res
  where
    prefix = prefixOf expr
    rtype  = typeOf expr
    salt   = saltOf expr
    hashes = argHashes s expr
    res    = exprPathExplicit s prefix rtype salt hashes

exprPathExplicit :: CutState -> String -> CutType -> Int -> [String] -> CutPath
exprPathExplicit (_, cfg) prefix rtype salt hashes = toCutPath cfg path
  where
    dir  = cfgTmpDir cfg </> "exprs" </> prefix
    base = (concat $ intersperse "_" hashes) ++ suf
    suf  = if salt == 0 then "" else "_" ++ show salt
    path = dir </> base <.> extOf rtype
