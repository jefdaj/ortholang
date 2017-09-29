{- ShortCut makes heavy use of tmpfiles, and this module controls where they go
 - inside the main tmpdir. After a rewrite, the overall layout should be:
 -
 - TMPDIR
 - |-- cache: a tmpdir per module for index files or whatever
 - |   |-- biomartr
 - |   |-- blast
 - |   |-- crb-blast
 - |   |-- seqio
 - |   `-- ...
 - |-- exprs: the hashed result of every expression, organized by fn
 - |   |-- all
 - |   |-- any
 - |   |-- concat_fastas
 - |   |-- crb_blast
 - |   |-- crb_blast_each
 - |   |-- extract_ids
 - |   |-- extract_seqs
 - |   |-- gbk_to_faa
 - |   |-- gbk_to_fna
 - |   |-- leave_each_out
 - |   |-- repeat
 - |   |-- repeat_each
 - |   |-- translate
 - |   `-- ...
 - `-- vars: symlinks from user variable names to hashed expressions
 -    |-- green_hits.str.list
 -    |-- greens.faa.list
 -    |-- plantcut.str.list
 -    |-- result
 -    `-- ...
 -
 - Files in the cache are organized however seems best on a per-module basis
 - with help from `cacheDir`, `cacheDirUniq`, and `cacheFile`.
 -
 - Expression paths are determined by `exprPath` or `exprPathExplicit`. They
 - get the base name by `show`ing the expression and `digest`ing the resulting
 - `String`, the extension based on the `CutType`, and the folder based on
 - constructor + function name if a function. Some made up examples:
 -
 -   ~/.shortcut/exprs/cut_list/f987e9b98a.str.list
 -   ~/.shortcut/exprs/cut_lit/a09f8e8b9c.str
 -   ~/.shortcut/exprs/crb_blast/38978s9a79.crb
 -   ~/.shortcut/exprs/gbk_to_fna/289379af7a.fna
 -
 - Var links are determined by `varPath` using the user-given name and `CutType`.
 -}

module ShortCut.Core.Paths
  -- cutpaths
  ( toGeneric   -- TODO remove once CutPaths are established
  , fromGeneric -- TODO remove once CutPaths are established
  , CutPath
  , toCutPath
  , fromCutPath
  -- cache dirs
  , cacheDir
  -- tmpfiles
  , exprPath
  , exprPathExplicit
  , varPath
  -- file io
  , readPaths
  , readLitPaths
  , writePaths
  )
  where

import Development.Shake (Action)
import Path (parseAbsFile, fromAbsFile)
import ShortCut.Core.Types -- (CutConfig)
import ShortCut.Core.Util (lookupVar, digest)
import ShortCut.Core.Debug (debugPath, debugReadLines, debugWriteLines)
import Data.String.Utils          (replace)
import Development.Shake.FilePath ((</>), (<.>), isAbsolute)
import Data.List                  (intersperse)

--------------
-- cutpaths --
--------------

-- TODO move to Types.hs once settled
newtype CutPath = CutPath FilePath deriving (Eq, Ord, Show)

-- Replace current absolute paths with generic placeholders that won't change
-- when the tmpDir is moved later or whatever.
-- TODO rewrite with a more elegant [(fn, string)] if there's time
toGeneric :: CutConfig -> String -> String
toGeneric cfg txt = replace (cfgWorkDir cfg) "$WORKDIR"
                  $ replace (cfgTmpDir  cfg) "$TMPDIR"
                  $ txt

-- Replace generic path placeholders with current paths
-- TODO rewrite with a more elegant [(fn, string)] if there's time
fromGeneric :: CutConfig -> String -> String
fromGeneric cfg txt = replace "$WORKDIR" (cfgWorkDir cfg)
                    $ replace "$TMPDIR"  (cfgTmpDir  cfg)
                    $ txt

-- TODO print warning on failure?
toCutPath :: CutConfig -> FilePath -> CutPath
toCutPath cfg = CutPath . toGeneric cfg . normalize
  where
    normalize p = case parseAbsFile p of
      Nothing -> error $ "toCutPath can't parse: " ++ p
      Just p' -> fromAbsFile p'

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

-- TODO remove VarPath, ExprPath types once CutPath works everywhere
varPath :: CutConfig -> CutVar -> CutExpr -> CutPath
varPath cfg (CutVar var) expr = toCutPath cfg $ cfgTmpDir cfg </> "vars" </> base
  where
    base = if var == "result" then var else var <.> extOf (typeOf expr)

-------------
-- file io --
-------------

readPaths :: CutConfig -> FilePath -> Action [CutPath]
readPaths cfg path = (fmap . map) CutPath (debugReadLines cfg path)

-- read a file as lines, convert to absolute paths, then parse those as cutpaths
-- used by the load_* functions to convert user-friendly relative paths to absolute
readLitPaths :: CutConfig -> FilePath -> Action [CutPath]
readLitPaths cfg path = do
  ls <- debugReadLines cfg path
  return $ map (toCutPath cfg . toAbs) ls
  where
    toAbs line = if isAbsolute line
                   then line
                   else cfgWorkDir cfg </> line

-- TODO take a CutPath for the out file too
-- TODO take Path Abs File and convert them... or Path Rel File?
writePaths :: CutConfig -> FilePath -> [CutPath] -> Action ()
writePaths cfg out cpaths = debugWriteLines cfg out paths
  where
    paths = map (\(CutPath path) -> path) cpaths

-- TODO debugReadLit(s)
-- TODO debugWriteLit(s)
