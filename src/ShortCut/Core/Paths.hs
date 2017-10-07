-- TODO rename this module to TmpFiles?

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
  ( CutPath
  , toCutPath
  , fromCutPath
  , toGeneric
  , fromGeneric
  -- cache dirs
  , cacheDir
  -- tmpfiles
  , exprPath
  , exprPathExplicit
  , varPath
  -- , resolveVar
  -- , resolveVars
  -- file io
  , readPath
  , readPaths
  , readLitPaths
  , writePath
  , writePaths
  , readLit
  , readLits
  , writeLit
  , writeLits
  -- read and write tmpfiles as strings
  , readString
  , readStrings
  , writeString
  , writeStrings
  )
  where

import Development.Shake (Action, trackWrite)
import Path (parseAbsFile, fromAbsFile)
import ShortCut.Core.Types -- (CutConfig)
import ShortCut.Core.Util (lookupVar, digest)
import ShortCut.Core.Debug (debugPath, debugReadLines, debugWriteLines, debug)
import Data.String.Utils          (replace)
import Development.Shake.FilePath ((</>), (<.>), isAbsolute)
import Data.List                  (intersperse, isPrefixOf)

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

isGeneric :: FilePath -> Bool
isGeneric path = "$TMPDIR" `isPrefixOf` path || "$WORKDIR" `isPrefixOf` path

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

-- This is like the "resolve refs" part of argHashes, but works on plain paths in IO
-- resolveVar :: CutConfig -> CutPath -> IO CutPath
-- resolveVar cfg p@(CutPath path) =
--   -- TODO is just using CutPath directly here OK?
--   if "$TMPDIR/vars" `isPrefixOf` path
--     then resolveSymlinks cfg (fromCutPath cfg p) >>= resolveVar cfg . toCutPath cfg
--     else return p

-- resolveVars :: CutConfig -> [CutPath] -> IO [CutPath]
-- resolveVars cfg = mapM (resolveVar cfg)

-- TODO rename to tmpPath?
exprPath :: CutState -> CutExpr -> CutPath
exprPath s@(scr, _) (CutRef _ _ _ v) = exprPath s $ lookupVar v scr
exprPath s@(_, cfg) expr = debugPath cfg "exprPath" expr res
  where
    prefix = prefixOf expr
    rtype  = typeOf expr
    salt   = saltOf expr
    hashes = argHashes s expr
    res    = exprPathExplicit cfg prefix rtype salt hashes

exprPathExplicit :: CutConfig -> String -> CutType -> Int -> [String] -> CutPath
exprPathExplicit cfg prefix rtype salt hashes = toCutPath cfg path
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

---------------
-- io checks --
---------------

-- These are just to alert me of programming mistakes,
-- and can be removed once the rest of the IO stuff is solid.

checkLits :: [String] -> [String] -- (or error, but let's ignore that)
checkLits = map checkLit
  where
    checkLit lit = if isGeneric lit
                     then error $ "placeholder in lit: '" ++ lit ++ "'"
                     else lit

checkPaths :: [FilePath] -> [FilePath]
checkPaths = map checkPath
  where
    checkPath path = if isAbsolute path || isGeneric path
                       then path
                       else error $ "invalid path: '" ++ path ++ "'"

-------------
-- file io --
-------------

readPaths :: CutConfig -> FilePath -> Action [CutPath]
readPaths cfg path = fmap (map CutPath . checkPaths) (debugReadLines cfg path)

-- TODO something safer than head!
readPath :: CutConfig -> FilePath -> Action CutPath
readPath cfg path = readPaths cfg path >>= return . head

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
writePaths cfg out cpaths = debugWriteLines cfg out paths >> trackWrite paths
  where
    paths = map (\(CutPath path) -> path) cpaths

writePath :: CutConfig -> FilePath -> CutPath -> Action ()
writePath cfg out path = writePaths cfg out [path]

readLits :: CutConfig -> FilePath -> Action [String]
readLits cfg path = debugReadLines cfg path >>= return . checkLits

-- TODO something safer than head!
-- TODO error if they contain $TMPDIR or $WORKDIR?
readLit :: CutConfig -> FilePath -> Action String
readLit cfg path = readLits cfg path >>= return . head

-- TODO error if they contain $TMPDIR or $WORKDIR?
writeLits :: CutConfig -> FilePath -> [String] -> Action ()
writeLits cfg path lits = debugWriteLines cfg path $ checkLits lits

writeLit :: CutConfig -> FilePath -> String -> Action ()
writeLit cfg path lit = writeLits cfg path [lit]

----------------------------------------
-- read and write tmpfiles as strings --
----------------------------------------

-- These are useful for generic functions like in Sets.hs which operate on
-- "lists of whatever". You include the CutType (of each element, not the
-- list!) so it knows how to convert to/from String, and then within the
-- function you treat them as Strings.

readStrings :: CutType -> CutConfig -> FilePath -> Action [String]
readStrings etype cfg path = if etype' `elem` [str, num]
  then readLits cfg path
  else (fmap . map) (fromCutPath cfg) (readPaths cfg path)
  where
    etype' = debug cfg ("readStrings (each " ++ extOf etype ++ ") from " ++ path) etype

readString :: CutType -> CutConfig -> FilePath -> Action String
readString etype cfg path = readStrings etype cfg path >>= return . head

writeStrings :: CutType -> CutConfig -> FilePath -> [String] -> Action ()
writeStrings etype cfg out whatevers = if etype' `elem` [str, num]
  then writeLits cfg out whatevers
  else writePaths cfg out $ map (toCutPath cfg) whatevers
  where
    etype' = debug cfg ("writeStrings (each " ++ extOf etype ++ "): " ++ show (take 3 whatevers)) etype

writeString :: CutType -> CutConfig -> FilePath -> String -> Action ()
writeString etype cfg out whatever = writeStrings etype cfg out [whatever]
