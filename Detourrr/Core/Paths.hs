-- TODO rename this module to TmpFiles?

{- Detourrr makes heavy use of tmpfiles, and this module controls where they go
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
 - `String`, the extension based on the `RrrType`, and the folder based on
 - constructor + function name if a function. Some made up examples:
 -
 -   ~/.detourrr/exprs/cut_list/f987e9b98a.str.list
 -   ~/.detourrr/exprs/cut_lit/a09f8e8b9c.str
 -   ~/.detourrr/exprs/crb_blast/38978s9a79.crb
 -   ~/.detourrr/exprs/gbk_to_fna/289379af7a.fna
 -
 - Var links are determined by `varPath` using the user-given name and `RrrType`.
 -}

module Detourrr.Core.Paths
  -- rrrpaths
  ( RrrPath()
  , toRrrPath
  , fromRrrPath
  , rrrPathString
  , stringRrrPath
  , toGeneric
  , fromGeneric
  -- cache dirs
  , cacheDir
  -- tmpfiles
  , argHashes
  -- , hashContent
  , exprPath
  , exprPathExplicit
  , varPath
  , checkLit
  , checkLits
  , checkPath
  , checkPaths
  -- , resolveVar
  -- , resolveVars
  -- file io
  -- , readPath
  -- , readPaths
  -- , readLitPaths
  -- , writePath
  -- , writePaths
  -- , readLit
  -- , readLits
  -- , writeLit
  -- , writeLits
  -- read and write tmpfiles as strings
  -- , readString
  -- , readStrings
  -- , writeString
  -- , writeStrings
  -- symlink stuff
  -- , tmpLink
  -- , symlink
  )
  where

-- import Development.Shake (Action, trackWrite, need, liftIO)
import Path (parseAbsFile, fromAbsFile)
import Detourrr.Core.Types -- (RrrConfig)
import Detourrr.Core.Config (debug)
import Detourrr.Core.Pretty (render, pPrint)
import Detourrr.Core.Util (digest)
-- import Detourrr.Core.Debug        (debugPath)
import Data.String.Utils          (replace)
import Development.Shake.FilePath ((</>), (<.>), isAbsolute)
import Data.List                  (intersperse, isPrefixOf)
-- import Data.IORef                 (IORef)

debugPath :: Show a => RrrConfig -> String -> RrrExpr -> a -> a
debugPath cfg name expr path = debug cfg msg path
  where
    ren = render $ pPrint expr
    msg = name ++ " for '" ++ ren ++ "' is " ++ show path -- TODO include types?

--------------
-- rrrpaths --
--------------

-- Replace current absolute paths with generic placeholders that won't change
-- when the tmpDir is moved later or whatever.
-- TODO rewrite with a more elegant [(fn, string)] if there's time
toGeneric :: RrrConfig -> String -> String
toGeneric cfg txt = replace (cfgWorkDir cfg) "$WORKDIR"
                  $ replace (cfgTmpDir  cfg) "$TMPDIR"
                  $ txt

-- Replace generic path placeholders with current paths
-- TODO rewrite with a more elegant [(fn, string)] if there's time
fromGeneric :: RrrConfig -> String -> String
fromGeneric cfg txt = replace "$WORKDIR" (cfgWorkDir cfg)
                    $ replace "$TMPDIR"  (cfgTmpDir  cfg)
                    $ checkPath txt

isGeneric :: FilePath -> Bool
isGeneric path
  = path == "<<emptylist>>" -- TODO could this be <<emptystr>>?
  || "$TMPDIR"  `isPrefixOf` path
  || "$WORKDIR" `isPrefixOf` path

-- TODO print warning on failure?
toRrrPath :: RrrConfig -> FilePath -> RrrPath
toRrrPath cfg = RrrPath . checkPath . toGeneric cfg . normalize
  where
    normalize p = case parseAbsFile p of
      Nothing -> error $ "toRrrPath can't parse: " ++ p
      Just p' -> fromAbsFile p'

fromRrrPath :: RrrConfig -> RrrPath -> FilePath
fromRrrPath cfg (RrrPath path) = fromGeneric cfg path

-- weird, but needed for writing rrrpaths to files in Actions.hs
rrrPathString :: RrrPath -> String
rrrPathString (RrrPath path) = path

-- TODO this is basically just exporting RrrPath right? any better way?
stringRrrPath :: String -> RrrPath
stringRrrPath = RrrPath

----------------
-- cache dirs --
----------------

cacheDir :: RrrConfig -> String -> RrrPath
cacheDir cfg modName = toRrrPath cfg path
  where
    path = cfgTmpDir cfg </> "cache" </> modName

-- TODO cacheDirUniq or Explicit?

--------------
-- tmpfiles --
--------------

-- This is just a convenience used in exprPath
-- TODO rename hSomething?
argHashes :: RrrState -> RrrExpr -> [String]
argHashes s@(scr,_, _, _) (RrrRef _ _ _ v) = case lookup v scr of
                                         Nothing -> error $ "no such var " ++ show v
                                         Just e  -> argHashes s e
argHashes _ (RrrLit  _ _     v    ) = [digest v]
argHashes s (RrrFun  _ _ _ _ es   ) = map (digest . exprPath s) es
argHashes s (RrrBop  _ _ _ _ e1 e2) = map (digest . exprPath s) [e1, e2]
argHashes s (RrrList _ _ _   es   ) = [digest $ map (digest . exprPath s) es]
argHashes s (RrrRules (CompiledExpr e _)) = argHashes s e

-- This is like the "resolve refs" part of argHashes, but works on plain paths in IO
-- resolveVar :: RrrConfig -> RrrPath -> IO RrrPath
-- resolveVar cfg p@(RrrPath path) =
--   -- TODO is just using RrrPath directly here OK?
--   if "$TMPDIR/vars" `isPrefixOf` path
--     then resolveSymlinks cfg True (fromRrrPath cfg p) >>= resolveVar cfg . toRrrPath cfg
--     else return p

-- resolveVars :: RrrConfig -> [RrrPath] -> IO [RrrPath]
-- resolveVars cfg = mapM (resolveVar cfg)

{- An attempt to speed up file access by making a tree of smaller dirs instead
 - of one giant one with a million+ files in it. Since it would complicate the
 - .tree files to split everything up, for now I just have a list of dirs that
 - are likely to benefit from it.
 - TODO write this in haskell instead of python! (currently in split_faa)
 -}
-- expandHashDirs :: FilePath -> FilePath
-- expandHashDirs = joinPath . map expandDir . splitPath 
--   where
--     expandDir d = if d `elem` dirsToExpand then undefined else d
--     dirsToExpand = ["load_faa"]
--     splitPath = undefined
--     joinPath = undefined

-- TODO rename to tmpPath?
exprPath :: RrrState -> RrrExpr -> RrrPath
exprPath s@(scr, _, _, _) (RrrRef _ _ _ v) = case lookup v scr of
                                         Nothing -> error $ "no such var " ++ show v ++ "\n" ++ show scr
                                         Just e  -> exprPath s e
exprPath s@(_, cfg, _, _) expr = debugPath cfg "exprPath" expr res
  where
    prefix = prefixOf expr
    rtype  = typeOf expr
    salt   = saltOf expr
    hashes = argHashes s expr
    res    = exprPathExplicit cfg prefix rtype salt hashes

exprPathExplicit :: RrrConfig -> String -> RrrType -> Int -> [String] -> RrrPath
exprPathExplicit cfg prefix rtype salt hashes = toRrrPath cfg path
  where
    dir  = cfgTmpDir cfg </> "exprs" </> prefix
    base = (concat $ intersperse "_" hashes) ++ suf
    suf  = if salt == 0 then "" else "_" ++ show salt
    path = dir </> base <.> extOf rtype

-- TODO remove VarPath, ExprPath types once RrrPath works everywhere
varPath :: RrrConfig -> RrrVar -> RrrExpr -> RrrPath
varPath cfg (RrrVar var) expr = toRrrPath cfg $ cfgTmpDir cfg </> "vars" </> base
  where
    base = if var == "result" then var else var <.> extOf (typeOf expr)

---------------
-- io checks --
---------------

-- These are just to alert me of programming mistakes,
-- and can be removed once the rest of the IO stuff is solid.
checkLit :: String -> String
checkLit lit = if isGeneric lit
                 then error $ "placeholder in lit: '" ++ lit ++ "'"
                 else lit

checkLits :: [String] -> [String] -- (or error, but let's ignore that)
checkLits = map checkLit


checkPath :: FilePath -> FilePath
checkPath path = if isAbsolute path || isGeneric path
                   then path
                   else error $ "invalid path: '" ++ path ++ "'"

checkPaths :: [FilePath] -> [FilePath]
checkPaths = map checkPath
