{-# LANGUAGE OverloadedStrings #-}

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
 - |   |-- replace_each
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
  ( CutPath()
  , toCutPath
  , fromCutPath
  , sharedPath
  , cutPathString
  , stringCutPath
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
  , upBy
  , makeTmpdirRelative
  )
  where

import Path (parseAbsFile, fromAbsFile)
import ShortCut.Core.Types -- (CutConfig)
-- import ShortCut.Core.Config (debug)
import ShortCut.Core.Pretty (render, pPrint)
import ShortCut.Core.Util (digest, trace)
import Data.String.Utils          (replace)
import Development.Shake.FilePath ((</>), (<.>), isAbsolute)
import Data.List                  (intersperse, isPrefixOf)
import Data.List.Split            (splitOn)
-- import Data.IORef                 (IORef)

import Text.PrettyPrint.HughesPJClass (Pretty)

-- TODO take Text instead?
traceP :: (Pretty a, Show b) => String -> a -> b -> b
traceP name expr path = trace ("core.paths." ++ name) msg path
  where
    ren = render $ pPrint expr
    msg = ren ++ " -> " ++ show path -- TODO include types?

--------------
-- cutpaths --
--------------

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
                    $ checkPath txt

isGeneric :: FilePath -> Bool
isGeneric path
  = path == "<<emptylist>>" -- TODO could this be <<emptystr>>?
  || "$TMPDIR"  `isPrefixOf` path
  || "$WORKDIR" `isPrefixOf` path

-- TODO print warning on failure?
toCutPath :: CutConfig -> FilePath -> CutPath
toCutPath cfg = CutPath . checkPath . toGeneric cfg . normalize
  where
    normalize p = case parseAbsFile p of
      Nothing -> error $ "toCutPath can't parse: " ++ p
      Just p' -> fromAbsFile p'

fromCutPath :: CutConfig -> CutPath -> FilePath
fromCutPath cfg (CutPath path) = fromGeneric cfg path

sharedPath :: CutConfig -> CutPath -> Maybe FilePath
sharedPath cfg (CutPath path) = fmap (\sd -> replace "$TMPDIR" sd path) (cfgShare cfg)

-- weird, but needed for writing cutpaths to files in Actions.hs
cutPathString :: CutPath -> String
cutPathString (CutPath path) = path

-- TODO this is basically just exporting CutPath right? any better way?
stringCutPath :: String -> CutPath
stringCutPath = CutPath

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
argHashes s@(scr,_, _, _) (CutRef _ _ _ v) = case lookup v scr of
                                         Nothing -> error $ "no such var " ++ show v
                                         Just e  -> argHashes s e
argHashes _ (CutLit  _ _     v    ) = [digest v]
argHashes s (CutFun  _ _ _ _ es   ) = map (digest . exprPath s) es
argHashes s (CutBop  _ _ _ _ e1 e2) = map (digest . exprPath s) [e1, e2]
argHashes s (CutList _ _ _   es   ) = [digest $ map (digest . exprPath s) es]
argHashes _ (CutRules (CompiledExpr _ p _)) = [digest p] -- TODO is this OK? it's about all we can do

-- This is like the "resolve refs" part of argHashes, but works on plain paths in IO
-- resolveVar :: CutConfig -> CutPath -> IO CutPath
-- resolveVar cfg p@(CutPath path) =
--   -- TODO is just using CutPath directly here OK?
--   if "$TMPDIR/vars" `isPrefixOf` path
--     then resolveSymlinks cfg True (fromCutPath cfg p) >>= resolveVar cfg . toCutPath cfg
--     else return p

-- resolveVars :: CutConfig -> [CutPath] -> IO [CutPath]
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
exprPath :: CutState -> CutExpr -> CutPath
exprPath (_, cfg, _, _) (CutRules (CompiledExpr _ (ExprPath p) _)) = toCutPath cfg p
exprPath s@(scr, _, _, _) (CutRef _ _ _ v) = case lookup v scr of
                                         Nothing -> error $ "no such var " ++ show v ++ "\n" ++ show scr
                                         Just e  -> exprPath s e
exprPath s@(_, cfg, _, _) expr = traceP "exprPath" expr res
  where
    prefix = prefixOf expr
    rtype  = typeOf expr
    salt   = saltOf expr
    hashes = argHashes s expr
    res    = exprPathExplicit cfg prefix rtype salt hashes

exprPathExplicit :: CutConfig -> String -> CutType -> RepeatSalt -> [String] -> CutPath
exprPathExplicit cfg prefix rtype (RepeatSalt s) hashes = toCutPath cfg path
  where
    dir  = cfgTmpDir cfg </> "exprs" </> prefix
    base = (concat $ intersperse "_" $ hashes ++ [show s])
    path = dir </> base <.> extOf rtype

-- TODO remove VarPath, ExprPath types once CutPath works everywhere
varPath :: CutConfig -> CutVar -> CutExpr -> CutPath
varPath cfg (CutVar (ReplaceID rep) var) expr = toCutPath cfg $ cfgTmpDir cfg </> repDir </> base
  where
    base = if var == "result" then var else var <.> extOf (typeOf expr)
    repDir = case rep of
               Nothing -> "vars"
               Just r  -> "reps" </> r -- TODO digest other stuff too, like the expr?

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


-----------
-- utils --
-----------

-- TODO move this somewhere else?

-- TODO there must be a standard function for this right?
-- TODO guard that the top level stays to prevent it being /
upBy :: Int -> CutPath -> CutPath
upBy n (CutPath path) = CutPath path'
  where
    components = splitOn  "/" path -- TODO allow other delims?
    components' = reverse $ drop n $ reverse components
    path' = concat $ intersperse "/" $ components'

{- For passing scripts paths that don't depend on the $TMPDIR location, but
 - also don't require any shortcut funny business to read. It relies on the
 - assumption that the script will be called from inside $TMPDIR. The level
 - is how many ..s to add to get back up to $TMPDIR from where you call it.
 - TODO any good way to simplify that?
 -}
makeTmpdirRelative :: Int -> CutPath -> FilePath
makeTmpdirRelative level (CutPath path) = replace "$TMPDIR" dots path
  where
    dots = concat $ intersperse "/" $ take level $ repeat ".."
