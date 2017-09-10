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
 -   ~/.shortcut/exprs/cut_set/f987e9b98a.str.list
 -   ~/.shortcut/exprs/cut_lit/a09f8e8b9c.str
 -   ~/.shortcut/exprs/crb_blast/38978s9a79.crb
 -   ~/.shortcut/exprs/gbk_to_fna/289379af7a.fna
 -
 - Var links are determined by `varPath` using the user-given name and `CutType`.
 -
 - Rough list of changes that need to be made to get there:
 -   DONE stop exporting cacheDir and exprDir directly
 -   DONE add newtype wrappers for different types of paths
 -   DONE refactor functions, adding newtypes everywhere as you go
 -   TODO introduce smart constructors (or only use cExpr) to prevent mistakes
 -}

module ShortCut.Core.Paths
  ( cacheDir
  , cacheDirUniq
  , cacheFile
  , exprPath
  , exprPathExplicit
  , varPath
  -- TODO resPath?
  )
  where

import ShortCut.Core.Types

import Development.Shake.FilePath ((<.>), (</>))
import ShortCut.Core.Util         (digest)
import ShortCut.Core.Debug (debug)
import Data.List (isInfixOf)

-- TODO should this handle calling cfgTmpDir too?
-- TODO decide tmpDir from the config
cacheDir :: CutConfig -> String -> CacheDir
cacheDir cfg modName = CacheDir $ cfgTmpDir cfg </> "cache" </> modName

-- Creates a unique hashed directory inside the main module cache dir.
-- Needed for scripts that name their tmpfiles the same each time they're run
-- (I'm looking at you, crb-blast...)
cacheDirUniq :: Show a => CutConfig -> String -> a -> CacheDir
cacheDirUniq cfg modName showable = CacheDir $ d </> digest showable
  where
    (CacheDir d) = cacheDir cfg modName

-- TODO is this needed at all?
cacheFile :: Show a => CutConfig -> String -> a -> String -> FilePath
cacheFile cfg modName uniq ext = d </> digest uniq <.> ext
  where
    (CacheDir d) = cacheDir cfg modName

-- helper fr exprPath* that finds the right subdirectory
-- CutLit  CutType Int String
-- CutRef  CutType Int [CutVar] CutVar -- do refs need a salt? yes! (i think?)
-- CutBop  CutType Int [CutVar] String  CutExpr CutExpr
-- CutFun  CutType Int [CutVar] String [CutExpr]
-- CutSet CutType Int [CutVar] [CutExpr]
exprPrefix :: CutExpr -> String
exprPrefix (CutLit _ _ _       ) = "cut_lit"
exprPrefix (CutRef _ _ _ _     ) = "cut_ref"
exprPrefix (CutBop _ _ _ _ _ _ ) = "cut_bop" -- TODO individual names?
exprPrefix (CutSet _ _ _ _    ) = "cut_set"
exprPrefix (CutFun _ _ _ name _) = name

exprPath :: CutConfig -> Bool -> CutExpr -> [ExprPath] -> ExprPath
exprPath cfg noFullPaths expr paths =
  exprPathExplicit cfg noFullPaths (typeOf expr) (exprPrefix expr)
                   (show expr:map show paths)

-- Same as exprPath, except you also set the type. This is needed when writing
-- Haskell functions that modify ShortCut functions, such as the r* ones in
-- ModuleAPI.hs
-- The noFullPaths thing helps with debugging, but could be removed later
exprPathExplicit :: CutConfig -> Bool -> CutType -> String -> [String] -> ExprPath
exprPathExplicit cfg noFullPaths rtn prefix strings = ExprPath rtn'
  where
    msg p = "found absolute path '" ++ p
              ++ "'.\nfull exprPathExplicit args:\n"
              ++ unlines ([show rtn, prefix] ++ strings)
    strings' = map (\s -> if (cfgTmpDir cfg) `isInfixOf` s
                            then error $ msg s
                            else debug cfg ("path ok: " ++ s) s) strings
    uniq   = digest $ unlines $ if noFullPaths then strings' else strings
    rtn'   = cfgTmpDir cfg </> "exprs" </> prefix </> uniq <.> extOf rtn

-- TODO flip arguments for consistency with everything else There's a special
-- case for "result", which is like the "main" function of a ShortCut script,
-- and always goes to <tmpdir>/result.
-- TODO auto-apply fromShortCutSet to result?
-- TODO rename varPath
varPath :: CutConfig -> CutVar -> CutExpr -> VarPath
varPath cfg (CutVar var) expr = VarPath $ cfgTmpDir cfg </> "vars" </> base
  where
    base = if var == "result" then var else var <.> extOf (typeOf expr)
