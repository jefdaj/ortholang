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
 - |   |-- crb_blast_all
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
 - Files in the cache will be organized however seems best on a per-module
 - basis.
 -
 - Expression paths are determined by the exprPath function. It gets the base
 - name by `show`ing the expression and `digest`ing the resulting `String`, the
 - extension based on type, and the folder based on constructor + function name
 - if a function. Some made up examples:
 -
 -   ~/.shortcut/exprs/cut_list/f987e9b98a.str.list
 -   ~/.shortcut/exprs/cut_lit/a09f8e8b9c.str
 -   ~/.shortcut/exprs/crb_blast/38978s9a79.crb
 -   ~/.shortcut/exprs/gbk_to_fna/289379af7a.fna
 -
 - Var links are the user-given name plus type extension.
 -
 - Rough list of changes that need to be made to get there:
 -   DONE stop exporting cacheDir and exprDir directly
 -   DONE add newtype wrappers for different types of paths
 -   TODO refactor functions, adding newtypes everywhere as you go
 -}

module ShortCut.Core.Paths
  ( hashedTmp     -- TODO rename exprPath
  , hashedTmp'    -- TODO rename exprPath', or remove if possible
  , scriptTmpFile -- TODO replace with updated cacheDir
  , scriptTmpDir  -- TODO replace with updated cacheDir
  , namedTmp      -- TODO rename varPath
  )
  where

import ShortCut.Core.Types
import ShortCut.Core.Debug (debug)
import ShortCut.Core.Util         (digest)
import Development.Shake.FilePath ((<.>), (</>))
import System.FilePath            (makeRelative)

-- TODO move these to Types
newtype CacheDir = CacheDir FilePath -- ~/.shortcut/cache/<modname>
newtype ExprPath = ExprPath FilePath -- ~/.shortcut/exprs/<fnname>/<hash>.<type>
newtype VarPath  = VarPath  FilePath -- ~/.shortcut/vars/<varname>.<type>
newtype ResPath  = ResPath  FilePath -- ~/.shortcut/vars/result (what about nesting?)

-- TODO remove to stop the temptation to use it
cacheDir :: CutConfig -> FilePath
cacheDir cfg = cfgTmpDir cfg </> "cache"

-- TODO what was this even for? remove it? yeah to stop the temptation :D
exprDir :: CutConfig -> FilePath
exprDir cfg = cacheDir cfg </> "shortcut"

-- TODO flip arguments for consistency with everything else There's a special
-- case for "result", which is like the "main" function of a ShortCut script,
-- and always goes to <tmpdir>/result.
-- TODO auto-apply fromShortCutList to result?
-- TODO rename varPath
namedTmp :: CutConfig -> CutVar -> CutExpr -> FilePath
namedTmp cfg (CutVar var) expr = debug cfg ("tmpfile:" ++ rtn) rtn
  where
    base = if var == "result" then var else var <.> extOf (typeOf expr)
    rtn  = cfgTmpDir cfg </> base

-- TODO extn can be found inside expr now; remove it
-- TODO rename exprPath?
hashedTmp :: CutConfig -> CutExpr -> [FilePath] -> FilePath
hashedTmp cfg expr paths = debug cfg ("tmpfile: " ++ rtn) rtn
  where
    paths' = map (makeRelative $ cfgTmpDir cfg) paths
    uniq   = digest $ unlines $ (show expr):paths'
    rtn    = exprDir cfg </> uniq <.> extOf (typeOf expr)

-- overrides the expression's "natural" extension
-- TODO figure out how to remove!
-- TODO rename exprPath'? or remove?
hashedTmp' :: CutConfig -> CutType -> CutExpr -> [FilePath] -> FilePath
hashedTmp' cfg rtn expr paths = debug cfg ("tmpfile: " ++ rtn') rtn'
  where
    paths' = map (makeRelative $ cfgTmpDir cfg) paths
    uniq   = digest $ unlines $ (show expr):paths'
    rtn'   = exprDir cfg </> uniq <.> extOf rtn

-- TODO should this handle calling cfgTmpDir too?
-- TODO rename cacheDir or something
scriptTmpDir :: Show a => CutConfig -> FilePath -> a -> FilePath
scriptTmpDir cfg tmpDir uniq = debug cfg ("tmpdir: " ++ rtn) rtn
  where
    rtn = tmpDir </> digest uniq

-- TODO is this needed at all?
scriptTmpFile :: Show a => CutConfig -> FilePath -> a -> String -> FilePath
scriptTmpFile cfg tmpDir uniq ext = debug cfg ("tmpfile: " ++ rtn) rtn
  where
    rtn = tmpDir </> digest uniq <.> ext
