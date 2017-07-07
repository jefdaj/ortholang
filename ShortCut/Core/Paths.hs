module ShortCut.Core.Paths
  ( hashedTmp
  , hashedTmp'
  , scriptTmpFile
  , scriptTmpDir
  -- , cacheDir
  -- , exprDir
  , namedTmp
  )
  where

import ShortCut.Core.Types
import ShortCut.Core.Debug (debug)
import ShortCut.Core.Util         (digest)
import Development.Shake.FilePath ((<.>), (</>))
import System.FilePath            (makeRelative)

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
