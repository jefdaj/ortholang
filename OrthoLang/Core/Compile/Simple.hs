module OrthoLang.Core.Compile.Simple where

import OrthoLang.Core.Compile.Basic

import Development.Shake
import Development.Shake.FilePath (isAbsolute)
import OrthoLang.Core.Types
import OrthoLang.Core.Pretty
import qualified Data.Map.Strict as M

import OrthoLang.Core.Paths (cacheDir, exprPath, exprPathExplicit, toOrthoLangPath,
                            fromOrthoLangPath, varPath, OrthoLangPath)

import Data.IORef                 (atomicModifyIORef')
import Data.List                  (intersperse, isPrefixOf, isInfixOf)
import Development.Shake.FilePath ((</>), (<.>), takeFileName)
import OrthoLang.Core.Actions      (runCmd, CmdDesc(..), traceA, debugA, need',
                                   readLit, readLits, writeLit, writeLits, hashContent,
                                   readLitPaths, writePaths, symlink)
-- import OrthoLang.Core.Locks        (withWriteLock')
import OrthoLang.Core.Sanitize     (hashIDsFile2, readHashedIDs)
import OrthoLang.Core.Util         (absolutize, resolveSymlinks, stripWhiteSpace,
                                   digest, removeIfExists, headOrDie, trace, unlessExists)
import System.FilePath            (takeExtension)
import System.Exit                (ExitCode(..))
import System.Directory           (createDirectoryIfMissing)

import Data.Maybe (isJust, fromJust)

import OrthoLang.Core.Paths (fromOrthoLangPath, decodeNewRulesDeps)
import OrthoLang.Core.Actions (writeCachedLines, need', readLit)
import System.FilePath ((</>))
import Control.Monad (when)


-- based on https://stackoverflow.com/a/18627837
-- uniqLines :: Ord a => [a] -> [a]
-- uniqLines = unlines . toList . fromList . lines

-- takes an action fn with any number of args and calls it with a tmpdir.
-- TODO rename something that goes with the map fns?
rSimple :: (OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ()) -> RulesFn
rSimple actFn = rSimple' Nothing actFn'
  where
    actFn' cfg ref ids _ args = actFn cfg ref ids args -- drop unused tmpdir

rSimpleTmp :: String
           -> (OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangPath -> [OrthoLangPath] -> Action ())
           -> RulesFn
rSimpleTmp prefix = rSimple' (Just prefix)

{- For scripts that just need some args passed to them. The first will be the
 - outPath, and the rest actual args. The string is the script name.
 -}
rSimpleScript :: String -> RulesFn
rSimpleScript = rSimple . aSimpleScript

rSimpleScriptPar :: String -> RulesFn
rSimpleScriptPar = rSimple . aSimpleScriptPar

rSimpleScriptNoFix :: String -> RulesFn
rSimpleScriptNoFix = rSimple . aSimpleScriptNoFix

aSimpleScriptNoFix :: String -> (OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ())
aSimpleScriptNoFix = aSimpleScript' False False

aSimpleScript :: String -> (OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ())
aSimpleScript = aSimpleScript' False True

aSimpleScriptPar :: String -> (OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ())
aSimpleScriptPar = aSimpleScript' True True

aSimpleScript' :: Bool -> Bool -> String -> (OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ())
aSimpleScript' parCmd fixEmpties script cfg ref ids (out:ins) = aSimple' cfg ref ids out actFn Nothing ins
  where
    -- TODO is tmpDir used here at all? should it be?
    -- TODO match []?
    actFn c r _ t (o:is) = let o'  = fromOrthoLangPath c o -- TODO better var names here
                               t'  = fromOrthoLangPath c t
                               is' = map (fromOrthoLangPath c) is
                           -- in wrappedCmdWrite parCmd fixEmpties c r o' is' [] [Cwd t'] script (o':is')
                           in runCmd c r $ CmdDesc
                             { cmdBinary = script
                             , cmdArguments = o':is'
                             , cmdFixEmpties = fixEmpties
                             , cmdParallel = parCmd
                             , cmdInPatterns = is'
                             , cmdOutPath = o'
                             , cmdExtraOutPaths = []
                             , cmdSanitizePaths = []
                             , cmdOptions = [Cwd t'] -- TODO remove?
                             , cmdExitCode = ExitSuccess
                             , cmdRmPatterns = [o'] -- TODO is this a sane default?
                             }
    actFn _ _ _ _ _ = fail "bad argument to aSimpleScript actFn"
aSimpleScript' _ _ _ _ _ _ as = error $ "bad argument to aSimpleScript: " ++ show as

rSimple' :: Maybe String
         -> (OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangPath -> [OrthoLangPath] -> Action ())
         -> RulesFn
rSimple' mTmpPrefix actFn s@(_, cfg, ref, ids) e@(OrthoLangFun _ _ _ _ exprs) = do
  argPaths <- mapM (rExpr s) exprs
  let argPaths' = map (\(ExprPath p) -> toOrthoLangPath cfg p) argPaths
  outPath' %> \_ -> aSimple' cfg ref ids outPath actFn mTmpDir argPaths'
  return (ExprPath outPath')
  where
    mTmpDir  = fmap (cacheDir cfg) mTmpPrefix -- TODO tables bug here?
    outPath  = exprPath s e
    outPath' = fromOrthoLangPath cfg outPath
rSimple' _ _ _ _ = fail "bad argument to rSimple'"

-- TODO aSimpleScript that calls aSimple' with a wrappedCmd as the actFn
-- TODO rSimpleScript that calls rSimple + that

-- TODO need to handle empty lists here?
aSimple' ::  OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangPath
         -> (OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangPath -> [OrthoLangPath] -> Action ())
         -> Maybe OrthoLangPath -> [OrthoLangPath] -> Action ()
aSimple' cfg ref ids outPath actFn mTmpDir argPaths = do
  need' cfg ref "ortholang.core.compile.basic.aSimple'" argPaths'
  argPaths'' <- liftIO $ mapM (fmap (toOrthoLangPath cfg) . resolveSymlinks (Just $ cfgTmpDir cfg)) argPaths'
  let o' = debug cfg ("aSimple' outPath': " ++ outPath' ++ "'") outPath
      as = debug cfg ("aSimple' argsPaths'': " ++ show argPaths'') argPaths''
  actFn cfg ref ids tmpDir (o':as)
  trackWrite [out] -- TODO remove?
  where
    -- TODO probably not "simple tmp" anymore... remove? rename?
    hashes     = concat $ intersperse "/" $ map digest argPaths'
    argPaths'  = map (fromOrthoLangPath cfg) argPaths
    outPath'   = fromOrthoLangPath cfg outPath
    out = traceA "aSimple'" outPath' (outPath':tmpDir':argPaths')
    (tmpDir, tmpDir') = case mTmpDir of
                Nothing  -> (toOrthoLangPath cfg $ cfgTmpDir cfg, cfgTmpDir cfg)
                Just dir -> (toOrthoLangPath cfg d, d)
                  where
                    d = fromOrthoLangPath cfg dir </> hashes
