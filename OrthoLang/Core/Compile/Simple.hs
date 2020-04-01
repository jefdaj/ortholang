module OrthoLang.Core.Compile.Simple where

import OrthoLang.Core.Compile.Basic

import Development.Shake
import Development.Shake.FilePath (isAbsolute)
import OrthoLang.Core.Types
import OrthoLang.Core.Pretty
import qualified Data.Map.Strict as M

import OrthoLang.Core.Paths (cacheDir, exprPath, unsafeExprPathExplicit, toPath,
                            fromPath, varPath, Path)

import Data.IORef                 (atomicModifyIORef')
import Data.List                  (intersperse, isPrefixOf, isInfixOf)
import Development.Shake.FilePath ((</>), (<.>), takeFileName)
import OrthoLang.Core.Actions      (runCmd, CmdDesc(..), traceA, debugA, need',
                                   readLit, readLits, writeLit, writeLits, hashContent,
                                   readLitPaths, writePaths, symlink)
-- import OrthoLang.Core.Locks        (withWriteLock')
import OrthoLang.Core.Sanitize     (hashIDsFile2, readIDs)
import OrthoLang.Util         (absolutize, resolveSymlinks, stripWhiteSpace,
                                   digest, removeIfExists, headOrDie, trace, unlessExists)
import System.FilePath            (takeExtension)
import System.Exit                (ExitCode(..))
import System.Directory           (createDirectoryIfMissing)

import Data.Maybe (isJust, fromJust)

import OrthoLang.Core.Paths (fromPath, decodeNewRulesDeps)
import OrthoLang.Core.Actions (writeCachedLines, need', readLit)
import System.FilePath ((</>))
import Control.Monad (when)


-- based on https://stackoverflow.com/a/18627837
-- uniqLines :: Ord a => [a] -> [a]
-- uniqLines = unlines . toList . fromList . lines

-- takes an action fn with any number of args and calls it with a tmpdir.
-- TODO rename something that goes with the map fns?
rSimple :: ([Path] -> Action ()) -> RulesFn
rSimple actFn = rSimple' Nothing actFn'
  where
    actFn' _ args = actFn args -- drop unused tmpdir

rSimpleTmp :: String
           -> (Path -> [Path] -> Action ())
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

aSimpleScriptNoFix :: String -> ([Path] -> Action ())
aSimpleScriptNoFix = aSimpleScript' False False

aSimpleScript :: String -> ([Path] -> Action ())
aSimpleScript = aSimpleScript' False True

aSimpleScriptPar :: String -> ([Path] -> Action ())
aSimpleScriptPar = aSimpleScript' True True

aSimpleScript' :: Bool -> Bool -> String -> ([Path] -> Action ())
aSimpleScript' parCmd fixEmpties script (out:ins) = do
  c <- fmap fromJust getShakeExtra
  -- TODO is tmpDir used here at all? should it be?
  -- TODO match []?
  let actFn t (o:is) = let o'  = fromPath c o -- TODO better var names here
                           t'  = fromPath c t
                           is' = map (fromPath c) is
                       -- in wrappedCmdWrite parCmd fixEmpties c r o' is' [] [Cwd t'] script (o':is')
                       in runCmd $ CmdDesc
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
      actFn _ _ = fail "bad argument to aSimpleScript actFn"
  aSimple' out actFn Nothing ins
aSimpleScript' _ _ _ as = error $ "bad argument to aSimpleScript: " ++ show as

rSimple' :: Maybe String
         -> (Path -> [Path] -> Action ())
         -> RulesFn
rSimple' mTmpPrefix actFn scr e@(Fun _ _ _ _ exprs) = do
  argPaths <- mapM (rExpr scr) exprs
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let mTmpDir  = fmap (cacheDir cfg) mTmpPrefix -- TODO tables bug here?
      outPath  = exprPath cfg dRef scr e
      outPath' = fromPath cfg outPath
      argPaths' = map (\(ExprPath p) -> toPath cfg p) argPaths
  outPath' %> \_ -> aSimple' outPath actFn mTmpDir argPaths'
  return (ExprPath outPath')
rSimple' _ _ _ _ = fail "bad argument to rSimple'"

-- TODO aSimpleScript that calls aSimple' with a wrappedCmd as the actFn
-- TODO rSimpleScript that calls rSimple + that

-- TODO need to handle empty lists here?
aSimple' ::  Path
         -> (Path -> [Path] -> Action ())
         -> Maybe Path -> [Path] -> Action ()
aSimple' outPath actFn mTmpDir argPaths = do
  -- TODO probably not "simple tmp" anymore... remove? rename?
  cfg <- fmap fromJust getShakeExtra
  let hashes     = concat $ intersperse "/" $ map digest argPaths'
      argPaths'  = map (fromPath cfg) argPaths
      outPath'   = fromPath cfg outPath
      out = traceA "aSimple'" outPath' (outPath':tmpDir':argPaths')
      (tmpDir, tmpDir') = case mTmpDir of
                  Nothing  -> (toPath cfg $ cfgTmpDir cfg, cfgTmpDir cfg)
                  Just dir -> let d = fromPath cfg dir </> hashes in (toPath cfg d, d)
  need' "ortholang.core.compile.basic.aSimple'" argPaths'
  argPaths'' <- liftIO $ mapM (fmap (toPath cfg) . resolveSymlinks (Just $ cfgTmpDir cfg)) argPaths'
  let o' = debug cfg "aSimple'" ("outPath': " ++ outPath' ++ "\"") outPath
      as = debug cfg "aSimple'" ("argsPaths'': " ++ show argPaths'') argPaths''
  actFn tmpDir (o':as)
  trackWrite [out] -- TODO remove?
