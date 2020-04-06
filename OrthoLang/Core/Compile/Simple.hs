module OrthoLang.Core.Compile.Simple
  (

  -- * Action makers
    aSimpleScript
  , aSimpleScriptNoFix
  , aSimpleScriptPar

  -- * RulesFns
  , rSimple
  , rSimpleScript
  , rSimpleScriptPar
  , rSimpleTmp

  )
  where

import OrthoLang.Core.Compile.Basic

import Prelude hiding (error)
import OrthoLang.Debug
import Development.Shake
import OrthoLang.Core.Types

import OrthoLang.Core.Paths (cacheDir, exprPath, toPath,
                            fromPath, Path)

import Data.List                  (intersperse)
import Development.Shake.FilePath ((</>))
import OrthoLang.Core.Actions      (runCmd, CmdDesc(..), traceA, need')
import OrthoLang.Util         (resolveSymlinks, digest)
import System.Exit                (ExitCode(..))

import Data.Maybe (fromJust)


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
aSimpleScript' _ _ _ as = error "aSimpleScript'" $ "bad argument: " ++ show as

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
  let o' = debugC cfg "aSimple'" ("outPath': " ++ outPath' ++ "\"") outPath
      as = debugC cfg "aSimple'" ("argsPaths'': " ++ show argPaths'') argPaths''
  actFn tmpDir (o':as)
  trackWrite [out] -- TODO remove?
