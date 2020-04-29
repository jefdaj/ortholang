module OrthoLang.Interpreter.Compile.Simple
  (

  -- * Used in Core
    aSimpleScript

  -- * Action makers
  , aSimpleScriptNoFix
  , aSimpleScriptPar

  -- * RulesFns
  , rSimple
  , rSimpleScript
  , rSimpleScriptPar
  , rSimpleTmp

  )
  where

import OrthoLang.Interpreter.Compile.Basic
import OrthoLang.Interpreter.Compile.NewRules

import Prelude hiding (error)
import OrthoLang.Debug
import Development.Shake
import OrthoLang.Types

import OrthoLang.Interpreter.Paths (cacheDir, exprPath, toPath,
                            fromPath, Path)

import Data.List                  (intersperse)
import Development.Shake.FilePath ((</>))
import OrthoLang.Interpreter.Actions      (runCmd, CmdDesc(..), traceA, need')
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
  let loc = "interpreter.compile.simple.aSimpleScript'"
      actFn t (o:is) = let o'  = fromPath loc c o -- TODO better var names here
                           t'  = fromPath loc c t
                           is' = map (fromPath loc c) is
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
      loc = "interpreter.compile.basic.rSimple'"
      outPath  = exprPath cfg dRef scr e
      outPath' = fromPath loc cfg outPath
      argPaths' = map (\(ExprPath p) -> toPath loc cfg p) argPaths
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
      loc = "interpreter.compile.basic.aSimple'"
      argPaths'  = map (fromPath loc cfg) argPaths
      outPath'   = fromPath loc cfg outPath
      out = traceA loc outPath' (outPath':tmpDir':argPaths')
      (tmpDir, tmpDir') = case mTmpDir of
                  Nothing  -> (toPath loc cfg $ tmpdir cfg, tmpdir cfg)
                  Just dir -> let d = fromPath loc cfg dir </> hashes in (toPath loc cfg d, d)
  need' loc argPaths'
  argPaths'' <- liftIO $ mapM (fmap (toPath loc cfg) . resolveSymlinks (Just $ tmpdir cfg)) argPaths'
  let o' = debugC loc ("outPath': " ++ outPath' ++ "\"") outPath
      as = debugC loc ("argsPaths'': " ++ show argPaths'') argPaths''
  actFn tmpDir (o':as)
  trackWrite [out] -- TODO remove?
