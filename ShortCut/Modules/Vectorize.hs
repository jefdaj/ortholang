-- This is named by analogy to R's Vectorize function, although I don't know if
-- the analogy is accurate. What this does is take a function and create one
-- that works the same, but on a list of the last argument type. For example
-- crb_blast compares a query to a target, and the vectorized version
-- crb_blast_all compares a query to a list of targets. No summary is performed
-- on the rsults; you just get back a list of the regular return type.

-- TODO remove this module and put what's left in Core somewhere

module ShortCut.Modules.Vectorize where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Debug        (debugReadLines)
import Development.Shake.FilePath ((</>))
import ShortCut.Core.Compile      (cExpr, hashedTmp, hashedTmp', scriptTmpFile, scriptTmpDir, exprDir)
import ShortCut.Modules.Repeat    (extractExprs)
import System.FilePath            (makeRelative)
import System.Directory           (createDirectoryIfMissing)

-- TODO is there any point to empty ones? maybe just import from other files...
--      could have a separate set of API/dev modules or something
cutModule :: CutModule
cutModule = CutModule
  { mName = "vectorize"
  , mFunctions = []
  }

-- TODO enforce that lists are actually all the same type (in parser)!
tVectorize :: ([CutType] -> Either String CutType) ->  [CutType]
           -> Either String CutType
tVectorize _ [] = Left "bad args to tVectorize"
tVectorize tFn argTypes = case tFn argTypes' of
  Left err -> Left err
  Right r  -> let res = (Right $ ListOf r) in res
  where
    (ListOf t) = last argTypes
    argTypes'  = init argTypes ++ [t]

rMapLastTmp :: (CutConfig -> [FilePath] -> Action ()) -> String -> CutType
            -> (CutState -> CutExpr -> Rules FilePath)
rMapLastTmp actFn tmpPrefix t@(ListOf elemType) s@(scr,cfg) e@(CutFun _ _ _ exprs) = do
  exprPaths <- mapM (cExpr s) exprs
  let outPath    = hashedTmp' cfg t e []
  outPath %> \_ -> do
    lastPaths <- debugReadLines cfg $ last exprPaths
    let inits  = init exprPaths
        lasts  = map (cfgTmpDir cfg </>) lastPaths
        tmpDir = exprDir cfg </> tmpPrefix
        outs   = map (\p -> scriptTmpFile cfg (exprDir cfg) p (extOf elemType)) lastPaths
        outs'  = map (makeRelative $ cfgTmpDir cfg) outs
    (flip mapM)
      (zip outs lasts)
      (\(out, last) -> do
        need (last:inits)
        liftIO $ createDirectoryIfMissing True tmpDir
        actFn cfg (tmpDir:out:last:inits)
        trackWrite [out]
      )
    need outs
    writeFileLines outPath outs'
  return outPath
rMapLastTmp _ _ _ _ _ = error "bad argument to cMapLastTmp"
