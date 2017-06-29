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
import ShortCut.Core.Compile      (cExpr, hashedTmp, scriptTmpFile, scriptTmpDir)
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

-- vectorize :: CutFunction -> String -> CutFunction
-- vectorize fn name = CutFunction
--   { fName      = name
--   , fFixity    = Prefix
--   , fTypeCheck = tVectorize $ fTypeCheck fn
--   , fCompiler  = cMapSimple $ fCompiler fn
--   }

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

-- TODO does this need a tmpDir?
-- TODO bug? argument could be a ref to a fn rather than the fn itself
--      (or a ref to a ref to a fn...) (am I thinking about this right?)
--      if so, probably a general problem with any fn that takes a list of args!
rMapLastTmp :: (CutConfig -> [FilePath] -> Action ()) -> String
           -> (CutState -> CutExpr -> Rules FilePath)
rMapLastTmp actFn tmpPrefix s@(scr,cfg) e@(CutFun _ _ _ exprs) = do
  -- initPaths <- mapM (cExpr s) (traceShow exprs $ init exprs)
  -- lastsPath <- cExpr s (last exprs)
  exprPaths <- mapM (cExpr s) exprs
  let outPath    = hashedTmp cfg e []
      (ListOf t) = typeOf $ last exprs -- TODO fails on a ref? not sure
  outPath %> \_ -> do
    lastPaths <- debugReadLines cfg $ last exprPaths
    let inits  = init exprPaths
        lasts  = map (cfgTmpDir cfg </>) lastPaths

        -- TODO this might work for the current one but not in general right?
        tmpDir = cfgTmpDir cfg </> "cache" </> tmpPrefix

        outs   = map (scriptTmpFile cfg (cfgTmpDir cfg </> "cache") (extOf t)) lastPaths
        outs' = map (makeRelative $ cfgTmpDir cfg) outs
    (flip mapM)
      (zip outs lasts)
      (\(out, last) -> do
        need (last:inits)
        liftIO $ createDirectoryIfMissing True tmpDir

        -- TODO soooo close now! is tmpDir just a little messed up?
        --      actually it looks pretty ok. maybe an earlier one is wrong?
        actFn cfg (tmpDir:out:last:inits)

        trackWrite [out]
      )
    need outs
    writeFileLines outPath outs'
  return outPath
rMapLastTmp _ _ _ _ = error "bad argument to cMapLastTmp"
