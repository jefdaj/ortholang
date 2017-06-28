-- This is named by analogy to R's Vectorize function, although I don't know if
-- the analogy is accurate. What this does is take a function and create one
-- that works the same, but on a list of the last argument type. For example
-- crb_blast compares a query to a target, and the vectorized version
-- crb_blast_all compares a query to a list of targets. No summary is performed
-- on the rsults; you just get back a list of the regular return type.

-- TODO remove this module and put what's left in Core somewhere

module ShortCut.Modules.Vectorize where

import Debug.Trace

import Development.Shake
import ShortCut.Core.Types

import Development.Shake.FilePath ((</>))
import ShortCut.Core.Compile      (cExpr, hashedTmp, scriptTmpFile)
import ShortCut.Modules.Repeat    (extractExprs)
import System.FilePath            (makeRelative)

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

-- TODO fix bug: argument could be a ref to a fn rather than the fn itself
--      (or a ref to a ref to a fn...)
-- TODO this is probably a general problem with any fn that takes a list of args!
rMapSimple :: (CutConfig -> [FilePath] -> Action ())
           -> (CutState -> CutExpr -> Rules FilePath)
rMapSimple actFn s@(scr,cfg) e@(CutFun _ _ _ exprs) = do
  -- initPaths <- mapM (cExpr s) (traceShow exprs $ init exprs)
  -- lastsPath <- cExpr s (last exprs)
  exprPaths <- mapM (cExpr s) (traceShow exprs exprs)
  let outPath    = hashedTmp cfg e []
      (ListOf t) = typeOf $ last exprs -- TODO fails on a ref? not sure
  outPath %> \_ -> do
    lastPaths <- readFileLines $ last exprPaths
    let inits = init exprPaths
        lasts = map (cfgTmpDir cfg </>) lastPaths
        outs  = map (scriptTmpFile (cfgTmpDir cfg </> "cache") (extOf t)) lastPaths
        outs' = map (makeRelative $ cfgTmpDir cfg) outs
    (flip mapM)
      (zip outs lasts)
      (\(out, last) -> do
        need (last:inits)
        let tmp = (out:last:inits)
        actFn cfg tmp
        trackWrite [out]
      )
    need outs
    writeFileLines outPath outs'
  return outPath
rMapSimple _ _ _ = error "bad argument to cMapSimple"
