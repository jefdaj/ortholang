-- This is named by analogy to R's Vectorize function, although I don't know if
-- the analogy is accurate. What this does is take a function and create one
-- that works the same, but on a list of the last argument type. For example
-- crb_blast compares a query to a target, and the vectorized version
-- crb_blast_all compares a query to a list of targets. No summary is performed
-- on the rsults; you just get back a list of the regular return type.

-- TODO awesome, it almost works! just need unique tmpdir per blast because of
--      silly naming issues?

module ShortCut.Modules.Vectorize where

import Debug.Trace

import ShortCut.Core.Types

import Development.Shake
import ShortCut.Core.Compile (cExpr, hashedTmp, scriptTmpFile)
import System.FilePath       (makeRelative)
import ShortCut.Modules.Repeat (extractExprs)
import Development.Shake.FilePath ((</>))

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

-- rMapLastTmps :: ([FilePath] -> Action ()) -> String -> CutType
--              -> (CutState -> CutExpr -> Rules FilePath)
-- rMapLastTmps actFn tmpPrefix rtnType s@(_,cfg) e@(CutFun _ _ _ exprs) = do
--   initPaths <- mapM (cExpr s) (init exprs)
--   lastsPath <- cExpr s (last exprs)
--   let outPath    = hashedTmp' cfg rtnType e []
--       tmpPrefix' = cfgTmpDir cfg </> "cache" </> tmpPrefix
--   outPath %> \_ -> do
--     lastPaths <- readFileLines lastsPath
--     let lasts = map (cfgTmpDir cfg </>) lastPaths
--         dirs  = map (\p -> scriptTmpDir tmpPrefix' [show e, show p]) lasts
--         outs  = map (\d -> d </> "out" <.> tExt rtnType) dirs
--         rels  = map (makeRelative $ cfgTmpDir cfg) outs
--     (flip mapM)
--       (zip3 lasts dirs outs)
--       (\(last, dir, out) -> do
--         need $ initPaths ++ [last]
--         liftIO $ createDirectoryIfMissing True dir
--         actFn $ [dir, out] ++ initPaths ++ [last]
--         trackWrite [out]
--       )
--     need outs
--     writeFileLines outPath rels
--   return outPath
-- rMapLastTmps _ _ _ _ _ = error "bad argument to rMapLastTmps"

rMapSimple :: ([FilePath] -> Action ()) -> (CutState -> CutExpr -> Rules FilePath)
rMapSimple actFn s@(scr,cfg) e@(CutFun _ _ _ exprs) = do
  initPaths <- mapM (cExpr s) (init exprs)
  lastsPath <- cExpr s (last exprs)
  let outPath    = hashedTmp cfg e []
      (ListOf t) = typeOf $ (traceShow (last exprs) last exprs)
  outPath %> \_ -> do
    lastPaths <- readFileLines lastsPath
    let lasts = map (cfgTmpDir cfg </>) lastPaths
        outs  = map (scriptTmpFile (cfgTmpDir cfg </> "cache") (extOf t)) lastPaths
        outs' = map (makeRelative $ cfgTmpDir cfg) outs
    (flip mapM)
      (zip outs lasts)
      (\(out, last) -> do
        need (last:initPaths)
        let tmp = (out:last:initPaths)
        actFn (traceShow tmp tmp)
        trackWrite [out]
      )
    need outs
    writeFileLines outPath outs'
  return outPath
rMapSimple _ _ _ = error "bad argument to cMapSimple"
