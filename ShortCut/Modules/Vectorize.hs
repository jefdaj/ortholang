-- This is named by analogy to R's Vectorize function, although I don't know if
-- the analogy is accurate. What this does is take a function and create one
-- that works the same, but on a list of the last argument type. For example
-- crb_blast compares a query to a target, and the vectorized version
-- crb_blast_all compares a query to a list of targets. No summary is performed
-- on the rsults; you just get back a list of the regular return type.

-- TODO awesome, it almost works! just need unique tmpdir per blast because of
--      silly naming issues?

module ShortCut.Modules.Vectorize where

-- import Debug.Trace

import ShortCut.Core.Types

import Development.Shake
import ShortCut.Core.Compile (cExpr, hashedTmp)
import System.FilePath       (makeRelative)
import ShortCut.Modules.Repeat (extractExprs)

-- TODO is there any point to empty ones? maybe just import from other files...
--      could have a separate set of API/dev modules or something
cutModule :: CutModule
cutModule = CutModule
  { mName = "vectorize"
  , mFunctions = []
  }

vectorize :: CutFunction -> String -> CutFunction
vectorize fn name = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = tVectorize $ fTypeCheck fn
  , fCompiler  = cVectorize $ fCompiler fn
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

-- TODO factor out common code between this and cRepeatEach
cVectorize :: (CutState -> CutExpr -> Rules FilePath)
           -> (CutState -> CutExpr -> Rules FilePath)
cVectorize cFn s@(scr,cfg) e@(CutFun t ds n args) = do
  let inits = init args
      lasts = extractExprs scr $ last args -- TODO what happens if this is a fn call?
  resPaths <- mapM (\l -> cFn s $ CutFun t ds n $ inits ++ [l]) lasts
  let outPath = hashedTmp cfg e resPaths
  outPath %> \out -> do
    need resPaths -- TODO if there's an error, try adding inits + lasts here too
    let outPaths' = map (makeRelative $ cfgTmpDir cfg) resPaths
    writeFileLines out outPaths'
  return outPath
cVectorize _ _ _ = error "bad argument to cVectorize"
