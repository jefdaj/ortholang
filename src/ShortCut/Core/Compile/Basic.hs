-- Once text has been parsed into an abstract syntax tree (Parse.hs), this
-- module "compiles" it by translating it into a set of Shake build rules. To
-- actually run the rules, use `eval` in the Interpret module.

-- TODO add more descriptive runtime error for canonicalizePath failing b/c no file
-- TODO see if you can avoid making more than one absolute symlink per input file
-- TODO make systematically sure there's only one rule for each file
-- TODO pass tmpDir as a config option somehow, and verbosity

-- TODO why doesn't turning down the verbosity actually work?

module ShortCut.Core.Compile.Basic
--   ( compileScript
--   , rBop
--   , rExpr
--   , rList
--   , addPrefixes
--   )
  where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Pretty

import ShortCut.Core.Paths (cacheDir, exprPath, exprPathExplicit, toCutPath,
                            fromCutPath, varPath, CutPath)

import Control.Monad              (when)
import Data.List                  (intersperse)
import Development.Shake.FilePath ((</>), (<.>))
-- import ShortCut.Core.Debug        (debugA, debugRules, debug)
import ShortCut.Core.Locks        (withWriteLock')
import ShortCut.Core.Actions      (wrappedCmdWrite, debugA, debugL, debugNeed,
                                   readLit, readLits, writeLit, writeLits, hashContent,
                                   readLitPaths, hashContent, writePaths, symlink, readStrings)
import ShortCut.Core.Util         (absolutize, resolveSymlinks, stripWhiteSpace,
                                   digest, removeIfExists)
import System.FilePath            (takeExtension)
import Debug.Trace       (trace)


debug :: CutConfig -> String -> a -> a
debug cfg msg rtn = if cfgDebug cfg then trace msg rtn else rtn

-- TODO restrict to CutExpr?
-- TODO put in rExpr to catch everything at once? but misses which fn was called
debugRules :: (Pretty a, Show b) => CutConfig -> String -> a -> b -> b
debugRules cfg name input out = debug cfg msg out
  where
    ren = render $ pPrint input
    msg = name ++ " compiled '" ++ ren ++ "' to " ++ show out

------------------------------
-- compile the ShortCut AST --
------------------------------

rExpr :: CutState -> CutExpr -> Rules ExprPath
rExpr s e@(CutLit _ _ _      ) = rLit s e
rExpr s e@(CutRef _ _ _ _    ) = rRef s e
rExpr s e@(CutList _ _ _ _   ) = rList s e
rExpr s e@(CutBop _ _ _ n _ _) = rulesByName s e n -- TODO turn into Fun?
rExpr s e@(CutFun _ _ _ n _  ) = rulesByName s e n

-- TODO remove once no longer needed (parser should find fns)
rulesByName :: CutState -> CutExpr -> String -> Rules ExprPath
rulesByName s@(_,cfg,_) expr name = case findFunction cfg name of
  Nothing -> error $ "no such function '" ++ name ++ "'"
  Just f  -> (fRules f) s expr

rAssign :: CutState -> CutAssign -> Rules (CutVar, VarPath)
rAssign s@(_,cfg,_) (var, expr) = do
  (ExprPath path) <- rExpr s expr
  path' <- rVar s var expr $ toCutPath cfg path
  let res  = (var, path')
      res' = debugRules cfg "rAssign" (var, expr) res
  return res'

-- TODO how to fail if the var doesn't exist??
--      (or, is that not possible for a typechecked AST?)
compileScript :: CutState -> Maybe String -> Rules ResPath
compileScript s@(as,_,_) permHash = do
  -- TODO this can't be done all in parallel because they depend on each other,
  --      but can parts of it be parallelized? or maybe it doesn't matter because
  --      evaluating the code itself is always faster than the system commands
  rpaths <- mapM (rAssign s) as
  case lookup (CutVar res) rpaths of
    Nothing -> fail "no result variable. that's not right!"
    Just (VarPath r) -> return $ ResPath r
  where
    -- p here is "result" + the permutation name/hash if there is one right?
    res = case permHash of
      Nothing -> "result"
      Just h  -> "result." ++ h

-- write a literal value from ShortCut source code to file
rLit :: CutState -> CutExpr -> Rules ExprPath
rLit s@(_,cfg,ref) expr = do
  let path  = exprPath s expr -- absolute paths allowed!
      path' = debugRules cfg "rLit" expr $ fromCutPath cfg path
  path' %> \_ -> aLit cfg ref expr path
  return (ExprPath path')

-- TODO take the path, not the expression?
aLit :: CutConfig -> Locks -> CutExpr -> CutPath -> Action ()
aLit cfg ref expr out = writeLit cfg ref out'' ePath -- TODO too much dedup?
  where
    paths :: CutExpr -> FilePath
    paths (CutLit _ _ p) = p
    paths _ = error "bad argument to paths"
    ePath = paths expr
    out'  = fromCutPath cfg out
    out'' = debugA cfg "aLit" out' [ePath, out']

rList :: CutState -> CutExpr -> Rules ExprPath
-- TODO is this the bug? refers to a list of other empty lists, no?
-- rList s e@(CutList Empty _ _ _) = rListLits s e -- TODO remove? rListPaths?
rList s e@(CutList rtn _ _ _)
  | rtn `elem` [Empty, str, num] = rListLits  s e -- TODO does Empty fit here?
  | otherwise                    = rListPaths s e
rList _ _ = error "bad arguemnt to rList"

-- special case for empty lists
-- TODO is a special type for this really needed?
-- rListEmpty :: CutState -> CutExpr -> Rules ExprPath
-- rListEmpty s@(_,cfg,ref) e@(CutList Empty _ _ _) = do
--   let link  = exprPath s e
--       link' = debugRules cfg "rListEmpty" e $ fromCutPath cfg link
--   link' %> \_ -> aListEmpty cfg ref link
--   return (ExprPath link')
-- rListEmpty _ e = error $ "bad arguemnt to rListEmpty: " ++ show e

-- TODO is this actually needed? seems the same as lits or paths really
--      (also, is there a need to write empty lists at all?)
-- aListEmpty :: CutConfig -> Locks -> CutPath -> Action ()
-- aListEmpty cfg ref link = writeLits cfg ref link'' [] -- TODO error here?
--   where
--     link'  = fromCutPath cfg link
--     link'' = debugAction cfg "aListEmpty" link' [link']

-- special case for writing lists of strings or numbers as a single file
rListLits :: CutState -> CutExpr -> Rules ExprPath
rListLits s@(_,cfg,ref) e@(CutList _ _ _ exprs) = do
  litPaths <- mapM (rExpr s) exprs
  let litPaths' = map (\(ExprPath p) -> toCutPath cfg p) litPaths
  outPath' %> \_ -> aListLits cfg ref litPaths' outPath
  return (ExprPath outPath')
  where
    outPath  = exprPath s e
    outPath' = debugRules cfg "rListLits" e $ fromCutPath cfg outPath
rListLits _ e = error $ "bad argument to rListLits: " ++ show e

-- TODO put this in a cache dir by content hash and link there
aListLits :: CutConfig -> Locks -> [CutPath] -> CutPath -> Action ()
aListLits cfg ref paths outPath = do
  -- need paths'
  lits <- mapM (readLit cfg ref) paths'
  let lits' = map stripWhiteSpace lits -- TODO insert <<emptylist>> here?
  debugL cfg $ "aListLits lits': " ++ show lits'
  writeLits cfg ref out'' lits'
  where
    out'   = fromCutPath cfg outPath
    out''  = debugA cfg "aListLits" out' (out':paths')
    paths' = map (fromCutPath cfg) paths

-- regular case for writing a list of links to some other file type
rListPaths :: CutState -> CutExpr -> Rules ExprPath
rListPaths s@(_,cfg,ref) e@(CutList rtn salt _ exprs) = do
  paths <- mapM (rExpr s) exprs
  let paths'   = map (\(ExprPath p) -> toCutPath cfg p) paths
      hash     = digest $ concat $ map digest paths'
      outPath  = exprPathExplicit cfg "list" (ListOf rtn) salt [hash]
      outPath' = debugRules cfg "rListPaths" e $ fromCutPath cfg outPath
  outPath' %> \_ -> aListPaths cfg ref paths' outPath
  return (ExprPath outPath')
rListPaths _ _ = error "bad arguemnts to rListPaths"

-- works on everything but lits: paths or empty lists
aListPaths :: CutConfig -> Locks -> [CutPath] -> CutPath -> Action ()
aListPaths cfg ref paths outPath = do
  debugNeed cfg "aListPaths" paths'
  paths'' <- liftIO $ mapM (resolveSymlinks $ Just $ cfgTmpDir cfg) paths'
  debugNeed cfg "aListPaths" paths''
  let paths''' = map (toCutPath cfg) paths'' -- TODO not working?
  writePaths cfg ref out'' paths'''
  where
    out'   = fromCutPath cfg outPath
    out''  = debugA cfg "aListPaths" out' (out':paths')
    paths' = map (fromCutPath cfg) paths -- TODO remove this

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
rRef :: CutState -> CutExpr -> Rules ExprPath
rRef (_,cfg,_) e@(CutRef _ _ _ var) = return $ ePath $ varPath cfg var e
  where
    ePath p = ExprPath $ debugRules cfg "rRef" e $ fromCutPath cfg p
rRef _ _ = error "bad argument to rRef"

-- Creates a symlink from varname to expression file.
-- TODO unify with rLink2, rLoad etc?
-- TODO do we need both the CutExpr and ExprPath? seems like CutExpr would do
rVar :: CutState -> CutVar -> CutExpr -> CutPath -> Rules VarPath
rVar (_,cfg,ref) var expr oPath = do
  vPath' %> \_ -> aVar cfg ref vPath oPath
  return (VarPath vPath')
  where
    vPath  = varPath cfg var expr
    vPath' = debugRules cfg "rVar" var $ fromCutPath cfg vPath

aVar :: CutConfig -> Locks -> CutPath -> CutPath -> Action ()
aVar cfg ref vPath oPath = do
  alwaysRerun
  debugNeed cfg "aVar" [oPath']
  withWriteLock' ref vPath' $ liftIO $ removeIfExists vPath'
  symlink cfg ref vPath'' oPath
  where
    oPath'  = fromCutPath cfg oPath
    vPath'  = fromCutPath cfg vPath
    vPath'' = debugA cfg "aVar" vPath [vPath', oPath']

-- Handles the actual rule generation for all binary operators.
-- TODO can it be factored out somehow? seems almost trivial now...
rBop :: CutState -> CutExpr -> (CutExpr, CutExpr)
      -> Rules (ExprPath, ExprPath, ExprPath)
rBop s@(_,cfg,_) e@(CutBop _ _ _ _ _ _) (n1, n2) = do
  (ExprPath p1) <- rExpr s n1
  (ExprPath p2) <- rExpr s n2
  let path  = fromCutPath cfg $ exprPath s e
      path' = debugRules cfg "rBop" e path
  return (ExprPath p1, ExprPath p2, ExprPath path')
rBop _ _ _ = error "bad argument to rBop"

------------------------------
-- [t]ypechecking functions --
------------------------------

typeError :: [CutType] -> [CutType] -> String
typeError expected actual =
  "Type error:\nexpected " ++ show expected
           ++ "\nbut got " ++ show actual

-- TODO this should fail for type errors like multiplying a list by a num!
defaultTypeCheck :: [CutType] -> CutType
                 -> [CutType] -> Either String CutType
defaultTypeCheck expected returned actual =
  if actual `typesMatch` expected
    then Right returned
    else Left $ typeError expected actual

--------------------------
-- links to input files --
--------------------------

{- Takes a string with the filepath to load. Creates a trivial expression file
 - that's just a symlink to the given path. These should be the only absolute
 - links, and the only ones that point outside the temp dir.
 - TODO still true?
 -}
mkLoad :: String -> CutType -> CutFunction
mkLoad name rtn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str] rtn
  , fTypeDesc  = mkTypeDesc name [str] rtn
  , fFixity    = Prefix
  , fRules     = rLoad
  }

{- Like cLoad, except it operates on a list of strings. Note that you can also
 - load lists using cLoad, but it's not recommended because then you have to
 - write the list in a file, whereas this can handle literal lists in the
 - source code.
 -}
mkLoadList :: String -> CutType -> CutFunction
mkLoadList name rtn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [(ListOf str)] (ListOf rtn)
  , fTypeDesc  = mkTypeDesc name [(ListOf str)] (ListOf rtn)
  , fFixity    = Prefix
  , fRules     = rLoadList
  }

-- The paths here are a little confusing: expr is a str of the path we want to
-- link to. So after compiling it we get a path to *that str*, and have to read
-- the file to access it. Then we want to `ln` to the file it points to.
rLoad :: CutState -> CutExpr -> Rules ExprPath
rLoad s@(_,cfg,ref) e@(CutFun _ _ _ _ [p]) = do
  (ExprPath strPath) <- rExpr s p
  out' %> \_ -> aLoad cfg ref (toCutPath cfg strPath) out
  return (ExprPath out')
  where
    out  = exprPath s e
    out' = fromCutPath cfg out
rLoad _ _ = error "bad argument to rLoad"

aLoadHash :: CutConfig -> Locks -> CutPath -> String -> Action CutPath
aLoadHash cfg ref src ext = do
  debugNeed cfg "aLoadHash" [src']
  md5 <- hashContent cfg ref src -- TODO permission error here?
  let tmpDir'   = fromCutPath cfg $ cacheDir cfg "load"
      hashPath' = tmpDir' </> md5 <.> ext
      hashPath  = toCutPath cfg hashPath'
  symlink cfg ref hashPath src
  return hashPath
  where
    src' = fromCutPath cfg src

aLoad :: CutConfig -> Locks -> CutPath -> CutPath -> Action ()
aLoad cfg ref strPath outPath = do
  debugNeed cfg "aLoad" [strPath']
  pth <- readLitPaths cfg ref strPath'
  src' <- liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) $ fromCutPath cfg $ head pth -- TODO safer!
  -- debugL cfg $ "aLoad src': '" ++ src' ++ "'"
  -- debugL cfg $ "aLoad outPath': '" ++ outPath' ++ "'"
  hashPath <- aLoadHash cfg ref (toCutPath cfg src') (takeExtension outPath')
  -- let hashPath'    = fromCutPath cfg hashPath
      -- hashPathRel' = ".." </> ".." </> makeRelative (cfgTmpDir cfg) hashPath'
  symlink cfg ref outPath'' hashPath
  -- debugTrackWrite cfg [outPath'] -- TODO WTF? why does this not get called by symlink?
  where
    strPath'  = fromCutPath cfg strPath
    outPath'  = fromCutPath cfg outPath
    outPath'' = debugA cfg "aLoad" outPath [strPath', outPath']

rLoadList :: RulesFn
rLoadList s e@(CutFun (ListOf r) _ _ _ [es])
  | r `elem` [str, num] = rLoadListLits s es
  | otherwise = rLoadListLinks s e
rLoadList _ _ = error "bad arguments to rLoadList"

-- special case for lists of str and num
-- TODO remove rtn and use (typeOf expr)?
-- TODO is this different from rListOne, except in its return type?
-- TODO is it different from rLink? seems like it's just a copy/link operation...
rLoadListLits :: RulesFn
rLoadListLits s@(_,cfg,ref) expr = do
  (ExprPath litsPath') <- rExpr s expr
  let litsPath = toCutPath cfg litsPath'
  outPath' %> \_ -> aLoadListLits cfg ref outPath litsPath
  return (ExprPath outPath')
  where
    outPath  = exprPath s expr
    outPath' = fromCutPath cfg outPath

aLoadListLits :: CutConfig -> Locks -> CutPath -> CutPath -> Action ()
aLoadListLits cfg ref outPath litsPath = do
  let litsPath' = fromCutPath cfg litsPath
      out       = debugA cfg "aLoadListLits" outPath' [outPath', litsPath']
  lits  <- readLits cfg ref litsPath'
  lits' <- liftIO $ mapM absolutize lits
  writeLits cfg ref out lits'
  where
    outPath' = fromCutPath cfg outPath

-- regular case for lists of any other file type
rLoadListLinks :: RulesFn
rLoadListLinks s@(_,cfg,ref) (CutFun rtn salt _ _ [es]) = do
  (ExprPath pathsPath) <- rExpr s es
  let hash     = digest $ toCutPath cfg pathsPath
      outPath  = exprPathExplicit cfg "list" rtn salt [hash]
      outPath' = fromCutPath cfg outPath
  outPath' %> \_ -> aLoadListLinks cfg ref (toCutPath cfg pathsPath) outPath
  return (ExprPath outPath')
rLoadListLinks _ _ = error "bad arguments to rLoadListLinks"

aLoadListLinks :: CutConfig -> Locks -> CutPath -> CutPath -> Action ()
aLoadListLinks cfg ref pathsPath outPath = do
  -- Careful! The user will write paths relative to workdir and those come
  -- through as a (ListOf str) here; have to read as Strings and convert to
  -- CutPaths
  paths <- readLitPaths cfg ref pathsPath'
  let paths' = map (fromCutPath cfg) paths
  paths'' <- liftIO $ mapM (resolveSymlinks $ Just $ cfgTmpDir cfg) paths'
  let paths''' = map (toCutPath cfg) paths''
  hashPaths <- mapM (\p -> aLoadHash cfg ref p
                         $ takeExtension $ fromCutPath cfg p) paths'''
  let hashPaths' = map (fromCutPath cfg) hashPaths
  -- debugL cfg $ "about to need: " ++ show paths''
  debugNeed cfg "aLoadListLinks" hashPaths'
  writePaths cfg ref out hashPaths
  where
    outPath'   = fromCutPath cfg outPath
    pathsPath' = fromCutPath cfg pathsPath
    out = debugA cfg "aLoadListLinks" outPath' [outPath', pathsPath']

-- based on https://stackoverflow.com/a/18627837
-- uniqLines :: Ord a => [a] -> [a]
-- uniqLines = unlines . toList . fromList . lines

------------
-- scores --
------------

{- Scores are lists of pairs of num and some other type "zipped" together.
 - They're are a little odd: not quite a core type because you only create and
 - use them with functions (no source code literal), but not quite modular
 - because they require some minor changes in Core to work.
 -
 - I couldn't figure out what a generic compiler should look like, so for now
 - they only have the corresponding Action; other modules do the prep work.
 -}
aScores :: CutConfig -> Locks -> CutPath -> CutPath -> CutType -> CutPath -> Action ()
aScores cfg ref scoresPath othersPath othersType outPath = do
  scorePaths <- readLits               cfg ref $ fromCutPath cfg scoresPath
  otherPaths <- readStrings othersType cfg ref $ fromCutPath cfg othersPath
  let out'        = fromCutPath cfg outPath
      out''       = debugA cfg "aScores" out' (out':scorePaths)
  scores <- mapM (readLit cfg ref) scorePaths -- also others BTW
  others <- mapM (readLit cfg ref) otherPaths
  let scores' = map stripWhiteSpace scores -- TODO insert <<emptylist>> here?
      others' = map stripWhiteSpace scores -- TODO insert <<emptylist>> here?
      rows    = map (\(a,b) -> unwords [a,b]) $ zip scores' others'
  when (length scores /= length others) $ error $ unlines
     ["mismatched scores and others in aScores:", show scores', show others']
  debugL cfg $ "aScores scores': " ++ show scores'
  debugL cfg $ "aScores others': " ++ show others'
  debugL cfg $ "aScores rows: "    ++ show rows
  writeLits cfg ref out'' rows

-- takes an action fn with any number of args and calls it with a tmpdir.
-- TODO rename something that goes with the map fns?
rSimple :: (CutConfig -> Locks -> [CutPath] -> Action ()) -> RulesFn
rSimple actFn = rSimple' Nothing actFn'
  where
    actFn' cfg ref _ args = actFn cfg ref args -- drop unused tmpdir

rSimpleTmp :: String
           -> (CutConfig -> Locks -> CutPath -> [CutPath] -> Action ())
           -> RulesFn
rSimpleTmp prefix = rSimple' (Just prefix)

{- For scripts that just need some args passed to them. The first will be the
 - outPath, and the rest actual args. The string is the script name.
 -}
rSimpleScript :: String -> RulesFn
rSimpleScript = rSimple . aSimpleScript

aSimpleScript :: String -> (CutConfig -> Locks -> [CutPath] -> Action ())
aSimpleScript script cfg ref (out:ins) = aSimple' cfg ref out actFn Nothing ins
  where
    -- TODO is tmpDir used here at all? should it be?
    -- TODO match []?
    actFn c r t (o:is) = let o'  = fromCutPath c o
                             t'  = fromCutPath c t
                             is' = map (fromCutPath c) is
                         in wrappedCmdWrite c r o' is' [] [Cwd t'] script (o':is')
--     actFn c t (o:as) = let o' = (let r = fromCutPath c o
--                                  in debug c ("actFn o': '" ++ r ++ "'") r)
--                            ins' = map (fromCutPath c) as
--                        in (let s' = debug c ("actFn script: '" ++ script ++ "'") script
-- --                            wrappedCmdWrite c lockPath inPaths outPaths opts bin args = do -- TODO why the "failed to build" errors?
--                            in wrappedCmdWrite cfg o' ins' [o'] [] s' (o':ins'))
aSimpleScript _ _ _ as = error $ "bad argument to aSimpleScript: " ++ show as

-- TODO rSimpleScriptTmp?

rSimple' :: Maybe String
         -> (CutConfig -> Locks -> CutPath -> [CutPath] -> Action ())
         -> RulesFn
rSimple' mTmpPrefix actFn s@(_,cfg,ref) e@(CutFun _ _ _ _ exprs) = do
  argPaths <- mapM (rExpr s) exprs
  let argPaths' = map (\(ExprPath p) -> toCutPath cfg p) argPaths
  outPath' %> \_ -> aSimple' cfg ref outPath actFn mTmpDir argPaths'
  return (ExprPath outPath')
  where
    mTmpDir  = fmap (cacheDir cfg) mTmpPrefix -- TODO tables bug here?
    outPath  = exprPath s e
    outPath' = fromCutPath cfg outPath
rSimple' _ _ _ _ = error "bad argument to rSimple'"

-- TODO aSimpleScript that calls aSimple' with a wrappedCmd as the actFn
-- TODO rSimpleScript that calls rSimple + that

-- TODO need to handle empty lists here?
aSimple' ::  CutConfig -> Locks -> CutPath
         -> (CutConfig -> Locks -> CutPath -> [CutPath] -> Action ())
         -> Maybe CutPath -> [CutPath] -> Action ()
aSimple' cfg ref outPath actFn mTmpDir argPaths = do
  debugNeed cfg "aSimple'" argPaths'
  argPaths'' <- liftIO $ mapM (fmap (toCutPath cfg) . resolveSymlinks (Just $ cfgTmpDir cfg)) argPaths'
  let o' = debug cfg ("aSimple' outPath': " ++ outPath' ++ "'") outPath
      as = debug cfg ("aSimple' argsPaths'': " ++ show argPaths'') argPaths''
  actFn cfg ref tmpDir (o':as)
  trackWrite [out] -- TODO remove?
  where
    -- TODO probably not "simple tmp" anymore... remove? rename?
    hashes     = concat $ intersperse "_" $ map digest argPaths'
    argPaths'  = map (fromCutPath cfg) argPaths
    outPath'   = fromCutPath cfg outPath
    out = debugA cfg "aSimple'" outPath' (outPath':tmpDir':argPaths')
    (tmpDir, tmpDir') = case mTmpDir of
                Nothing  -> (toCutPath cfg $ cfgTmpDir cfg, cfgTmpDir cfg)
                Just dir -> (toCutPath cfg d, d)
                  where
                    d = fromCutPath cfg dir </> hashes
