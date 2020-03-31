{-|
This is one of the most annoyingly-complicated-yet-useful parts of OrthoLang.
It lets you generate "mapped" variants of your normal functions. That is, if
you wrote something to do a computation on one input file and make one
output, this will turn it into something that does a computation on a list
of inputs and gives a list of outputs. It can only map over one argument (at
a time), but I haven't found any need for more than that yet. You decide
which argument using an integer index. See Blast.hs for some examples of how
it simplifies writing a large number of _each functions.

Mapping is implemented by:

1. adding a special @cache\/each\/\<hash\>@ dir

2. telling Shake that everything in that dir should be generated starting
   from a matching @.args@ file

3. telling Shake to generate a @.args@ file per list element after evaluating
   the mapped-over input list

4. telling Shake to need the output for each output file and gather them into
   a final output list

The special dir seems to be needed to separate the things that should be
made using @.args@ files, so Shake doesn't get confused trying to need @\<every
other output file\>.args@ too.

TODO can this be used to implement 'replace_each' in a way that allows fn calls?
-}

module OrthoLang.Core.Compile.Map
  ( rMap
  , rMapTmp
  , rMapTmps
  , rMapSimpleScript
  )
  where

import Development.Shake
import OrthoLang.Core.Compile.Basic
import OrthoLang.Core.Compile.Simple
import OrthoLang.Core.Types
import Text.PrettyPrint.HughesPJClass

import Data.List                  (intersperse)
import Data.List.Utils            (replace)
import Development.Shake.FilePath ((</>), (<.>), replaceBaseName)
import OrthoLang.Core.Actions      (readPaths, writePaths, symlink,
                                   readLit, writeLits, traceA, debugA, need')
import OrthoLang.Core.Paths        (cacheDir, toPath, fromPath, exprPath,
                                   Path, exprPathExplicit, argHashes)
import OrthoLang.Util         (digest, resolveSymlinks, unlessExists,
                                   popFrom, insertAt)
import OrthoLang.Locks       (withWriteOnce)
import System.Directory           (createDirectoryIfMissing)

-- TODO swap out plain debug fn for a proper one with module name

debugA' :: String -> String -> Action ()
debugA' name msg = debugA ("ortholang.core.compile.map." ++ name) msg

------------------------------------
-- simplified versions for export --
------------------------------------

-- for action functions that don't need a tmpdir
rMap :: Int -> (Config -> LocksRef -> IDsRef -> [Path] -> Action ()) -> RulesFn
rMap index actFn = rMapMain index Nothing actFn'
  where
    actFn' cfg ref ids _ args = actFn cfg ref ids args -- drops unused tmpdir

-- for action functions that need one tmpdir reused between calls
rMapTmp :: Int -> (Config -> LocksRef -> IDsRef -> Path -> [Path] -> Action ())
        -> String -> RulesFn
rMapTmp index actFn tmpPrefix s@(scr, cfg, _, _, _) = rMapMain index (Just tmpFn) actFn s
  where
    tmpDir = cacheDir cfg tmpPrefix
    tmpFn  = return . const tmpDir

-- for action functions that need a unique tmpdir each call
-- TODO use a hash for the cached path rather than the name, which changes!
rMapTmps :: Int
          -> (Config -> LocksRef -> IDsRef -> Path -> [Path] -> Action ())
          -> String -> RulesFn
rMapTmps index actFn tmpPrefix s@(scr, cfg, _, _, _) e = rMapMain index (Just tmpFn) actFn s e
  where
    tmpFn args = do
      let base = concat $ intersperse "/" $ map digest args
          dir  = fromPath cfg $ cacheDir cfg tmpPrefix
      return $ toPath cfg (dir </> base)

{- Like rSimpleScript, but the last argument should be a list.
 - It will be evaluated and one call made to aSimpleScript with each element.
 -}
rMapSimpleScript :: Int -> String -> RulesFn
rMapSimpleScript index = rMap index . aSimpleScript

--------------------
-- main algorithm --
--------------------

{- This separately hooks up aMapElem to operate on any .args files, and aMap to
 - generate some .args files then gather them into the overall list. Those two
 - then need to agree on tmpfiles, communicating only through the .args files.
 -
 - The main reason for such trickiness is that things need to be hooked up in
 - the Rules monad, before knowing the contents of the list that will be mapped
 - over. Given that I'm not sure there's any way to avoid intermediate files,
 - but am open to alternatives if anyone thinks of something!
 -}
rMapMain :: Int -> Maybe ([Path] -> IO Path)
         -> (Config -> LocksRef -> IDsRef -> Path -> [Path] -> Action ())
         -> RulesFn
rMapMain mapIndex mTmpFn actFn s@(scr, cfg, ref, ids, dMap) e@(Fun r salt _ name exprs) = do
  let mapIndex' = mapIndex - 1 -- index arguments from 1 rather than 0
      (mappedExpr, normalExprs) = popFrom mapIndex' exprs
  regularArgPaths <- mapM (rExpr s) normalExprs
  (ExprPath mappedArgsPath) <- rExpr s mappedExpr
  let singleName     = replace "_each" "" name -- TODO any less brittle ideas? could make this a fn
      mainOutPath    = fromPath cfg $ exprPath cfg scr e
      regularArgPaths'  = map (\(ExprPath p) -> toPath cfg p) regularArgPaths
      argLastsPath'  = toPath cfg mappedArgsPath
      elemCacheDir   = (fromPath cfg $ cacheDir cfg "each") </> hashFun cfg scr e
      elemCacheDir'  = toPath cfg elemCacheDir -- TODO redundant?
      elemCachePtn   = elemCacheDir </> "*" </> "result" -- <.> extOf eType
      eType = case r of
                (ListOf t) -> debug cfg "rMapMain" ("type of \"" ++ render (pPrint e)
                                  ++ "' (" ++ show e ++ ") is " ++ show t) t
                _ -> error $ "bad argument to rMapMain: " ++ show e
  elemCachePtn %> aMapElem cfg ref ids eType mTmpFn actFn singleName salt
  mainOutPath  %> aMapMain cfg ref ids mapIndex' regularArgPaths' elemCacheDir' eType argLastsPath'
  return $ debugRules cfg "rMapMain" e $ ExprPath mainOutPath
rMapMain _ _ _ _ _ = fail "bad argument to rMapMain"

hashFun :: Config -> Script -> Expr -> String
hashFun cfg scr e@(Fun _ s _ n _) = digest $ [n, show s] ++ argHashes cfg scr e
hashFun _ _ _ = error "hashFun only hashes function calls so far"

{- This calls aMapArgs to leave a .args file for each set of args, then gathers
 - up the corresponding outPaths and returns a list of them.
 -}
aMapMain :: Config -> LocksRef -> IDsRef -> Int
         -> [Path] -> Path -> Type -> Path -> FilePath
         -> Action ()
aMapMain cfg ref ids mapIndex regularArgs mapTmpDir eType mappedArg outPath = do
  need' cfg ref "ortholang.core.compile.map.aMapMain" regularArgs'
  let resolve = resolveSymlinks $ Just $ cfgTmpDir cfg
  regularArgs'' <- liftIO $ mapM resolve regularArgs'
  mappedPaths  <- readPaths cfg ref mappedArgList'
  mappedPaths' <- liftIO $ mapM resolve $ map (fromPath cfg) mappedPaths
  debugA' "aMapMain" $ "mappedPaths': " ++ show mappedPaths'
  mapM_ (aMapArgs cfg ref ids mapIndex eType regularArgs'' mapTmpDir')
        (map (toPath cfg) mappedPaths') -- TODO wrong if lits?
  let outPaths = map (eachPath cfg mapTmpDir' eType) mappedPaths'
  need' cfg ref "ortholang.core.compile.map.aMapMain" outPaths
  outPaths' <- liftIO $ mapM resolve outPaths
  let out = traceA "aMapMain" outPath
              (outPath:regularArgs' ++ [mapTmpDir', mappedArgList'])
  if eType `elem` [str, num]
    then mapM (readLit cfg ref) outPaths' >>= writeLits cfg ref out
    else writePaths cfg ref out $ map (toPath cfg) outPaths'
  where
    regularArgs'   = map (fromPath cfg) regularArgs
    mappedArgList' = fromPath cfg mappedArg
    mapTmpDir'     = fromPath cfg mapTmpDir

-- TODO take + return Paths?
-- TODO blast really might be nondeterministic here now that paths are hashed!
eachPath :: Config -> FilePath -> Type -> FilePath -> FilePath
eachPath cfg tmpDir eType path = tmpDir </> hash' </> "result" -- <.> extOf eType TODO /result?
  where
    path' = toPath cfg path
    hash  = digest path'
    hash' = debug cfg "eachPath" ("hash of " ++ show path' ++ " is " ++ hash) hash

-- This leaves arguments in .args files for aMapElem to find.
-- TODO This should be done for each replace operation in replace_each
-- TODO put mapIndex and mappedArg together, and rename that something with path
aMapArgs :: Config -> LocksRef -> IDsRef -> Int
         -> Type -> [FilePath] -> FilePath -> Path
         -> Action ()
aMapArgs cfg ref _ mapIndex eType regularArgs' tmp' mappedArg = do
  let mappedArg' = fromPath cfg mappedArg
      argsPath   = replaceBaseName (eachPath cfg tmp' eType mappedArg') "args"
      -- argPaths   = regularArgs' ++ [mappedArg'] -- TODO abs path bug here?
      argPaths   = insertAt mapIndex mappedArg' regularArgs'
      argPaths'  = map (toPath cfg) argPaths
  debugFn $ "mappedArg': " ++ show mappedArg'
  debugFn $ "argsPath: " ++ show argsPath
  debugFn $ "argPaths: " ++ show argPaths
  debugFn $ "argPaths': " ++ show argPaths'
  writePaths cfg ref argsPath argPaths'
  where
    debugFn = debugA' "aMapArgs"

{- This gathers together Rules-time and Action-time arguments and passes
 - everything to actFn. To save on duplicated computation it writes the same
 - outfile that would have come from the equivalent non-mapped (single)
 - function if that doesn't exist yet, then links to it from the real outPath.
 - Shake will be suprised because the single outPath wasn't declared in any
 - Rule beforehand, but it should be able to adjust and skip repeating it when
 - the time comes.
 -
 - TODO does it have a race condition for writing the single file?
 - TODO any way to make that last FilePath into a Path? does it even matter?
 - TODO can actFn here be looked up from the individal fn itsef passed in the definition?
 - TODO after singleFn works, can we remove tmpFn? (ok if not)
 -}
aMapElem :: Config -> LocksRef -> IDsRef -> Type
         -> Maybe ([Path] -> IO Path)
         -> (Config -> LocksRef -> IDsRef -> Path -> [Path] -> Action ())
         -> String -> Salt -> FilePath -> Action ()
aMapElem cfg ref ids eType tmpFn actFn singleName salt out = do
  let argsPath = replaceBaseName out "args"
  args <- readPaths cfg ref argsPath
  let args' = map (fromPath cfg) args
  args'' <- liftIO $ mapM (resolveSymlinks $ Just $ cfgTmpDir cfg) args' -- TODO remove?
  need' cfg ref "ortholang.core.compile.map.aMapElem" args'
  debugA "ortholang.core.compile.map.aMapElem" $ "out: " ++ show out
  dir <- liftIO $ case tmpFn of
    Nothing -> return $ cacheDir cfg "each" -- TODO any better option than this or undefined?
    Just fn -> do
      d <- fn args
      let d' = fromPath cfg d
      createDirectoryIfMissing True d'
      return d
  let out' = traceA "aMapElem" (toPath cfg out) args''
      -- TODO in order to match exprPath should this NOT follow symlinks?
      hashes  = map (digest . toPath cfg) args'' -- TODO make it match exprPath
      single  = exprPathExplicit cfg singleName eType salt hashes
      single' = fromPath cfg single
      args''' = single:map (toPath cfg) args''
  -- TODO any risk of single' being made after we test for it here?
  unlessExists single' $ actFn cfg ref ids dir args'''
  -- TODO is there a way to use withWriteOnce without an indefinite block??
  -- withWriteOnce ref single' $ actFn cfg ref ids dir args''' -- TODO is this the bug???
  -- withWriteOnce ref (single' <.> "test") $ do
  --   actFn cfg ref ids dir args''' -- TODO is this the bug???
  --   writeFile' (single' <.> "test") ""
  symlink cfg ref out' single
