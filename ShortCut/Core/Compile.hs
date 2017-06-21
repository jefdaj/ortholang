-- TODO once the modules are done, will this mostly be gone?
--      all except the basic refs, symlinks, etc. i guess

-- Once text has been parsed into an abstract syntax tree (Parse.hs), this
-- module "compiles" it by translating it into a set of Shake build rules. To
-- actually run the rules, use `eval` in the Interpret module.

-- TODO add more descriptive runtime error for canonicalizePath failing b/c no file
-- TODO see if you can avoid making more than one absolute symlink per input file
-- TODO make systematically sure there's only one rule for each file
-- TODO pass tmpDir as a config option somehow, and verbosity

-- TODO why doesn't turning down the verbosity actually work?

module ShortCut.Core.Compile
  ( compileScript
  , cBop
  , hashedTmp
  , hashedTmp'
  , cExpr
  , cList
  , cacheDir
  , addPrefixes
  , digest
  , cPathList
  , cLitList
  )
  where

import Debug.Trace

import Development.Shake
import ShortCut.Core.Types

import Crypto.Hash                (hash, Digest, MD5)
import Data.ByteString.Char8      (pack)
import Data.List                  (find)
import Data.Maybe                 (fromJust)
import Development.Shake.FilePath ((<.>), (</>))
import System.FilePath            (makeRelative)


--------------------------------------------------------
-- prefix variable names so duplicates don't conflict --
--------------------------------------------------------

-- TODO only mangle the specific vars we want changed!

mangleExpr :: (CutVar -> CutVar) -> CutExpr -> CutExpr
mangleExpr _ e@(CutLit  _ _) = e
mangleExpr fn (CutRef  t vs v      ) = CutRef  t (map fn vs)   (fn v)
mangleExpr fn (CutBop  t vs n e1 e2) = CutBop  t (map fn vs) n (mangleExpr fn e1) (mangleExpr fn e2)
mangleExpr fn (CutFun  t vs n es   ) = CutFun  t (map fn vs) n (map (mangleExpr fn) es)
mangleExpr fn (CutList t vs   es   ) = CutList t (map fn vs)   (map (mangleExpr fn) es)
-- mangleExpr fn (CutSubs r v ss as) = CutSubs (mangleExpr fn r) (fn v) (mangleExpr fn ss) (mangleScript fn as)
-- CutSubs CutExpr CutExpr CutVar [CutAssign] -- dep, ind, ind', cxt

mangleAssign :: (CutVar -> CutVar) -> CutAssign -> CutAssign
mangleAssign fn (var, expr) = (fn var, mangleExpr fn expr)

mangleScript :: (CutVar -> CutVar) -> CutScript -> CutScript
mangleScript fn = map (mangleAssign fn)

-- TODO pad with zeros?
-- Add a "dupN." prefix to each variable name in the path from independent
-- -> dependent variable, using a list of those varnames
addPrefix :: String -> (CutVar -> CutVar)
addPrefix p (CutVar s) = CutVar $ s ++ "." ++ p

-- TODO should be able to just apply this to a duplicate script section right?
addPrefixes :: String -> CutScript -> CutScript
addPrefixes p = mangleScript (addPrefix p)


---------------------
-- determine paths --
---------------------

-- TODO move all this stuff to utils or a new config module or something...

-- TODO remove or put in Types
cacheDir :: CutConfig -> FilePath
cacheDir cfg = cfgTmpDir cfg </> "cache"

-- TODO what was this even for? remove it?
exprDir :: CutConfig -> FilePath
exprDir cfg = cacheDir cfg </> "shortcut"

-- Note that MD5 is no longer considered secure
-- But for our purposes (checking for updated files) it doesn't matter.
-- See https://en.wikipedia.org/wiki/MD5
digest :: (Show a) => a -> String
digest val = take 10 $ show (hash asBytes :: Digest MD5)
  where
    asBytes = (pack . show) val

-- TODO flip arguments for consistency with everything else There's a special
-- case for "result", which is like the "main" function of a ShortCut script,
-- and always goes to <tmpdir>/result.
namedTmp :: CutConfig -> CutVar -> CutExpr -> FilePath
namedTmp cfg (CutVar var) expr = cfgTmpDir cfg </> base
  where
    base  = if var == "result" then var else var <.> extOf (typeOf expr)

-- TODO extn can be found inside expr now; remove it
hashedTmp :: CutConfig -> CutExpr -> [FilePath] -> FilePath
hashedTmp cfg expr paths = exprDir cfg </> uniq <.> extOf (typeOf expr)
  where
    paths' = map (makeRelative $ cfgTmpDir cfg) paths
    uniq = digest $ unlines $ (show expr):paths'

-- overrides the expression's "natural" extension
-- TODO figure out how to remove!
hashedTmp' :: CutConfig -> CutType -> CutExpr -> [FilePath] -> FilePath
hashedTmp' cfg rtn expr paths = exprDir cfg </> uniq <.> extOf rtn
  where
    paths' = map (makeRelative $ cfgTmpDir cfg) paths
    uniq = digest $ unlines $ (show expr):paths'

scriptTmp :: Show a => FilePath -> String -> a -> FilePath
scriptTmp tmpDir ext uniq = tmpDir </> digest uniq <.> ext

------------------------------
-- compile the ShortCut AST --
------------------------------

-- TODO what happens to plain sets?
-- TODO WAIT ARE SETS REALLY NEEDED? OR CAN WE JUST REFER TO FILETYPES?
cExpr :: CutState -> CutExpr -> Rules FilePath
cExpr s e@(CutLit  _ _      ) = cLit s e
cExpr s e@(CutRef  _ _ _    ) = cRef s e
cExpr s e@(CutList _ _ _    ) = cList s e
cExpr s e@(CutBop  _ _ n _ _) = compileByName s e n -- TODO turn into Fun?
cExpr s e@(CutFun  _ _ n _  ) = compileByName s e n

-- TODO remove once no longer needed (parser should find fns)
compileByName :: CutState -> CutExpr -> String -> Rules FilePath
compileByName s@(_,cfg) expr name = case findByName cfg name of
  Nothing -> error $ "no such function '" ++ name ++ "'"
  Just f  -> (fCompiler f) s expr

-- TODO remove once no longer needed (parser should find fns)
findByName :: CutConfig -> String -> Maybe CutFunction
findByName cfg name = find (\f -> fName f == name) fs
  where
    ms = cfgModules cfg
    fs = concatMap mFunctions ms

cAssign :: CutState -> CutAssign -> Rules (CutVar, FilePath)
cAssign s (var, expr) = do
  path  <- cExpr s expr
  path' <- cVar s var expr path
  return (var, path')

-- TODO how to fail if the var doesn't exist??
--      (or, is that not possible for a typechecked AST?)
compileScript :: CutState -> Maybe String -> Rules FilePath
compileScript s@(as,_) p = do
  -- liftIO $ putStrLn "entering compileScript"
  rpaths <- mapM (cAssign s) as
  return $ fromJust $ lookup (CutVar res) rpaths
  where
    res = case p of
      Nothing -> "result"
      Just p' -> "result." ++ p'

-- write a literal value from ShortCut source code to file
cLit :: CutState -> CutExpr -> Rules FilePath
cLit (_,cfg) expr = do
  -- liftIO $ putStrLn "entering cLit"
  let path = hashedTmp cfg expr []
  path %> \out -> writeFileChanged out $ paths expr ++ "\n" -- TODO is writeFileChanged right?
  return path
  where
    paths :: CutExpr -> String
    paths (CutLit _ s) = s
    paths _ = error "bad argument to paths"

-- TODO how to show the list once it's created? not just as a list of paths!
-- TODO why are lists of lists not given .list.list ext? hides a more serious bug?
--      or possibly the bug is that we're making accidental lists of lists?
cList :: CutState -> CutExpr -> Rules FilePath
cList (_,cfg) e@(CutList EmptyList _ _) = do
  let link = hashedTmp cfg e []
  link %> \out -> quietly $ cmd "touch" [out]
  return link
cList s@(_,cfg) e@(CutList _ _ exprs) = do
  paths <- mapM (cExpr s) exprs
  let path   = hashedTmp cfg e paths
      paths' = map (makeRelative $ cfgTmpDir cfg) paths
  path %> \out -> need paths >> writeFileChanged out (unlines paths')
  return path
cList _ _ = error "bad arguemnts to cList"

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
cRef :: CutState -> CutExpr -> Rules FilePath
cRef (_,cfg) expr@(CutRef _ _ var) = return $ namedTmp cfg var expr
cRef _ _ = error "bad argument to cRef"

-- Creates a symlink from varname to expression file.
-- TODO how should this handle file extensions? just not have them?
-- TODO or pick up the extension of the destination?
cVar :: CutState -> CutVar -> CutExpr -> FilePath -> Rules FilePath
cVar (_,cfg) var expr dest = do
  -- liftIO $ putStrLn "entering cVar"
  let link  = namedTmp cfg var expr
      dest' = makeRelative (cfgTmpDir cfg) dest
  link %> \out -> do
    alwaysRerun
    need [dest]
    -- putQuiet $ unwords ["link", (cfgTmpDir cfg) </> dest', out]
    quietly $ cmd "ln -fs" [dest', out]
  return link

-- handles the actual rule generation for all binary operators
-- basically the `paths` functions with pattern matching factored out
cBop :: CutState -> CutType -> CutExpr -> (CutExpr, CutExpr)
      -> Rules (FilePath, FilePath, FilePath)
cBop s@(_,cfg) t expr (n1, n2) = do
  -- liftIO $ putStrLn "entering cBop"
  p1 <- cExpr s n1
  p2 <- cExpr s n2
  return (p1, p2, hashedTmp' cfg t expr [p1, p2])

-- this is needed when calling a script that writes a list of literals,
-- because shortcut expects a list of hashed filenames *pointing* to literals
-- TODO this needs to announce that it makes those literal files, doesn't it?
-- cPathList :: CutConfig -> CutType -> FilePath -> FilePath -> Action ()
-- cPathList cfg litType inPath outPath = do
--   lits <- fmap lines $ readFile' (traceShow inPath inPath)
--   let litExprs  = map (\l -> CutLit litType l)       (traceShow lits lits)
--       litPaths  = map (\e -> hashedTmp cfg e [])     (traceShow litExprs litExprs)
--       litPaths' = map (makeRelative $ cfgTmpDir cfg) (traceShow litPaths litPaths)
--   -- TODO actually writing the files doesn't seem to be enough for shake;
--   -- have to also announce that they were written somehow:
--   liftIO $ mapM_ (\(l, p) -> writeFile (cfgTmpDir cfg </> p) (l ++ "\n")) (zip lits litPaths')
--   trackWrite litPaths
--   need [inPath]
--   writeFileLines outPath litPaths'
--   return ()

-- TODO fix "XXX.str.list: openFile: does not exist" (need it first, making this Rules?)
cPathList :: CutState -> CutType -> FilePath -> Rules FilePath
cPathList state litType listPath = do
  litPaths <- fmap lines $ liftIO $ readFile listPath -- TODO should this be in an action?
  let litExprs = map (CutLit litType) litPaths
      listExpr = CutList (ListOf litType) [] litExprs
  cExpr state listExpr

-- reverse of cPathList
-- for passing a shortcut list in a format scripts will understand
-- cLitList :: CutConfig -> FilePath -> FilePath -> Action ()
-- cLitList cfg inPath outPath = do
--   litPaths <- fmap lines $ readFile' inPath
--   -- TODO are there extra newlines here? TODO readFile'?
--   litLines <- mapM (liftIO . readFile . (cfgTmpDir cfg </>)) litPaths
--   writeFileLines outPath litLines

cLitList :: FilePath -> FilePath -> Rules FilePath
cLitList tmpDir inPath = do
  let outPath = scriptTmp tmpDir "txt" inPath
  outPath %> \out -> do
    litPaths <- readFileLines inPath
    need litPaths
    lits <- mapM readFile' litPaths
    writeFileLines out lits
  return outPath
