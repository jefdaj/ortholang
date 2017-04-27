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
  , cLoad
  , hashedTmp
  , hashedTmp'
  , cExpr
  , cacheDir
  )
  where

import Development.Shake
import ShortCut.Core.Types

import Data.List              (find)
import Crypto.Hash                (hash, Digest, MD5)
import Data.ByteString.Char8      (pack)
import Data.Maybe                 (fromJust)
import Data.Scientific            (Scientific)
import Data.Set                   (Set, union, difference, intersection
                                  ,fromList, toList)
import Development.Shake.FilePath ((<.>), (</>))
import System.Directory           (canonicalizePath)
import System.FilePath            (makeRelative)
import Data.String.Utils          (strip)

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
hashedTmp' cfg (CutType extn _) expr paths = exprDir cfg </> uniq <.> extn
  where
    paths' = map (makeRelative $ cfgTmpDir cfg) paths
    uniq = digest $ unlines $ (show expr):paths'
hashedTmp' _ _ _ _ = error "bad arguments to hashedTmp'"

-- TODO what happens to plain sets?
-- TODO WAIT ARE SETS REALLY NEEDED? OR CAN WE JUST REFER TO FILETYPES?
cExpr :: CutConfig -> CutExpr -> Rules FilePath
cExpr c e@(CutLit  _ _    ) = cLit c e
cExpr c e@(CutRef  _ _    ) = cRef c e
cExpr c e@(CutList _ _    ) = cList c e
cExpr c e@(CutBop  _ n _ _) = compileByName c e n -- TODO turn into Fun?
cExpr c e@(CutFun  _ n _  ) = compileByName c e n

-- TODO remove once no longer needed (parser should find fns)
compileByName :: CutConfig -> CutExpr -> String -> Rules FilePath
compileByName cfg expr name = case findByName cfg name of
  Nothing -> error $ "no such function '" ++ name ++ "'"
  Just f  -> (fCompiler f) cfg expr

-- TODO remove once no longer needed (parser should find fns)
findByName :: CutConfig -> String -> Maybe CutFunction
findByName cfg name = find (\f -> fName f == name) fs
  where
    ms = cfgModules cfg
    fs = concat $ map mFunctions ms

cAssign :: CutConfig -> CutAssign -> Rules (CutVar, FilePath)
cAssign cfg (var, expr) = do
  path  <- cExpr cfg expr
  path' <- cVar cfg var expr path
  return (var, path')

-- TODO how to fail if the var doesn't exist??
--      (or, is that not possible for a typechecked AST?)
compileScript :: CutConfig -> CutScript -> Rules FilePath
compileScript cfg as = do
  -- liftIO $ putStrLn "entering compileScript"
  rpaths <- mapM (cAssign cfg) as
  return $ fromJust $ lookup (CutVar "result") rpaths

-- write a literal value from ShortCut source code to file
cLit :: CutConfig -> CutExpr -> Rules FilePath
cLit cfg expr = do
  -- liftIO $ putStrLn "entering cLit"
  let path = hashedTmp cfg expr []
  path %> \out -> do
    -- putQuiet $ unwords ["write", out]
    writeFileChanged out $ paths expr ++ "\n"
  return path
  where
    paths :: CutExpr -> String
    paths (CutLit _ s) = s
    paths _ = error "bad argument to paths"

-- TODO how to show the list once it's created? not just as a list of paths!
cList :: CutConfig -> CutExpr -> Rules FilePath
cList cfg e@(CutList EmptyList []) = do
  let link = hashedTmp cfg e []
  link %> \out -> quietly $ cmd "touch" [out]
  return link
cList cfg e@(CutList rtn exprs) = do
  paths <- mapM (cExpr cfg) exprs
  let path = hashedTmp cfg e paths
  path %> \out -> need paths >> writeFileLines out paths
  return path

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
cRef :: CutConfig -> CutExpr -> Rules FilePath
cRef cfg expr@(CutRef _ var) = do
  -- liftIO $ putStrLn "entering cRef"
  return $ namedTmp cfg var expr
cRef _ _ = error "bad argument to cRef"

-- creates a symlink from expression file to input file
-- these should be the only absolute ones,
-- and the only ones that point outside the temp dir
cLoad :: CutConfig -> CutExpr -> Rules FilePath
cLoad cfg e@(CutFun _ _ [f]) = do
  -- liftIO $ putStrLn "entering cLoad"
  path <- cExpr cfg f
  let link = hashedTmp cfg e [path]
  link %> \out -> do
    str'   <- fmap strip $ readFile' path
    path'' <- liftIO $ canonicalizePath str'
    -- putQuiet $ unwords ["link", str', out]
    quietly $ cmd "ln -fs" [path'', out]
  return link
cLoad _ _ = error "bad argument to cLoad"

-- Creates a symlink from varname to expression file.
-- TODO how should this handle file extensions? just not have them?
-- TODO or pick up the extension of the destination?
cVar :: CutConfig -> CutVar -> CutExpr -> FilePath -> Rules FilePath
cVar cfg var expr dest = do
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
cBop :: CutConfig -> CutType -> CutExpr -> (CutExpr, CutExpr)
      -> Rules (FilePath, FilePath, FilePath)
cBop cfg t expr (n1, n2) = do
  -- liftIO $ putStrLn "entering cBop"
  p1 <- cExpr cfg n1
  p2 <- cExpr cfg n2
  return (p1, p2, hashedTmp' cfg t expr [p1, p2])
