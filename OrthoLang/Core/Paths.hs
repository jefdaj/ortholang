{-# LANGUAGE OverloadedStrings #-}

-- TODO rename this module to TmpFiles?

{- OrthoLang makes heavy use of tmpfiles, and this module controls where they go
 - inside the main tmpdir. After a rewrite, the overall layout should be:
 -
 - TMPDIR
 - |-- cache: a tmpdir per module for index files or whatever
 - |   |-- biomartr
 - |   |-- blast
 - |   |-- crb-blast
 - |   |-- seqio
 - |   `-- ...
 - |-- exprs: the hashed result of every expression, organized by fn
 - |   |-- all
 - |   |-- any
 - |   |-- concat_fastas
 - |   |-- crb_blast
 - |   |-- crb_blast_each
 - |   |-- extract_ids
 - |   |-- extract_seqs
 - |   |-- gbk_to_faa
 - |   |-- gbk_to_fna
 - |   |-- leave_each_out
 - |   |-- repeat
 - |   |-- replace_each
 - |   |-- translate
 - |   `-- ...
 - `-- vars: symlinks from user variable names to hashed expressions
 -    |-- green_hits.str.list
 -    |-- greens.faa.list
 -    |-- plantcut.str.list
 -    |-- result
 -    `-- ...
 -
 - Files in the cache are organized however seems best on a per-module basis
 - with help from `cacheDir`, `cacheDirUniq`, and `cacheFile`.
 -
 - Expression paths are determined by `exprPath` or `exprPathExplicit`. They
 - get the base name by `show`ing the expression and `digest`ing the resulting
 - `String`, the extension based on the `OrthoLangType`, and the folder based on
 - constructor + function name if a function. Some made up examples:
 -
 -   ~/.ortholang/exprs/cut_list/f987e9b98a.str.list
 -   ~/.ortholang/exprs/cut_lit/a09f8e8b9c.str
 -   ~/.ortholang/exprs/crb_blast/38978s9a79.crb
 -   ~/.ortholang/exprs/gbk_to_fna/289379af7a.fna
 -
 - Var links are determined by `varPath` using the user-given name and `OrthoLangType`.
 -}

module OrthoLang.Core.Paths
  -- cutpaths
  ( OrthoLangPath()
  , toOrthoLangPath
  , fromOrthoLangPath
  , sharedPath
  , cutPathString
  , stringOrthoLangPath
  , toGeneric
  , fromGeneric
  -- cache dirs
  , cacheDir
  -- tmpfiles
  , argHashes
  -- , hashContent
  , exprPath
  , exprPathDigest
  , insertNewRulesDigest
  , decodeNewRulesDeps
  , exprPathExplicit
  , varPath
  , checkLit
  , checkLits
  , checkPath
  , checkPaths
  -- , resolveVar
  -- , resolveVars
  -- file io
  -- , readPath
  -- , readPaths
  -- , readLitPaths
  -- , writePath
  -- , writePaths
  -- , readLit
  -- , readLits
  -- , writeLit
  -- , writeLits
  -- read and write tmpfiles as strings
  -- , readString
  -- , readStrings
  -- , writeString
  -- , writeStrings
  -- symlink stuff
  -- , tmpLink
  -- , symlink
  , upBy
  , makeTmpdirRelative
  )
  where

import qualified Debug.Trace as DT

import Path (parseAbsFile, fromAbsFile)
import OrthoLang.Core.Types -- (OrthoLangConfig)
-- import OrthoLang.Core.Config (debug)
import OrthoLang.Core.Pretty (render, pPrint)
import OrthoLang.Core.Util (digest, trace)
import Data.String.Utils          (replace)
import Development.Shake.FilePath ((</>), (<.>), isAbsolute, makeRelative, splitPath)
import Data.List                  (intersperse, isPrefixOf)
import Data.List.Split            (splitOn)

import qualified Data.Map.Strict as M
import Development.Shake
import Data.IORef                 (readIORef, atomicModifyIORef')
import Control.Monad (when)
import Data.Maybe (fromJust, catMaybes)
import Data.IORef (atomicModifyIORef')

import Text.PrettyPrint.HughesPJClass (Pretty)

-- TODO take Text instead?
traceP :: (Pretty a, Show b) => String -> a -> b -> b
traceP name expr path = trace ("core.paths." ++ name) msg path
  where
    ren = render $ pPrint expr
    msg = ren ++ " -> " ++ show path -- TODO include types?

--------------
-- cutpaths --
--------------

-- Replace current absolute paths with generic placeholders that won't change
-- when the tmpDir is moved later or whatever.
-- TODO rewrite with a more elegant [(fn, string)] if there's time
toGeneric :: OrthoLangConfig -> String -> String
toGeneric cfg txt = replace (cfgWorkDir cfg) "$WORKDIR"
                  $ replace (cfgTmpDir  cfg) "$TMPDIR"
                  $ txt

-- Replace generic path placeholders with current paths
-- TODO rewrite with a more elegant [(fn, string)] if there's time
fromGeneric :: OrthoLangConfig -> String -> String
fromGeneric cfg txt = replace "$WORKDIR" (cfgWorkDir cfg)
                    $ replace "$TMPDIR"  (cfgTmpDir  cfg)
                    $ checkPath txt

isGeneric :: FilePath -> Bool
isGeneric path
  = path == "<<emptylist>>" -- TODO could this be <<emptystr>>?
  || "$TMPDIR"  `isPrefixOf` path
  || "$WORKDIR" `isPrefixOf` path

-- TODO print warning on failure?
toOrthoLangPath :: OrthoLangConfig -> FilePath -> OrthoLangPath
toOrthoLangPath cfg = OrthoLangPath . checkPath . toGeneric cfg . normalize
  where
    normalize p = case parseAbsFile p of
      Nothing -> error $ "toOrthoLangPath can't parse: " ++ p
      Just p' -> fromAbsFile p'

fromOrthoLangPath :: OrthoLangConfig -> OrthoLangPath -> FilePath
fromOrthoLangPath cfg (OrthoLangPath path) = fromGeneric cfg path

sharedPath :: OrthoLangConfig -> OrthoLangPath -> Maybe FilePath
sharedPath cfg (OrthoLangPath path) = fmap (\sd -> replace "$TMPDIR" sd path) (cfgShare cfg)

-- weird, but needed for writing cutpaths to files in Actions.hs
cutPathString :: OrthoLangPath -> String
cutPathString (OrthoLangPath path) = path

-- TODO this is basically just exporting OrthoLangPath right? any better way?
stringOrthoLangPath :: String -> OrthoLangPath
stringOrthoLangPath = OrthoLangPath

----------------
-- cache dirs --
----------------

cacheDir :: OrthoLangConfig -> String -> OrthoLangPath
cacheDir cfg modName = toOrthoLangPath cfg path
  where
    path = cfgTmpDir cfg </> "cache" </> modName

-- TODO cacheDirUniq or Explicit?

--------------
-- tmpfiles --
--------------

-- This is just a convenience used in exprPath
-- TODO rename hSomething?
argHashes :: OrthoLangState -> OrthoLangExpr -> [String]
argHashes s@(scr,_, _, _) (OrthoLangRef _ _ _ v) = case lookup v scr of
                                         Nothing -> error $ "no such var " ++ show v
                                         Just e  -> argHashes s e
argHashes _ (OrthoLangLit  _ _     v    ) = [digest v]
argHashes s (OrthoLangFun  _ _ _ _ es   ) = map (digest . exprPath s) es
argHashes s (OrthoLangBop  _ _ _ _ e1 e2) = map (digest . exprPath s) [e1, e2]
argHashes s (OrthoLangList _ _ _   es   ) = [digest $ map (digest . exprPath s) es]
argHashes _ (OrthoLangRules (CompiledExpr _ p _)) = [digest p] -- TODO is this OK? it's about all we can do

-- This is like the "resolve refs" part of argHashes, but works on plain paths in IO
-- resolveVar :: OrthoLangConfig -> OrthoLangPath -> IO OrthoLangPath
-- resolveVar cfg p@(OrthoLangPath path) =
--   -- TODO is just using OrthoLangPath directly here OK?
--   if "$TMPDIR/vars" `isPrefixOf` path
--     then resolveSymlinks cfg True (fromOrthoLangPath cfg p) >>= resolveVar cfg . toOrthoLangPath cfg
--     else return p

-- resolveVars :: OrthoLangConfig -> [OrthoLangPath] -> IO [OrthoLangPath]
-- resolveVars cfg = mapM (resolveVar cfg)

{- An attempt to speed up file access by making a tree of smaller dirs instead
 - of one giant one with a million+ files in it. Since it would complicate the
 - .tree files to split everything up, for now I just have a list of dirs that
 - are likely to benefit from it.
 - TODO write this in haskell instead of python! (currently in split_faa)
 -}
-- expandHashDirs :: FilePath -> FilePath
-- expandHashDirs = joinPath . map expandDir . splitPath 
--   where
--     expandDir d = if d `elem` dirsToExpand then undefined else d
--     dirsToExpand = ["load_faa"]
--     splitPath = undefined
--     joinPath = undefined

-- TODO rename to tmpPath?
exprPath :: OrthoLangState -> OrthoLangExpr -> OrthoLangPath
exprPath (_, cfg, _, _) (OrthoLangRules (CompiledExpr _ (ExprPath p) _)) = toOrthoLangPath cfg p
exprPath s@(scr, _, _, _) (OrthoLangRef _ _ _ v) = case lookup v scr of
                                         Nothing -> error $ "no such var " ++ show v ++ "\n" ++ show scr
                                         Just e  -> exprPath s e
exprPath s@(_, cfg, _, _) expr = traceP "exprPath" expr res
  where
    prefix = prefixOf expr
    rtype  = typeOf expr
    salt   = saltOf expr
    hashes = argHashes s expr
    res    = exprPathExplicit cfg prefix rtype salt hashes

exprPathDigest :: OrthoLangPath -> ExprDigest
exprPathDigest = ExprDigest . digest

insertNewRulesDigest :: OrthoLangState -> OrthoLangExpr -> IO ()
insertNewRulesDigest s e = let r = insertNewRulesDigest' s e
                           in DT.trace (show (exprPathDigest (exprPath s e)) ++ " -> " ++ show e) r

insertNewRulesDigest' st@(_, cfg, _, idr) expr = atomicModifyIORef' idr $
  \h@(HashedIDs {hExprs = ids}) -> (h {hExprs = M.insert eDigest (eType, ePath) ids}, ())
  where
    eType   = typeOf expr
    ePath   = exprPath st expr
    eDigest = exprPathDigest ePath

-- TODO what monad should this be in?
-- TODO encode lookup failure as Maybe? it indicates a programmer error though, not user error
-- TODO take an ExprPath
-- TODO remove any unneccesary path components before lookup, and count the necessary ones
-- TODO is drop 2 a safe enough way to remove 'result' and repeat salt from the ends of the paths?
-- TODO better split function
decodeNewRulesDeps :: OrthoLangConfig -> HashedIDsRef -> ExprPath
                   -> IO (OrthoLangType, [OrthoLangType], [OrthoLangPath])
decodeNewRulesDeps cfg idsRef o@(ExprPath out) = do
  HashedIDs {hExprs = ids} <- readIORef idsRef
  let dKeys  = map ExprDigest $ reverse $ drop 2 $ reverse $ drop 2 $ map init $ splitPath $ makeRelative (cfgTmpDir cfg) out
      dVals  = catMaybes $ map (\k -> M.lookup k ids) dKeys
      dVals' = trace "ortholang.core.types.decodeNewRulesDeps" (out ++ " -> " ++ show dVals) dVals
      dTypes = map fst dVals'
      dPaths = map snd dVals'
      oKey   = exprPathDigest $ toOrthoLangPath cfg out
      Just (oType, _) = M.lookup oKey ids
  -- TODO user-visible error here if one or more lookups fails
  -- liftIO $ putStrLn $ "decodeNewRulesDeps ids: " ++ show ids
  -- liftIO $ putStrLn $ "decodeNewRulesDeps p: " ++ show p
  -- liftIO $ putStrLn $ "decodeNewRulesDeps dKeys: " ++ show dKeys
  -- liftIO $ putStrLn $ "decodeNewRulesDeps dTypes: " ++ show dTypes
  -- liftIO $ putStrLn $ "decodeNewRulesDeps dVals': " ++ show dVals'
  when (length dVals /= length dKeys) $ error $ "failed to decode path: '" ++ out ++ "'"
  return (oType, dTypes, dPaths)

-- TODO remove repeat salt if fn is deterministic
exprPathExplicit :: OrthoLangConfig -> String -> OrthoLangType -> RepeatSalt -> [String] -> OrthoLangPath
exprPathExplicit cfg prefix rtype (RepeatSalt s) hashes = toOrthoLangPath cfg path
  where
    dir  = cfgTmpDir cfg </> "exprs" </> prefix
    base = (concat $ intersperse "/" $ hashes ++ [show s])
    path = dir </> base </> "result" -- <.> extOf rtype

-- TODO remove VarPath, ExprPath types once OrthoLangPath works everywhere
varPath :: OrthoLangConfig -> OrthoLangVar -> OrthoLangExpr -> OrthoLangPath
varPath cfg (OrthoLangVar (ReplaceID rep) var) expr = toOrthoLangPath cfg $ cfgTmpDir cfg </> repDir </> base
  where
    base = if var == "result" then var else var <.> extOf (typeOf expr)
    repDir = case rep of
               Nothing -> "vars"
               Just r  -> "reps" </> r -- TODO digest other stuff too, like the expr?

---------------
-- io checks --
---------------

-- These are just to alert me of programming mistakes,
-- and can be removed once the rest of the IO stuff is solid.
checkLit :: String -> String
checkLit lit = if isGeneric lit
                 then error $ "placeholder in lit: '" ++ lit ++ "'"
                 else lit

checkLits :: [String] -> [String] -- (or error, but let's ignore that)
checkLits = map checkLit


checkPath :: FilePath -> FilePath
checkPath path = if isAbsolute path || isGeneric path
                   then path
                   else error $ "invalid path: '" ++ path ++ "'"

checkPaths :: [FilePath] -> [FilePath]
checkPaths = map checkPath


-----------
-- utils --
-----------

-- TODO move this somewhere else?

-- TODO there must be a standard function for this right?
-- TODO guard that the top level stays to prevent it being /
upBy :: Int -> OrthoLangPath -> OrthoLangPath
upBy n (OrthoLangPath path) = OrthoLangPath path'
  where
    components = splitOn  "/" path -- TODO allow other delims?
    components' = reverse $ drop n $ reverse components
    path' = concat $ intersperse "/" $ components'

{- For passing scripts paths that don't depend on the $TMPDIR location, but
 - also don't require any ortholang funny business to read. It relies on the
 - assumption that the script will be called from inside $TMPDIR. The level
 - is how many ..s to add to get back up to $TMPDIR from where you call it.
 - TODO any good way to simplify that?
 -}
makeTmpdirRelative :: Int -> OrthoLangPath -> FilePath
makeTmpdirRelative level (OrthoLangPath path) = replace "$TMPDIR" dots path
  where
    dots = concat $ intersperse "/" $ take level $ repeat ".."
