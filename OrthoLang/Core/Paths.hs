{-# LANGUAGE OverloadedStrings #-}

-- TODO rename this module to TmpFiles?

{-|
OrthoLang makes heavy use of tmpfiles, and this module controls where they go
inside the main tmpdir. The overall layout is:

@
TMPDIR
|-- cache: per-module indexes, temporary files, etc.
|   |-- biomartr
|   |-- blast
|   |-- crb-blast
|   |-- seqio
|   `-- ...
|-- exprs: hashed result of every expression, organized by fn + arg hashes + salt
|   |-- all
|   |-- any
|   |-- concat_fastas
|   |-- crb_blast
|   |-- crb_blast_each
|   `-- ...
|-- vars: symlinks from user variable names to hashed expressions
|   |-- green_hits.str.list
|   |-- greens.faa.list
|   |-- plantcut.str.list
|   |-- result
|   `-- ...
`-- reps: per-repeat vars separated by random hash prefixes
    |-- 00f6aa06e2
    |   |-- green_hits.str.list
    |   |-- greens.faa.list
    |   |-- plantcut.str.list
    |   |-- result
    |   `-- ...
    |-- 13ba15a45b
    `-- ...
@

Files in the cache are organized however seems best on a per-module basis
with help from 'cacheDir', 'cacheDirUniq', and 'cacheFile'.

Var links are determined by 'varPath' using the user-given name and 'Type'.

Expression paths merit some more explanation. They are determined by
'exprPath' or 'exprPathExplicit'. They get the base name by 'show'ing the
expression and 'digest'ing the resulting 'String', and the folder based on
constructor + function name if a function. Some made up examples:

@
TMPDIR\/exprs\/cut_list\/f987e9b98a.str.list
TMPDIR\/exprs\/cut_lit\/a09f8e8b9c.str
TMPDIR\/exprs\/crb_blast\/38978s9a79.crb
TMPDIR\/exprs\/gbk_to_fna\/289379af7a.fna
@

For most functions, the full path is determined by fn name + argument digests
+ repeat salt, like this:

@
TMPDIR\/exprs\/fn_name\/\<digest1\>\/\<digest2\>\/\<digest3\>\/\<salt\>\/result
@

The repeat salt is a number (0, 1, ...) that causes OrthoLang to re-generate
the result multiple times by changing the path when a user calls one of the
repeat functions. Note: deterministic functions will soon have their repeat
salts removed.

The last directory with 'result' is a per-call tmpdir for executing scripts
and cleaning up anything they generate if they fail before trying again.
There may also be 'stdout' and 'stderr' logs, and lockfiles.

Digests are truncated md5sums of the corresponding expression path. Their
implementation doesn't really matter much. The important thing is that
whenever an expression is compiled to a path (TODO link to that), we also
store its digest (in the 'IDs' IORef for now) to look up later. Then we
can decode the dependencies of any function call (note: not every
expression!) from its path and tell Shake to 'need' them.

That works for fn calls, but not for literals or lists since they have no
depdendencies and an indeterminate number of dependencies respectively. So
their paths are chosen by content. There's also no need for salts or
per-call tmpdirs:

@
TMPDIR\/exprs\/\<num or str\>\/\<digest of content\>
TMPDIR\/exprs\/list\/\<digest of element digests\>
@

Lists are especially tricky because we can't necessarily know their contents at
\"Rules-time\". Even lists of literals can be generated by functions. So we
treat them as having only one argument digest for their whole contents. In case
of explicit 'str' or 'num' literals from the source code we can fold over their
digests to generate it, and in case of function calls we use their digest
directly.

There are also a few special cases where we have to break up the fn call
tmpdirs further for performance reasons, because having more than ~1000
files per dir is really slow on Linux. So for example 'split_faa' has a
whole tree of dirs for all the tiny FASTA files it produces.

The @TMPDIR\/cache\/lines@ dir is also special. Any text file written anywhere
by 'writeCachedLines' actually goes there, and is symlinked to its
destination. That sounds complicated, but is necessary to make sure the same
file contents always have the same canonical path, which is necessary for
set deduplication to work.
-}

module OrthoLang.Core.Paths
  (
  -- * Convert to\/from paths
    Path()
  , toPath
  , fromPath
  , sharedPath
  , pathString
  , stringPath
  , toGeneric
  , fromGeneric

  -- * Generate paths
  , cacheDir
  , exprPath
  , exprPathExplicit
  , varPath

  -- * Validate paths
  , checkLit
  , checkLits
  , checkPath
  , checkPaths

  -- * Generate path digests
  , exprPathDigest
  , exprDigests
  , scriptDigests
  , listExprs
  , listScriptExprs
  -- , insertNewRulesDigest
  , decodeNewRulesDeps

  -- * Misc utilities (move to Util.hs?)
  , argHashes
  , upBy
  , makeTmpdirRelative

  -- TODO remove? move to Actions?
  -- tmpfiles
  -- , hashContent
  -- , resolveVar
  -- , resolveVars
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
  )
  where

-- import qualified Debug.Trace as DT

import Path (parseAbsFile, fromAbsFile)
import OrthoLang.Core.Types -- (Config)
-- import OrthoLang.Core.Config (debug)
import OrthoLang.Core.Pretty (render, pPrint)
import OrthoLang.Core.Util (digest, trace, traceShow)
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

import Text.PrettyPrint.HughesPJClass (Pretty, pPrint, render)

-- TODO take Text instead?
traceP :: (Pretty a, Show b) => String -> a -> b -> b
traceP name expr path = trace ("core.paths." ++ name) msg path
  where
    ren = render $ pPrint expr
    msg = "\"" ++ ren ++ "' -> " ++ show path -- TODO include types?

-- traceD name c s expr = trace ("core.paths." ++ name) msg
--   where
--     ren  = render $ pPrint expr
--     -- ren  = show expr
--     path = exprPath c s expr
--     dig  = exprPathDigest path
--     msg  = "\"" ++ ren ++ "' -> (" ++ show dig ++ ", " ++ show path ++ ")"

--------------
-- cutpaths --
--------------

-- | Replace current absolute paths with generic placeholders that won't change
-- when the tmpDir is moved later or whatever.
-- TODO rewrite with a more elegant [(fn, string)] if there's time
toGeneric :: Config -> String -> String
toGeneric cfg txt = replace (cfgWorkDir cfg) "$WORKDIR"
                  $ replace (cfgTmpDir  cfg) "$TMPDIR"
                  $ txt

-- | Replace generic path placeholders with current paths
-- TODO rewrite with a more elegant [(fn, string)] if there's time
fromGeneric :: Config -> String -> String
fromGeneric cfg txt = replace "$WORKDIR" (cfgWorkDir cfg)
                    $ replace "$TMPDIR"  (cfgTmpDir  cfg)
                    $ checkPath txt

isGeneric :: FilePath -> Bool
isGeneric path
  = path == "<<emptylist>>" -- TODO could this be <<emptystr>>?
  || "$TMPDIR"  `isPrefixOf` path
  || "$WORKDIR" `isPrefixOf` path

-- TODO print warning on failure?
toPath :: Config -> FilePath -> Path
toPath cfg = Path . checkPath . toGeneric cfg . normalize
  where
    normalize p = case parseAbsFile p of
      Nothing -> error $ "toPath can't parse: " ++ p
      Just p' -> fromAbsFile p'

fromPath :: Config -> Path -> FilePath
fromPath cfg (Path path) = fromGeneric cfg path

sharedPath :: Config -> Path -> Maybe FilePath
sharedPath cfg (Path path) = fmap (\sd -> replace "$TMPDIR" sd path) (cfgShare cfg)

-- | weird, but needed for writing cutpaths to files in Actions.hs
pathString :: Path -> String
pathString (Path path) = path

-- TODO this is basically just exporting Path right? any better way?
stringPath :: String -> Path
stringPath = Path

----------------
-- cache dirs --
----------------

cacheDir :: Config -> String -> Path
cacheDir cfg modName = toPath cfg path
  where
    path = cfgTmpDir cfg </> "cache" </> modName

-- TODO cacheDirUniq or Explicit?

--------------
-- tmpfiles --
--------------

-- | This is just a convenience used in exprPath
-- TODO rename hSomething?
-- TODO does it need the config at all?
argHashes :: Config -> Script -> Expr -> [String]
argHashes c s (Ref _ _ _ v) = case lookup v (sAssigns s) of
                                         Nothing -> error $ "no such var " ++ show v
                                         Just e  -> argHashes c s e
argHashes _ _ (Lit  _ _     v    ) = [digest v]
argHashes c s (Fun  _ _ _ _ es   ) = map (digest . exprPath c s) es
argHashes c s (Bop  _ _ _ _ e1 e2) = map (digest . exprPath c s) [e1, e2]
argHashes c s (Lst _ _ _   es   ) = [digest $ map (digest . exprPath c s) es]
argHashes _ _ (Com (CompiledExpr _ p _)) = [digest p] -- TODO is this OK? it's about all we can do

-- This is like the "resolve refs" part of argHashes, but works on plain paths in IO
-- resolveVar :: Config -> Path -> IO Path
-- resolveVar cfg p@(Path path) =
--   -- TODO is just using Path directly here OK?
--   if "$TMPDIR/vars" `isPrefixOf` path
--     then resolveSymlinks cfg True (fromPath cfg p) >>= resolveVar cfg . toPath cfg
--     else return p

-- resolveVars :: Config -> [Path] -> IO [Path]
-- resolveVars cfg = mapM (resolveVar cfg)

{- | An attempt to speed up file access by making a tree of smaller dirs instead
 -   of one giant one with a million+ files in it. Since it would complicate the
 -   .tree files to split everything up, for now I just have a list of dirs that
 -   are likely to benefit from it.
 -
 - TODO write this in haskell instead of python! (currently in split_faa)
 -}
-- expandHashDirs :: FilePath -> FilePath
-- expandHashDirs = joinPath . map expandDir . splitPath 
--   where
--     expandDir d = if d `elem` dirsToExpand then undefined else d
--     dirsToExpand = ["load_faa"]
--     splitPath = undefined
--     joinPath = undefined

-- rExpr s e@(Bop t r ds _ e1 e2) = rExpr s fn
--   where
--     es = 
--     fn = 

-- | Temporary hack to fix Bop expr paths
bop2fun :: Expr -> Expr
bop2fun e@(Bop t r ds _ e1 e2) = Fun t r ds (prefixOf e) [Lst t r ds [e1, e2]]
bop2fun e = error $ "bop2fun call with non-Bop: \"" ++ render (pPrint e) ++ "\""

-- TODO rename to tmpPath?
-- TODO remove the third parseenv arg (digestmap)?
exprPath :: Config -> Script -> Expr -> Path
exprPath c _ (Com (CompiledExpr _ (ExprPath p) _)) = toPath c p
exprPath c s (Ref _ _ _ v) = case lookup v (sAssigns s) of
                               Nothing -> error $ "no such var " ++ show v ++ "\n" ++ show (sAssigns s)
                               Just e  -> exprPath c s e
exprPath c s e@(Bop _ _ _ _ _ _) = exprPath c s (bop2fun e)
exprPath c s expr = traceP "exprPath" expr res
  where
    prefix = prefixOf expr
    rtype  = typeOf expr
    salt   = saltOf expr
    hashes = argHashes c s expr
    res    = exprPathExplicit c prefix rtype salt hashes

exprPathDigest :: Path -> PathDigest
exprPathDigest = PathDigest . digest

exprDigest :: Config -> Script -> Expr -> DigestMap
exprDigest cfg scr expr = traceShow "core.paths.exprDigest" res
  where
    p = exprPath cfg scr expr
    dKey = PathDigest $ digest p
    res = M.singleton dKey (typeOf expr, p)

exprDigests :: Config -> Script -> [Expr] -> DigestMap
exprDigests cfg scr exprs = M.unions $ map (exprDigest cfg scr) $ concatMap listExprs exprs

scriptDigests :: Config -> Script -> DigestMap
scriptDigests cfg scr = exprDigests cfg scr $ listScriptExprs scr

{-|
"Flatten" (or "unfold"?) an expression into a list of it + subexpressions.

TODO is there a better word for this, or a matching typeclass?
-}
listExprs :: Expr -> [Expr]
listExprs e@(Lit _ _ _) = [e]
listExprs e@(Ref _ _ _ _) = [e]
listExprs e@(Bop _ _ _ _ e1 e2) = e : concatMap listExprs [bop2fun e, e1, e2] -- TODO remove e?
listExprs e@(Fun _ _ _ _ es   ) = e : concatMap listExprs es
listExprs e@(Lst _ _ _   es   ) = e : concatMap listExprs es
listExprs e@(Com _) = [e] -- TODO is this right?

listScriptExprs :: Script -> [Expr]
listScriptExprs (Script {sAssigns = as}) = concatMap listExprs $ map snd as

-- insertNewRulesDigest :: GlobalEnv -> Expr -> IO ()
-- insertNewRulesDigest st@(_, cfg, _, idr) expr
--   = traceD "insertNewRulesDigest" st expr
--   $ atomicModifyIORef' idr
--   $ \h@(IDs {hExprs = ids}) -> (h {hExprs = M.insert eDigest (eType, ePath) ids}, ())
--   where
--     eType   = typeOf expr
--     ePath   = exprPath cfg scr expr
--     eDigest = exprPathDigest ePath

-- TODO what monad should this be in?
-- TODO encode lookup failure as Maybe? it indicates a programmer error though, not user error
-- TODO take an ExprPath
-- TODO remove any unneccesary path components before lookup, and count the necessary ones
-- TODO is drop 2 a safe enough way to remove 'result' and repeat salt from the ends of the paths?
-- TODO better split function
decodeNewRulesDeps :: Config -> DigestMap -> ExprPath
                   -> IO (Type, [Type], [Path])
decodeNewRulesDeps cfg dMap o@(ExprPath out) = do
  let dKeys  = map PathDigest
             $ reverse $ dropWhile (/= "exprs")
             $ reverse $ drop 2
             $ map init $ splitPath $ makeRelative (cfgTmpDir cfg) out
      dVals  = catMaybes $ map (\k -> M.lookup k dMap) dKeys
      dVals' = trace "ortholang.core.types.decodeNewRulesDeps" ("\"" ++ out ++ "' -> " ++ show dVals) dVals
      dTypes = map fst dVals'
      dPaths = map snd dVals'
      oKey   = exprPathDigest $ toPath cfg out
      Just (oType, _) = M.lookup oKey dMap
  -- TODO user-visible error here if one or more lookups fails
  -- liftIO $ putStrLn $ "decodeNewRulesDeps ids: " ++ show ids
  -- liftIO $ putStrLn $ "decodeNewRulesDeps p: " ++ show p
  -- liftIO $ putStrLn $ "decodeNewRulesDeps dKeys: " ++ show dKeys
  -- liftIO $ putStrLn $ "decodeNewRulesDeps dTypes: " ++ show dTypes
  -- liftIO $ putStrLn $ "decodeNewRulesDeps dVals': " ++ show dVals'
  when (length dVals /= length dKeys) $ error $ "failed to decode path: \"" ++ out ++ "\""
  return (oType, dTypes, dPaths)

-- TODO remove repeat salt if fn is deterministic
exprPathExplicit :: Config -> String -> Type -> Salt -> [String] -> Path
exprPathExplicit cfg prefix rtype (Salt s) hashes = toPath cfg path
  where
    dir  = cfgTmpDir cfg </> "exprs" </> prefix
    base = (concat $ intersperse "/" $ hashes ++ [show s])
    path = dir </> base </> "result" -- <.> extOf rtype

-- TODO remove VarPath, ExprPath types once Path works everywhere
varPath :: Config -> Var -> Expr -> Path
varPath cfg (Var (RepID rep) var) expr = toPath cfg $ cfgTmpDir cfg </> repDir </> base
  where
    base = if var == "result" then var else var <.> extOf (typeOf expr)
    repDir = case rep of
               Nothing -> "vars"
               Just r  -> "reps" </> r -- TODO digest other stuff too, like the expr?

---------------
-- io checks --
---------------

-- | These are just to alert me of programming mistakes,
-- and can be removed once the rest of the IO stuff is solid.
checkLit :: String -> String
checkLit lit = if isGeneric lit
                 then error $ "placeholder in lit: \"" ++ lit ++ "\""
                 else lit

checkLits :: [String] -> [String] -- (or error, but let's ignore that)
checkLits = map checkLit


checkPath :: FilePath -> FilePath
checkPath path = if isAbsolute path || isGeneric path
                   then path
                   else error $ "invalid path: \"" ++ path ++ "\""

checkPaths :: [FilePath] -> [FilePath]
checkPaths = map checkPath


-----------
-- utils --
-----------

-- TODO move this somewhere else?

-- TODO there must be a standard function for this right?
-- TODO guard that the top level stays to prevent it being /
upBy :: Int -> Path -> Path
upBy n (Path path) = Path path'
  where
    components = splitOn  "/" path -- TODO allow other delims?
    components' = reverse $ drop n $ reverse components
    path' = concat $ intersperse "/" $ components'

{- | For passing scripts paths that don't depend on the $TMPDIR location, but
 -   also don't require any ortholang funny business to read. It relies on the
 -   assumption that the script will be called from inside $TMPDIR. The level
 -   is how many ..s to add to get back up to $TMPDIR from where you call it.
 -
 - TODO any good way to simplify that?
 -}
makeTmpdirRelative :: Int -> Path -> FilePath
makeTmpdirRelative level (Path path) = replace "$TMPDIR" dots path
  where
    dots = concat $ intersperse "/" $ take level $ repeat ".."
