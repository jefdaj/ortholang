module OrthoLang.Core.Types
  (
  -- * Types and Newtypes
    OrthoLangPath(..)
  , Action1
  , Action2
  , Action3
  , ActionFn
  , RulesFn
  , TypeChecker
  -- * Action functions for NewRules
  , NewAction1
  , NewAction2
  , NewAction3
  , NewRulesFn
  -- * Basic data structures
  , OrthoLangAssign
  , OrthoLangExpr(..)
  , CompiledExpr(..)
  , OrthoLangConfig(..)
  , findType
  , findModule
  , findFunction
  , listFunctions
  , listFunctionNames
  , operatorChars
  -- , WrapperConfig(..)
  , OrthoLangType(..)
  , ReplaceID(..)
  , RepeatSalt(..)
  , OrthoLangVar(..)
  , OrthoLangScript
  , Locks
  , HashedIDs(..)
  , HashedIDsRef
  , GlobalEnv
  , ensureResult
  , lookupResult
  -- , Assoc(..) -- we reuse this from Parsec
  -- , OrthoLangFixity(..)
  -- parse monad
  , ParseM
  -- repl monad
  -- , print
  , prompt
  , runReplM
  , ReplM
  -- misc
  -- , prettyShow
  , str, num -- TODO load these from modules
  , typeOf
  , extOf
  , descOf
  , depsOf
  , rDepsOf
  , defaultShow
  , defaultShowN -- same but lets you pick how many lines to show
  -- module stuff (in flux)
  , OrthoLangFunction(..)
  , FnTag(..)
  , mkTypeDesc
  , OrthoLangModule(..)
  , saltOf
  , setSalt
  , prefixOf
  -- * Wrappers to prevent confusing the various paths
  , CacheDir(..)
  , ExprPath(..)
  , VarPath(..)
  , ResPath(..)
  , ExprDigest(..)
  -- * Misc experimental stuff
  , extractExprs
  , extractLoads
  , typeMatches
  , typesMatch
  , nonEmptyType
  , isNonEmpty
  -- new rules infrastructure
  )
  where

-- import Prelude hiding (print)
import qualified Data.Map.Strict as M
import Text.Parsec (Parsec)

import OrthoLang.Core.Locks (Locks, withReadLock)
import OrthoLang.Core.Util  (readFileStrict, readFileLazy, headOrDie, trace)

import Development.Shake              (Rules, Action, Resource)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy       (StateT, execStateT, lift)
import Control.Monad.Trans.Maybe      (MaybeT(..), runMaybeT)
import Data.List                      (nub, find, isPrefixOf)
import System.Console.Haskeline       (InputT, getInputLine, runInputT, Settings)
import Data.IORef                     (IORef)
import Data.Maybe (fromJust, catMaybes)
-- import Text.PrettyPrint.HughesPJClass (Doc, text, doubleQuotes)

newtype OrthoLangPath = OrthoLangPath FilePath deriving (Eq, Ord, Show)

-- Note that each ActionN takes N+1 OrthoLangPaths, because the first is the output
-- TODO take the output last instead?
type Action1 = OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangPath -> OrthoLangPath -> Action ()
type Action2 = OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangPath -> OrthoLangPath -> OrthoLangPath -> Action ()
type Action3 = OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangPath -> OrthoLangPath -> OrthoLangPath -> OrthoLangPath -> Action ()

-- TODO remove when able in favor of well-typed versions above
type ActionFn    = OrthoLangConfig -> CacheDir -> [ExprPath] -> Action ()

type RulesFn     = GlobalEnv -> OrthoLangExpr -> Rules ExprPath
type TypeChecker = [OrthoLangType] -> Either String OrthoLangType

type NewAction1 = OrthoLangConfig -> Locks -> HashedIDsRef -> ExprPath -> FilePath                         -> Action ()
type NewAction2 = OrthoLangConfig -> Locks -> HashedIDsRef -> ExprPath -> FilePath -> FilePath             -> Action ()
type NewAction3 = OrthoLangConfig -> Locks -> HashedIDsRef -> ExprPath -> FilePath -> FilePath -> FilePath -> Action ()

type NewRulesFn = OrthoLangConfig -> Locks -> HashedIDsRef -> Rules ()

newtype CacheDir = CacheDir FilePath deriving (Read, Show, Eq) -- ~/.ortholang/cache/<modname>
newtype ExprPath = ExprPath FilePath deriving (Read, Show, Eq) -- ~/.ortholang/exprs/<fnname>/<hash>.<type>
newtype VarPath  = VarPath  FilePath deriving (Read, Show, Eq) -- ~/.ortholang/vars/<varname>.<type>
newtype ResPath  = ResPath  FilePath deriving (Read, Show, Eq) -- ~/.ortholang/vars/result[.<hash>.<type>]
newtype ExprDigest = ExprDigest String deriving (Read, Show, Eq, Ord)

-- Filename extension, which in OrthoLang is equivalent to variable type
-- TODO can this be done better with phantom types?
-- data Ext = ListOf Ext | Ext String
  -- deriving (Eq, Show, Read)

-- A digest identifying which replace_* call the variable is part of.
-- TODO This isn't very elegant; can it be removed?
newtype ReplaceID = ReplaceID (Maybe String) deriving (Eq, Show, Read)

-- A number that can be incremented to change the expression's hash, causing repeat evaluation.
newtype RepeatSalt = RepeatSalt Int deriving (Eq, Show, Read)

data OrthoLangVar = OrthoLangVar ReplaceID String
  deriving (Eq, Show, Read)
 
-- the common fields are:
-- * return type
-- * salt, which can be changed to force re-evaluation of an expr + all depends
--   (it should start at 0 and be incremented, though that doesn't really matter)
-- TODO start from 1 instead of 0?
-- TODO test that it works correctly! in particular, it should go thru refs!
--      (do we need to add salts of subepxressions or something? or use randoms?)
-- * list of dependencies (except lits don't have any)
data OrthoLangExpr
  = OrthoLangLit OrthoLangType RepeatSalt String
  | OrthoLangRef OrthoLangType RepeatSalt [OrthoLangVar] OrthoLangVar -- do refs need a salt? yes! (i think?)
  | OrthoLangBop OrthoLangType RepeatSalt [OrthoLangVar] String  OrthoLangExpr OrthoLangExpr
  | OrthoLangFun OrthoLangType RepeatSalt [OrthoLangVar] String [OrthoLangExpr]
  | OrthoLangList OrthoLangType RepeatSalt [OrthoLangVar] [OrthoLangExpr]
  | OrthoLangRules CompiledExpr -- wrapper around previously-compiled rules (see below)
  deriving (Eq, Show)

-- An expression that has already been compiled to Rules, wrapped so it can be
-- passed to another function. Because Rules can't be shown or compared, we
-- also carry around the original OrthoLangExpr. TODO is the expr necessary? helpful?
-- The CompiledExpr constructor is just here so we can customize the Show and Eq instances.
-- The extra ExprPath is weird, but seems to be required since we can't get at the second one.
data CompiledExpr = CompiledExpr OrthoLangType ExprPath (Rules ExprPath)

-- TODO is it a bad idea to hide the compiled-ness?
-- TODO can this be made into a OrthoLangPath?
-- TODO is show ever really needed?
instance Show CompiledExpr where
  show (CompiledExpr t p _) = "CompiledExpr " ++ extOf t ++ " " ++ show p ++ " <<Rules ExprPath>>"

-- CompiledExprs are compared by the expressions they were compiled from.
instance Eq CompiledExpr where
  (CompiledExpr _ p1 _) == (CompiledExpr _ p2 _) = p1 == p2

-- TODO is this not actually needed? seems "show expr" handles it?
saltOf :: OrthoLangExpr -> RepeatSalt
saltOf (OrthoLangLit _ n _)       = n
saltOf (OrthoLangRef _ n _ _)     = n
saltOf (OrthoLangBop _ n _ _ _ _) = n
saltOf (OrthoLangFun _ n _ _ _)   = n
saltOf (OrthoLangList _ n _ _)     = n
saltOf (OrthoLangRules (CompiledExpr _ _ _)) = error "CompiledExprs don't have salts" -- TODO is that OK?

-- TODO this needs to be recursive?
-- TODO would a recursive version be able to replace addPrefixes in ReplaceEach?
setSalt :: Int -> OrthoLangExpr -> OrthoLangExpr
setSalt r (OrthoLangLit t _ s)          = OrthoLangLit  t (RepeatSalt r) s
setSalt r (OrthoLangRef t _ ds v)       = OrthoLangRef  t (RepeatSalt r) ds v
setSalt r (OrthoLangBop t _ ds s e1 e2) = OrthoLangBop  t (RepeatSalt r) ds s e1 e2
setSalt r (OrthoLangFun t _ ds s es)    = OrthoLangFun  t (RepeatSalt r) ds s es
setSalt r (OrthoLangList t _ ds es)     = OrthoLangList t (RepeatSalt r) ds es
setSalt _ (OrthoLangRules (CompiledExpr _ _ _)) = error "setSalt not implemented for compiled rules" -- TODO should it be?

-- TODO add names to the OrthoLangBops themselves... or associate with prefix versions?
prefixOf :: OrthoLangExpr -> String
prefixOf (OrthoLangLit rtn _ _     ) = extOf rtn
prefixOf (OrthoLangFun _ _ _ name _) = name
prefixOf (OrthoLangList _ _ _ _    ) = "list"
prefixOf (OrthoLangRef _ _ _ _     ) = error  "OrthoLangRefs don't need a prefix"
prefixOf (OrthoLangRules (CompiledExpr _ _ _)) = error "CompiledExprs don't need a prefix"
prefixOf (OrthoLangBop _ _ _ n _ _ ) = case n of
                                   "+" -> "add"
                                   "-" -> "subtract"
                                   "*" -> "multiply"
                                   "/" -> "divide"
                                   "|" -> "any"
                                   "&" -> "all"
                                   "~" -> "diff"
                                   x   -> error $ "unknown OrthoLangBop: '" ++ x ++ "'"


-- TODO have a separate OrthoLangAssign for "result"?
type OrthoLangAssign = (OrthoLangVar, OrthoLangExpr)

-- TODO should this be RulesState? aka all the stuff needed when parsing/compiling in Rules
data OrthoLangScript = OrthoLangScript
  { sAssigns :: [OrthoLangAssign]
  -- ^ Structured representation of the code suitable for printing, transforming, etc.
  , sDepends :: M.Map ExprDigest (OrthoLangType, OrthoLangPath)
  -- ^ Map suitable for auto-discovery of dependencies from expression path digests
  }

ensureResult :: OrthoLangScript -> OrthoLangScript
ensureResult scr = if null scr then noRes else scr'
  where
    noRes = [(resVar, OrthoLangLit str (RepeatSalt 0) "no result")]
    resVar = OrthoLangVar (ReplaceID Nothing) "result"
    scr' = scr ++ [(resVar, snd $ last scr)]

lookupResult :: [(OrthoLangVar, b)] -> Maybe b
lookupResult scr = if null matches
  then (if null scr then Nothing else Just (snd $ last scr))
  else Just (snd $ last matches)
  where
    matches = filter (\(OrthoLangVar _ v, _) -> v == "result") scr

-- TODO tExt etc aren't well defined for the other constructors... is that a problem?
-- TODO how to make the record fields not partial functions?
data OrthoLangType
  = Empty -- TODO remove this? should never be a need to define an empty list
  | ListOf OrthoLangType
  | ScoresOf OrthoLangType

  {- These are kind of like simpler, less extensible typeclasses. They're just
   - a list of types that can be treated similarly in some circumstances, for
   - example "files whose length is their number of lines" or "FASTA files (faa
   - or fna)".
   -}
  | OrthoLangTypeGroup
    { tgExt   :: String
    , tgDesc  :: String
    -- , tgShow  :: OrthoLangConfig -> Locks -> FilePath -> IO String -- TODO remove?
    , tgMember :: OrthoLangType -> Bool
    }

  | OrthoLangType
    { tExt  :: String
    , tDesc :: String -- TODO include a longer help text too
    , tShow :: OrthoLangConfig -> Locks -> FilePath -> IO String
    }
  -- deriving (Eq, Show, Read)

defaultShow :: OrthoLangConfig -> Locks -> FilePath -> IO String
defaultShow = defaultShowN 5

defaultShowN :: Int -> OrthoLangConfig -> Locks -> FilePath -> IO String
defaultShowN nLines _ locks = fmap (unlines . fmtLines . lines) . (readFileLazy locks)
  where
    fmtLine  l  = if length l > 80 then take 77 l ++ "..." else l
    fmtLines ls = let nPlusOne = map fmtLine $ take (nLines + 1) ls
                  in if length nPlusOne > nLines
                    then init nPlusOne ++ ["..."]
                    else nPlusOne

-- TODO is it dangerous to just assume they're the same by extension?
--      maybe we need to assert no duplicates while loading modules?
-- TODO should this use typesMatch?
instance Eq OrthoLangType where
  Empty        == Empty        = True
  (ListOf a)   == (ListOf b)   = a == b
  (ScoresOf a) == (ScoresOf b) = a == b
  (OrthoLangType {tExt = t1}) == (OrthoLangType {tExt = t2}) = t1 == t2
  (OrthoLangTypeGroup {tgExt = t1}) == (OrthoLangTypeGroup {tgExt = t2}) = t1 == t2
  (OrthoLangTypeGroup {tgMember = fn}) == t = fn t
  t == (OrthoLangTypeGroup {tgMember = fn}) = fn t
  _ == _ = False -- TODO should this behave differently?

instance Show OrthoLangType where
  show = extOf

typeOf :: OrthoLangExpr -> OrthoLangType
typeOf (OrthoLangLit   t _ _      ) = t
typeOf (OrthoLangRef   t _ _ _    ) = t
typeOf (OrthoLangBop   t _ _ _ _ _) = t
typeOf (OrthoLangFun   t _ _ _ _  ) = t
typeOf (OrthoLangList  t _ _ _    ) = ListOf t -- t can be Empty
typeOf (OrthoLangRules (CompiledExpr t _ _)) = t
-- typeOf (OrthoLangList _ _ _ ts     ) = ListOf $ nonEmptyType $ map typeOf ts
-- typeOf (OrthoLangList _ _ _ []     ) = Empty
-- typeOf (OrthoLangList _ _ _ []     ) = ListOf Empty

-- Works around a bug where if the first element is an empty list but others
-- have elements, it would call the whole thing an "emptylist.list".
-- Note no typechecking happens here; heterogenous lists won't be noticed.
-- nonEmptyType :: [OrthoLangExpr] -> OrthoLangType
-- nonEmptyType    []  = Empty
-- nonEmptyType (x:[]) = typeOf x -- catches (ListOf Empty)
-- nonEmptyType (_:xs) = nonEmptyType xs

-- note that traceShow in here can cause an infinite loop
-- and that there will be an issue if it's called on Empty alone
extOf :: OrthoLangType -> String
extOf Empty                      = "empty" -- for lists with nothing in them yet
extOf (ListOf                t ) = extOf t ++ ".list"
extOf (ScoresOf              t ) = extOf t ++ ".scores"
extOf (OrthoLangTypeGroup {tgExt = t}) = t -- TODO should this not be called an extension? it's never written to disk
                                     -- TODO put back the < > ?
extOf (OrthoLangType      { tExt = t}) = t

-- TODO is this needed for anything other than repl :help? if not, could use IO to load docs
descOf :: OrthoLangType -> String
descOf Empty         = "empty list" -- for lists with nothing in them yet
descOf (ListOf   t ) = "list of " ++ descOf t
descOf (ScoresOf t ) = "scores for " ++ descOf t
descOf (OrthoLangTypeGroup {tgDesc = t}) = t
descOf (OrthoLangType      { tDesc = t}) = t

varOf :: OrthoLangExpr -> [OrthoLangVar]
varOf (OrthoLangRef _ _ _ v) = [v]
varOf _                = [ ]

depsOf :: OrthoLangExpr -> [OrthoLangVar]
depsOf (OrthoLangLit  _ _ _         ) = []
depsOf (OrthoLangRef  _ _ vs v      ) = v:vs -- TODO redundant?
depsOf (OrthoLangBop  _ _ vs _ e1 e2) = nub $ vs ++ concatMap varOf [e1, e2]
depsOf (OrthoLangFun  _ _ vs _ es   ) = nub $ vs ++ concatMap varOf es
depsOf (OrthoLangList _ _ vs   es   ) = nub $ vs ++ concatMap varOf es
depsOf (OrthoLangRules (CompiledExpr _ _ _)) = [] -- TODO should this be an error instead? their deps are accounted for

rDepsOf :: OrthoLangScript -> OrthoLangVar -> [OrthoLangVar]
rDepsOf scr var = map fst rDeps
  where
    rDeps = filter (\(_,e) -> isRDep e) scr
    isRDep expr = elem var $ depsOf expr

-- TODO move to modules as soon as parsing works again
-- TODO keep literals in the core along with refs and stuff? seems reasonable
-- TODO how about lists/sets, are those core too?

str :: OrthoLangType
str = OrthoLangType
  { tExt  = "str"
  , tDesc = "string"
  -- TODO make one of the read functions be IO for this instead
  , tShow = \_ ls f -> do
      -- putStrLn $ "reading " ++ f
      txt <- fmap init $ withReadLock ls f $ readFileStrict ls f
      let txt' = if txt == "<<emptystr>>" then "" else txt
      return $ "\"" ++ txt' ++ "\""
  }

num :: OrthoLangType
num = OrthoLangType
  { tExt  = "num"
  , tDesc = "number in scientific notation"
  , tShow = \_ ls f -> do
      txt <- withReadLock ls f $ readFileStrict ls f
      return $ init txt -- TODO prettyNum?
  }

------------
-- config --
------------

-- TODO always load defaults for WorkDir, TmpDir, Verbose
-- TODO make these into FilePaths and an Int/Bool
-- TODO rename cfg prefix to just c?
data OrthoLangConfig = OrthoLangConfig
  { cfgScript  :: Maybe FilePath
  , cfgInteractive :: Bool
  , cfgTmpDir  :: FilePath
  , cfgWorkDir :: FilePath
  , cfgDebug   :: Maybe String
  , cfgModules :: [OrthoLangModule]
  , cfgWrapper :: Maybe FilePath
  , cfgOutFile :: Maybe FilePath
  , cfgShare   :: Maybe FilePath
  , cfgReport  :: Maybe String
  , cfgTestPtn :: [String] -- [] means run all tests
  , cfgWidth   :: Maybe Int -- for testing
  , cfgSecure  :: Bool
  , cfgNoProg  :: Bool
  , cfgParLock :: Resource
  , cfgOS      :: String
  , cfgThreads :: Int
  }
  deriving Show

-- note: only lists the first name of each function,
--       which for binary operators will be the single-char one
listFunctionNames :: OrthoLangConfig -> [String]
listFunctionNames cfg = map fName $ concat $ map mFunctions $ cfgModules cfg

findModule :: OrthoLangConfig -> String -> Maybe OrthoLangModule
findModule cfg name = find (\m -> mName m == name) (cfgModules cfg)

-- used by the compiler and repl
-- TODO find bops by char or name too
-- TODO filter to get a list and assert length == 1fs
findFunction :: OrthoLangConfig -> String -> Maybe OrthoLangFunction
findFunction cfg name = find (\f -> fName f == name || fmap (\c -> [c]) (fOpChar f) == Just name) fs
  where
    ms = cfgModules cfg
    fs = concatMap mFunctions ms

findType :: OrthoLangConfig -> String -> Maybe OrthoLangType
findType cfg ext = find (\t -> extOf t == ext) ts
  where
    ms = cfgModules cfg
    ts = concatMap mTypes ms

listFunctions :: OrthoLangConfig -> [OrthoLangFunction]
listFunctions cfg = concatMap mFunctions $ cfgModules cfg

-- Now with guard against accidentally including parts of prefix fn names!
operatorChars :: OrthoLangConfig -> [Char]
operatorChars cfg = catMaybes $ map fOpChar $ listFunctions cfg

-----------------
-- Parse monad --
-----------------

-- we sanitize the input fasta files to prevent various bugs,
-- then use this hash -> seqid map to put the original ids back at the end
-- hFiles is for making paths generic
-- hSeqIDs is the main one, and stores hash -> seqid maps indexed by their (generic) hFiles path
-- hExprs is for decoding exprs/<hash>/<hash>/... paths
-- TODO use bytestring-tries rather than maps with string keys?
-- TODO should this be ActionIDs in general? aka all the stuff that might be needed in Action
data HashedIDs = HashedIDs
  { hFiles  :: M.Map String String
  , hSeqIDs :: M.Map String (M.Map String String)
  }

-- this lets me cheat and not bother threading the ID map through all the monad stuff
-- TODO go back and do it right
type HashedIDsRef = IORef HashedIDs

-- TODO can this be broken up by scope? GlobalState, RulesState, ActionState
type GlobalEnv = (OrthoLangScript, OrthoLangConfig, Locks, HashedIDsRef)
type ParseM a = Parsec String GlobalEnv a

----------------
-- Repl monad --
----------------

type ReplM a = StateT GlobalEnv (MaybeT (InputT IO)) a

-- TODO use useFile(Handle) for stdin?
-- TODO use getExternalPrint to safely print during Tasty tests!
runReplM :: Settings IO -> ReplM a -> GlobalEnv -> IO (Maybe GlobalEnv)
runReplM settings replm state =
  runInputT settings $ runMaybeT $ execStateT replm state

prompt :: String -> ReplM (Maybe String)
prompt = lift . lift . getInputLine

-- eODO does this need rewriting for externalPrint?
-- print :: (String -> IO ()) -> String -> ReplM ()
-- print pFn = liftIO . pFn
-- print pFn = lift . lift . pFn

--------------------------------
-- Module stuff (all in flux) --
--------------------------------

-- TODO replace current OrthoLangType with something like this:
-- TODO does eq make sense here?
-- data OrthoLangType = OrthoLangType
--   { tName :: String
--   , tExt  :: String
--   , tDesc :: String
--   }
--   deriving (Eq, Show, Read)

-- TODO should there be any more fundamental difference between fns and bops?
-- data OrthoLangFixity = Prefix | Infix
--   deriving (Eq, Show, Read)

data FnTag
  = Stochastic -- do repeat, do cache/share
  | ReadsDirs  -- do not repeat, do not cache/share
  | ReadsFile  -- do not repeat, do cache/share
  | ReadsURL   -- do not repeat, do not cache/share?
  | Broken     -- remove from functions list when loading
  | Hidden     -- remove from user-facing lists
  deriving (Eq, Read, Show)

-- TODO does eq make sense here? should i just be comparing names??
-- TODO pretty instance like "union: [set, set] -> set"? just "union" for now
data OrthoLangFunction = OrthoLangFunction
  { fName      :: String           -- ^ main (prefix) function name
  , fOpChar    :: Maybe Char       -- ^ infix operator symbol, if any
  , fTypeCheck :: TypeChecker      -- ^ checks input types, returning an error message or return type
  , fTypeDesc  :: String           -- ^ human-readable description
  , fTags      :: [FnTag]          -- ^ function tags (TODO implement these)
  , fOldRules  :: RulesFn          -- ^ old-style rules (TODO deprecate, then remove)
  , fNewRules  :: Maybe NewRulesFn -- ^ new-style rules (TODO write them all, then remove the Maybe)
  }
  -- , fHidden    :: Bool -- hide "internal" functions like reverse blast
  -- deriving (Eq, Read)

mkTypeDesc :: String -> [OrthoLangType] -> OrthoLangType -> String
mkTypeDesc n is o = unwords $ [n, ":"] ++ map extOf is ++ ["->", extOf o]

-- TODO does eq make sense here?
data OrthoLangModule = OrthoLangModule
  { mName      :: String
  , mDesc      :: String
  , mTypes     :: [OrthoLangType]
  , mFunctions :: [OrthoLangFunction]
  }
  -- deriving (Eq, Read)

-- TODO what about prettyShow in Pretty.hs?
instance Show OrthoLangModule where
  show = mName

-- TODO what if it's a function call?
-- do we have to make a rule that you can't use those?
-- (uuuugly! but not a show-stopper for now)
extractExprs :: OrthoLangScript -> OrthoLangExpr -> [OrthoLangExpr]
extractExprs  _  (OrthoLangList _ _ _ es) = es
extractExprs scr (OrthoLangRef  _ _ _ v ) = case lookup v scr of
                                        Nothing -> error $ "no such var " ++ show v
                                        Just e  -> extractExprs scr e
extractExprs _   (OrthoLangFun _ _ _ _ _) = error explainFnBug
extractExprs scr (OrthoLangBop _ _ _ _ l r) = extractExprs scr l ++ extractExprs scr r
extractExprs  _   e               = error $ "bad arg to extractExprs: " ++ show e

-- TODO any good way to avoid fromJust here?
exprDepsOf :: OrthoLangScript -> OrthoLangExpr -> [OrthoLangExpr]
exprDepsOf scr expr = map (\e -> fromJust $ lookup e scr) (depsOf expr)

-- needed to know what to still evaluate despite caching, so we can load seqid hashes
-- TODO rename to reflect that it's mostly about getting the functions which can't be cached
extractLoads :: OrthoLangScript -> OrthoLangExpr -> [OrthoLangExpr]
-- extractLoads s e = filter isLoad $ extractExprs s e
extractLoads s e = filter isLoad' $ exprDepsOf s e
  where
    isLoad' expr = let res = isLoad expr in trace "ortholang.core.types.extractLoads" ("isLoad '" ++ show expr ++ "'? " ++ show res) res
    isLoad (OrthoLangFun _ _ _ name _) = "load" `isPrefixOf` name || "glob" `isPrefixOf` name
    isLoad _ = False

-- TODO will this get printed, or will there just be a parse error?
explainFnBug :: String
explainFnBug =
  "You've stumbled on an outstanding bug. Sorry about that! \
  \The problem is that when doing transformations involving lists \
  \like repeat or map, OrthoLang can't \"see\" through future function calls; \
  \it can only manipulate lists whose elements are known *before* running the \
  \program. If you want Jeff to consider rewriting some things to fix that, \
  \drop him a line!"

-- this mostly checks equality, but also has to deal with how an empty list can
-- be any kind of list
-- TODO is there any more elegant way? this seems error-prone...
-- TODO is this the same as the Eq instance?
typeMatches :: OrthoLangType -> OrthoLangType -> Bool
typeMatches Empty _ = True
typeMatches _ Empty = True
typeMatches (ListOf   a) (ListOf   b) = typeMatches a b
typeMatches (ScoresOf a) (ScoresOf b) = typeMatches a b
typeMatches g1@(OrthoLangTypeGroup {}) g2@(OrthoLangTypeGroup {}) = g1 == g2
typeMatches a (OrthoLangTypeGroup {tgMember = fn}) = fn a
typeMatches (OrthoLangTypeGroup {tgMember = fn}) b = fn b
typeMatches a b = a == b

typesMatch :: [OrthoLangType] -> [OrthoLangType] -> Bool
typesMatch as bs = sameLength && allMatch
  where
    sameLength = length as == length bs
    allMatch   = all (\(a,b) -> a `typeMatches` b) (zip as bs)

nonEmptyType :: [OrthoLangType] -> Either String OrthoLangType
nonEmptyType ts = if typesOK then Right elemType else Left errorMsg
  where
    nonEmpty = filter isNonEmpty ts
    elemType = if      null ts       then Empty
               else if null nonEmpty then headOrDie "nonEmptyType failed" ts -- for example (ListOf Empty)
               else    headOrDie "nonEmptyType failed" nonEmpty
    typesOK  = all (typeMatches elemType) ts
    errorMsg = "all elements of a list must have the same type"

isNonEmpty :: OrthoLangType -> Bool
isNonEmpty Empty      = False
isNonEmpty (ListOf t) = isNonEmpty t
isNonEmpty _          = True
