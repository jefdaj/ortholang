module OrthoLang.Core.Types
  (
  -- * Types and Newtypes
    Path(..)
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
  , Assign
  , Expr(..)
  , CompiledExpr(..)
  , Config(..)
  , findType
  , findModule
  , findFunction
  , listFunctions
  , listFunctionNames
  , operatorChars
  -- , WrapperConfig(..)
  , Type(..)
  , RepID(..)
  , Salt(..)
  , Var(..)
  , Script(..)
  , emptyScript
  , emptyDigests
  , LocksRef
  , IDs(..)
  , emptyIDs
  , DigestMap
  , IDsRef
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
  , Function(..)
  , FnTag(..)
  , mkTypeDesc
  , Module(..)
  , saltOf
  , setSalt
  , prefixOf
  -- * Wrappers to prevent confusing the various paths
  , CacheDir(..)
  , ExprPath(..)
  , VarPath(..)
  , ResPath(..)
  , PathDigest(..)
  -- * Misc experimental stuff
  , extractExprs
  , extractLoads
  , typeMatches
  , typesMatch
  , nonEmptyType
  , isNonEmpty
  -- new rules infrastructure
  , ActionEnv
  , ActionR
  , ActionR1
  , ActionR2
  , ActionR3
  , runActionR
  , RulesEnv
  , RulesR
  , runRulesR
  , ParseEnv
  )
  where

-- import Prelude hiding (print)
import qualified Data.Map.Strict as M
import Text.Parsec (Parsec)

import OrthoLang.Core.Locks (LocksRef, withReadLock)
import OrthoLang.Core.Util  (readFileStrict, readFileLazy, headOrDie, trace)

import Development.Shake              (Rules, Action, Resource)
-- import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy       (StateT, execStateT, lift)
import Control.Monad.Trans.Maybe      (MaybeT(..), runMaybeT)
import Data.List                      (nub, find, isPrefixOf)
import System.Console.Haskeline       (InputT, getInputLine, runInputT, Settings)
import Data.IORef                     (IORef)
import Data.Maybe (fromJust, catMaybes)

import Control.Monad.Reader -- TODO only specific imports

newtype Path = Path FilePath deriving (Eq, Ord, Show)

-- Note that each ActionN takes N+1 Paths, because the first is the output
-- TODO take the output last instead?
type Action1 = Config -> LocksRef -> IDsRef -> Path -> Path -> Action ()
type Action2 = Config -> LocksRef -> IDsRef -> Path -> Path -> Path -> Action ()
type Action3 = Config -> LocksRef -> IDsRef -> Path -> Path -> Path -> Path -> Action ()

-- TODO remove when able in favor of well-typed versions above
type ActionFn    = Config -> CacheDir -> [ExprPath] -> Action ()

type RulesFn     = GlobalEnv -> Expr -> Rules ExprPath
type TypeChecker = [Type] -> Either String Type

type NewAction1 = Config -> LocksRef -> IDsRef -> ExprPath -> FilePath                         -> Action ()
type NewAction2 = Config -> LocksRef -> IDsRef -> ExprPath -> FilePath -> FilePath             -> Action ()
type NewAction3 = Config -> LocksRef -> IDsRef -> ExprPath -> FilePath -> FilePath -> FilePath -> Action ()

type NewRulesFn = Config -> LocksRef -> IDsRef -> Rules ()

newtype CacheDir = CacheDir FilePath deriving (Read, Show, Eq) -- ~/.ortholang/cache/<modname>
newtype ExprPath = ExprPath FilePath deriving (Read, Show, Eq) -- ~/.ortholang/exprs/<fnname>/<hash>.<type>
newtype VarPath  = VarPath  FilePath deriving (Read, Show, Eq) -- ~/.ortholang/vars/<varname>.<type>
newtype ResPath  = ResPath  FilePath deriving (Read, Show, Eq) -- ~/.ortholang/vars/result[.<hash>.<type>]
newtype PathDigest = PathDigest String deriving (Read, Show, Eq, Ord)

-----------------------------------------------
-- experimental: add state to Rules + Action --
-----------------------------------------------

-- this is needed to pass DigestMap around, and makes the rest easier
type ActionEnv = (Config, LocksRef, IDsRef, DigestMap)
type ActionR a = ReaderT ActionEnv Action a

runActionR :: ActionEnv -> ActionR a -> Action a
runActionR env act = runReaderT act env

type ActionR1 = ExprPath -> FilePath                         -> ActionR ()
type ActionR2 = ExprPath -> FilePath -> FilePath             -> ActionR ()
type ActionR3 = ExprPath -> FilePath -> FilePath -> FilePath -> ActionR ()

-- this isn't really needed, but makes passing globalenv around easier
-- TODO use it? or remove it
-- for now, same as GlobalEnv. but not sure if refs are needed
type RulesEnv = (Config, LocksRef, IDsRef, DigestMap) -- TODO remove locks? ids? put script back?
type RulesR a = ReaderT RulesEnv Rules a -- TODO is this right?

runRulesR :: RulesEnv -> RulesR a -> Rules a
runRulesR env act = runReaderT act env

-- Filename extension, which in OrthoLang is equivalent to variable type
-- TODO can this be done better with phantom types?
-- data Ext = ListOf Ext | Ext String
  -- deriving (Eq, Show, Read)

-- A digest identifying which replace_* call the variable is part of.
-- TODO This isn't very elegant; can it be removed?
newtype RepID = RepID (Maybe String) deriving (Eq, Show, Read)

-- A number that can be incremented to change the expression's hash, causing repeat evaluation.
newtype Salt = Salt Int deriving (Eq, Show, Read)

data Var = Var RepID String
  deriving (Eq, Show, Read)
 
-- the common fields are:
-- * return type
-- * salt, which can be changed to force re-evaluation of an expr + all depends
--   (it should start at 0 and be incremented, though that doesn't really matter)
-- TODO start from 1 instead of 0?
-- TODO test that it works correctly! in particular, it should go thru refs!
--      (do we need to add salts of subepxressions or something? or use randoms?)
-- * list of dependencies (except lits don't have any)
data Expr
  = Lit Type Salt String
  | Ref Type Salt [Var] Var -- do refs need a salt? yes! (i think?)
  | Bop Type Salt [Var] String  Expr Expr
  | Fun Type Salt [Var] String [Expr]
  | Lst Type Salt [Var] [Expr]
  | Com CompiledExpr -- wrapper around previously-compiled rules (see below)
  deriving (Eq, Show)

-- An expression that has already been compiled to Rules, wrapped so it can be
-- passed to another function. Because Rules can't be shown or compared, we
-- also carry around the original Expr. TODO is the expr necessary? helpful?
-- The CompiledExpr constructor is just here so we can customize the Show and Eq instances.
-- The extra ExprPath is weird, but seems to be required since we can't get at the second one.
data CompiledExpr = CompiledExpr Type ExprPath (Rules ExprPath)

-- TODO is it a bad idea to hide the compiled-ness?
-- TODO can this be made into a Path?
-- TODO is show ever really needed?
instance Show CompiledExpr where
  show (CompiledExpr t p _) = "CompiledExpr " ++ extOf t ++ " " ++ show p ++ " <<Rules ExprPath>>"

-- CompiledExprs are compared by the expressions they were compiled from.
instance Eq CompiledExpr where
  (CompiledExpr _ p1 _) == (CompiledExpr _ p2 _) = p1 == p2

-- TODO is this not actually needed? seems "show expr" handles it?
saltOf :: Expr -> Salt
saltOf (Lit _ n _)       = n
saltOf (Ref _ n _ _)     = n
saltOf (Bop _ n _ _ _ _) = n
saltOf (Fun _ n _ _ _)   = n
saltOf (Lst _ n _ _)     = n
saltOf (Com (CompiledExpr _ _ _)) = error "CompiledExprs don't have salts" -- TODO is that OK?

-- TODO this needs to be recursive?
-- TODO would a recursive version be able to replace addPrefixes in ReplaceEach?
setSalt :: Int -> Expr -> Expr
setSalt r (Lit t _ s)          = Lit  t (Salt r) s
setSalt r (Ref t _ ds v)       = Ref  t (Salt r) ds v
setSalt r (Bop t _ ds s e1 e2) = Bop  t (Salt r) ds s e1 e2
setSalt r (Fun t _ ds s es)    = Fun  t (Salt r) ds s es
setSalt r (Lst t _ ds es)     = Lst t (Salt r) ds es
setSalt _ (Com (CompiledExpr _ _ _)) = error "setSalt not implemented for compiled rules" -- TODO should it be?

-- TODO add names to the Bops themselves... or associate with prefix versions?
prefixOf :: Expr -> String
prefixOf (Lit rtn _ _     ) = extOf rtn
prefixOf (Fun _ _ _ name _) = name
prefixOf (Lst _ _ _ _    ) = "list"
prefixOf (Ref _ _ _ _     ) = error  "Refs don't need a prefix"
prefixOf (Com (CompiledExpr _ _ _)) = error "CompiledExprs don't need a prefix"
prefixOf (Bop _ _ _ n _ _ ) = case n of
                                   "+" -> "add"
                                   "-" -> "subtract"
                                   "*" -> "multiply"
                                   "/" -> "divide"
                                   "|" -> "any"
                                   "&" -> "all"
                                   "~" -> "diff"
                                   x   -> error $ "unknown Bop: '" ++ x ++ "'"


-- TODO have a separate Assign for "result"?
type Assign = (Var, Expr)
data Script = Script
  { sAssigns :: [Assign]  -- ^ Structured representation of the code suitable for printing, transforming, etc.
  , sDigests :: DigestMap -- ^ Map suitable for auto-discovery of dependencies from expression paths
  }

-- TODO is totally ignoring sDigests OK here?
instance Show Script where
  show scr = show (sAssigns scr)

emptyScript :: Script
emptyScript = Script {sAssigns = [], sDigests = M.empty}

emptyDigests :: DigestMap
emptyDigests = M.empty

ensureResult :: Script -> Script
ensureResult scr@(Script {sAssigns = as}) = if null as then noRes else scr'
  where
    resVar = Var (RepID Nothing) "result"
    noRes  = scr {sAssigns = as ++ [(resVar, Lit str (Salt 0) "no result")]}
    scr'   = scr {sAssigns = as ++ [(resVar, snd $ last as)               ]}

lookupResult :: [(Var, b)] -> Maybe b
lookupResult scr = if null matches
  then (if null scr then Nothing else Just (snd $ last scr))
  else Just (snd $ last matches)
  where
    matches = filter (\(Var _ v, _) -> v == "result") scr

-- TODO tExt etc aren't well defined for the other constructors... is that a problem?
-- TODO how to make the record fields not partial functions?
data Type
  = Empty -- TODO remove this? should never be a need to define an empty list
  | ListOf Type
  | ScoresOf Type

  {- These are kind of like simpler, less extensible typeclasses. They're just
   - a list of types that can be treated similarly in some circumstances, for
   - example "files whose length is their number of lines" or "FASTA files (faa
   - or fna)".
   -}
  | TypeGroup
    { tgExt   :: String
    , tgDesc  :: String
    -- , tgShow  :: Config -> LocksRef -> FilePath -> IO String -- TODO remove?
    , tgMember :: Type -> Bool
    }

  | Type
    { tExt  :: String
    , tDesc :: String -- TODO include a longer help text too
    , tShow :: Config -> LocksRef -> FilePath -> IO String
    }
  -- deriving (Eq, Show, Read)

defaultShow :: Config -> LocksRef -> FilePath -> IO String
defaultShow = defaultShowN 5

defaultShowN :: Int -> Config -> LocksRef -> FilePath -> IO String
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
instance Eq Type where
  Empty        == Empty        = True
  (ListOf a)   == (ListOf b)   = a == b
  (ScoresOf a) == (ScoresOf b) = a == b
  (Type {tExt = t1}) == (Type {tExt = t2}) = t1 == t2
  (TypeGroup {tgExt = t1}) == (TypeGroup {tgExt = t2}) = t1 == t2
  (TypeGroup {tgMember = fn}) == t = fn t
  t == (TypeGroup {tgMember = fn}) = fn t
  _ == _ = False -- TODO should this behave differently?

instance Show Type where
  show = extOf

typeOf :: Expr -> Type
typeOf (Lit   t _ _      ) = t
typeOf (Ref   t _ _ _    ) = t
typeOf (Bop   t _ _ _ _ _) = t
typeOf (Fun   t _ _ _ _  ) = t
typeOf (Lst  t _ _ _    ) = ListOf t -- t can be Empty
typeOf (Com (CompiledExpr t _ _)) = t
-- typeOf (Lst _ _ _ ts     ) = ListOf $ nonEmptyType $ map typeOf ts
-- typeOf (Lst _ _ _ []     ) = Empty
-- typeOf (Lst _ _ _ []     ) = ListOf Empty

-- Works around a bug where if the first element is an empty list but others
-- have elements, it would call the whole thing an "emptylist.list".
-- Note no typechecking happens here; heterogenous lists won't be noticed.
-- nonEmptyType :: [Expr] -> Type
-- nonEmptyType    []  = Empty
-- nonEmptyType (x:[]) = typeOf x -- catches (ListOf Empty)
-- nonEmptyType (_:xs) = nonEmptyType xs

-- note that traceShow in here can cause an infinite loop
-- and that there will be an issue if it's called on Empty alone
extOf :: Type -> String
extOf Empty                      = "empty" -- for lists with nothing in them yet
extOf (ListOf                t ) = extOf t ++ ".list"
extOf (ScoresOf              t ) = extOf t ++ ".scores"
extOf (TypeGroup {tgExt = t}) = t -- TODO should this not be called an extension? it's never written to disk
                                     -- TODO put back the < > ?
extOf (Type      { tExt = t}) = t

-- TODO is this needed for anything other than repl :help? if not, could use IO to load docs
descOf :: Type -> String
descOf Empty         = "empty list" -- for lists with nothing in them yet
descOf (ListOf   t ) = "list of " ++ descOf t
descOf (ScoresOf t ) = "scores for " ++ descOf t
descOf (TypeGroup {tgDesc = t}) = t
descOf (Type      { tDesc = t}) = t

varOf :: Expr -> [Var]
varOf (Ref _ _ _ v) = [v]
varOf _                = [ ]

depsOf :: Expr -> [Var]
depsOf (Lit  _ _ _         ) = []
depsOf (Ref  _ _ vs v      ) = v:vs -- TODO redundant?
depsOf (Bop  _ _ vs _ e1 e2) = nub $ vs ++ concatMap varOf [e1, e2]
depsOf (Fun  _ _ vs _ es   ) = nub $ vs ++ concatMap varOf es
depsOf (Lst _ _ vs   es   ) = nub $ vs ++ concatMap varOf es
depsOf (Com (CompiledExpr _ _ _)) = [] -- TODO should this be an error instead? their deps are accounted for

rDepsOf :: Script -> Var -> [Var]
rDepsOf scr var = map fst rDeps
  where
    rDeps = filter (\(_,e) -> isRDep e) (sAssigns scr)
    isRDep expr = elem var $ depsOf expr

-- TODO move to modules as soon as parsing works again
-- TODO keep literals in the core along with refs and stuff? seems reasonable
-- TODO how about lists/sets, are those core too?

str :: Type
str = Type
  { tExt  = "str"
  , tDesc = "string"
  -- TODO make one of the read functions be IO for this instead
  , tShow = \_ ls f -> do
      -- putStrLn $ "reading " ++ f
      txt <- fmap init $ withReadLock ls f $ readFileStrict ls f
      let txt' = if txt == "<<emptystr>>" then "" else txt
      return $ "\"" ++ txt' ++ "\""
  }

num :: Type
num = Type
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
-- TODO remove anything non-printable
-- TODO read defaults from a config file
data Config = Config
  { cfgScript  :: Maybe FilePath
  , cfgInteractive :: Bool
  , cfgTmpDir  :: FilePath
  , cfgWorkDir :: FilePath
  , cfgDebug   :: Maybe String
  , cfgModules :: [Module]
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
listFunctionNames :: Config -> [String]
listFunctionNames cfg = map fName $ concat $ map mFunctions $ cfgModules cfg

findModule :: Config -> String -> Maybe Module
findModule cfg name = find (\m -> mName m == name) (cfgModules cfg)

-- used by the compiler and repl
-- TODO find bops by char or name too
-- TODO filter to get a list and assert length == 1fs
findFunction :: Config -> String -> Maybe Function
findFunction cfg name = find (\f -> fName f == name || fmap (\c -> [c]) (fOpChar f) == Just name) fs
  where
    ms = cfgModules cfg
    fs = concatMap mFunctions ms

findType :: Config -> String -> Maybe Type
findType cfg ext = find (\t -> extOf t == ext) ts
  where
    ms = cfgModules cfg
    ts = concatMap mTypes ms

listFunctions :: Config -> [Function]
listFunctions cfg = concatMap mFunctions $ cfgModules cfg

-- Now with guard against accidentally including parts of prefix fn names!
operatorChars :: Config -> [Char]
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
data IDs = IDs
  { hFiles  :: M.Map String String
  , hSeqIDs :: M.Map String (M.Map String String)
  -- , hExprs  :: DigestMap
  }

emptyIDs :: IDs
emptyIDs = IDs M.empty M.empty

-- this lets me cheat and not bother threading the ID map through all the monad stuff
-- TODO go back and do it right
type IDsRef = IORef IDs

type DigestMap = M.Map PathDigest (Type, Path)

-- TODO config always first? or make a record + ask* fns
type GlobalEnv = (Script, Config, LocksRef, IDsRef)

-- TODO config always first? or make a record + ask* fns
type ParseEnv = (Config, Script) -- script also contains the digestmap now
type ParseM a = Parsec String ParseEnv a

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

-- TODO replace current Type with something like this:
-- TODO does eq make sense here?
-- data Type = Type
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
data Function = Function
  { fName      :: String           -- ^ main (prefix) function name
  , fOpChar    :: Maybe Char       -- ^ infix operator symbol, if any
  , fTypeCheck :: TypeChecker      -- ^ checks input types, returning an error message or return type
  , fTypeDesc  :: String           -- ^ human-readable description
  , fTags      :: [FnTag]          -- ^ function tags (TODO implement these)
  , fOldRules  :: RulesFn          -- ^ old-style rules (TODO deprecate, then remove)
  , fNewRules  :: Maybe (RulesR ()) -- ^ new-style rules (TODO write them all, then remove the Maybe)
  }
  -- , fHidden    :: Bool -- hide "internal" functions like reverse blast
  -- deriving (Eq, Read)

mkTypeDesc :: String -> [Type] -> Type -> String
mkTypeDesc n is o = unwords $ [n, ":"] ++ map extOf is ++ ["->", extOf o]

-- TODO does eq make sense here?
data Module = Module
  { mName      :: String
  , mDesc      :: String
  , mTypes     :: [Type]
  , mFunctions :: [Function]
  }
  -- deriving (Eq, Read)

-- TODO what about prettyShow in Pretty.hs?
instance Show Module where
  show = mName

-- TODO what if it's a function call?
-- do we have to make a rule that you can't use those?
-- (uuuugly! but not a show-stopper for now)
extractExprs :: Script -> Expr -> [Expr]
extractExprs  _  (Lst _ _ _ es) = es
extractExprs scr (Ref  _ _ _ v ) = case lookup v (sAssigns scr) of
                                        Nothing -> error $ "no such var " ++ show v
                                        Just e  -> extractExprs scr e
extractExprs _   (Fun _ _ _ _ _) = error explainFnBug
extractExprs scr (Bop _ _ _ _ l r) = extractExprs scr l ++ extractExprs scr r
extractExprs  _   e               = error $ "bad arg to extractExprs: " ++ show e

-- TODO any good way to avoid fromJust here?
exprDepsOf :: Script -> Expr -> [Expr]
exprDepsOf scr expr = map (\e -> fromJust $ lookup e $ sAssigns scr) (depsOf expr)

-- needed to know what to still evaluate despite caching, so we can load seqid hashes
-- TODO rename to reflect that it's mostly about getting the functions which can't be cached
extractLoads :: Script -> Expr -> [Expr]
-- extractLoads s e = filter isLoad $ extractExprs s e
extractLoads s e = filter isLoad' $ exprDepsOf s e
  where
    isLoad' expr = let res = isLoad expr in trace "ortholang.core.types.extractLoads" ("isLoad '" ++ show expr ++ "'? " ++ show res) res
    isLoad (Fun _ _ _ name _) = "load" `isPrefixOf` name || "glob" `isPrefixOf` name
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
typeMatches :: Type -> Type -> Bool
typeMatches Empty _ = True
typeMatches _ Empty = True
typeMatches (ListOf   a) (ListOf   b) = typeMatches a b
typeMatches (ScoresOf a) (ScoresOf b) = typeMatches a b
typeMatches g1@(TypeGroup {}) g2@(TypeGroup {}) = g1 == g2
typeMatches a (TypeGroup {tgMember = fn}) = fn a
typeMatches (TypeGroup {tgMember = fn}) b = fn b
typeMatches a b = a == b

typesMatch :: [Type] -> [Type] -> Bool
typesMatch as bs = sameLength && allMatch
  where
    sameLength = length as == length bs
    allMatch   = all (\(a,b) -> a `typeMatches` b) (zip as bs)

nonEmptyType :: [Type] -> Either String Type
nonEmptyType ts = if typesOK then Right elemType else Left errorMsg
  where
    nonEmpty = filter isNonEmpty ts
    elemType = if      null ts       then Empty
               else if null nonEmpty then headOrDie "nonEmptyType failed" ts -- for example (ListOf Empty)
               else    headOrDie "nonEmptyType failed" nonEmpty
    typesOK  = all (typeMatches elemType) ts
    errorMsg = "all elements of a list must have the same type"

isNonEmpty :: Type -> Bool
isNonEmpty Empty      = False
isNonEmpty (ListOf t) = isNonEmpty t
isNonEmpty _          = True
