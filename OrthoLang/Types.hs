{-# LANGUAGE DeriveDataTypeable #-}

module OrthoLang.Types
  (
  -- * Types and Newtypes
    Path(..)
  , Action1
  , Action2
  , Action3
  , ActionFn
  , RulesFn
  -- , typecheckFn
  -- , TypeChecker
  -- * Basic data structures
  , Assign
  , Expr(..)
  , CompiledExpr(..)
  , Config(..)
  , findType
  , findGroup
  , findEncoding
  , findModule
  , findFunction -- TODO remove?
  , findFun
  , listFunctions
  , listFunctionNames
  , operatorChars
  -- , WrapperConfig(..)
  , Ext(..)
  , Type(..)
  , Encoding(..)
  , TypeGroup(..)
  , TypeSig(..)
  , RepID(..)
  , Salt(..)
  , Var(..)
  , Script
  , emptyScript
  , emptyDigests
  , LocksRef
  , IDs(..)
  , emptyIDs
  , DigestMap
  , DigestsRef
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
  -- misc
  -- , prettyShow
  , str, num -- TODO load these from modules
  , lit
  , typeOf
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
  , typeSigMatches
  -- , typeSigsMatch
  -- new rules infrastructure
  , RulesEnv
  , RulesR
  , runRulesR
  , NewRules(..)
  -- , RulesR2
  -- , runRulesR2
  -- , ParseEnv
  , ReplM
  , runReplM
  , ReplCmd
  )
  where

import qualified Data.Map.Strict as M
-- import Text.Parsec (Parsec)

import Prelude hiding (error)
import OrthoLang.Debug
import OrthoLang.Locks (LocksRef, withReadLock)
import OrthoLang.Util  (readFileStrict, readFileLazy)

import Development.Shake              (Rules, Action, Resource)
-- import Control.Monad.IO.Class (liftIO)
import Data.Char                      (toLower)
import Data.List                      (nub, find, isPrefixOf)
-- import System.Console.Haskeline       (InputT, getInputLine, runInputT, Settings)
import Data.IORef                     (IORef)
import Data.Maybe (fromJust, catMaybes)

import Control.Monad.Reader -- TODO only specific imports
-- import Control.Monad.State.Lazy       -- (StateT, execStateT, lift)
import Data.Typeable hiding (typeOf)
import System.Console.Haskeline hiding (catch)
import Control.Monad.State.Strict (StateT, execStateT, lift, get, put)
import System.FilePath.Posix      ((</>))
import System.IO                  (Handle, hPutStrLn, stdout)

-- for ParseM (TODO clean up)
import Text.Parsec hiding (Empty)
import Control.Monad.Reader
import Control.Monad.Trans.Except

newtype Path = Path FilePath deriving (Eq, Ord, Show, Typeable)

-- Note that each ActionN takes N+1 Paths, because the first is the output
-- TODO take the output last instead?
type Action1 = Path -> Path -> Action ()
type Action2 = Path -> Path -> Path -> Action ()
type Action3 = Path -> Path -> Path -> Path -> Action ()

-- TODO remove when able in favor of well-typed versions above
type ActionFn    = Config -> CacheDir -> [ExprPath] -> Action ()

type RulesFn     = Script -> Expr -> Rules ExprPath
-- type TypeChecker = [Type] -> Either String Type

newtype CacheDir = CacheDir FilePath deriving (Read, Show, Eq, Typeable) -- ~/.ortholang/cache/<modname>
newtype ExprPath = ExprPath FilePath deriving (Read, Show, Eq, Typeable) -- ~/.ortholang/exprs/<fnname>/<hash>.<type>
newtype VarPath  = VarPath  FilePath deriving (Read, Show, Eq, Typeable) -- ~/.ortholang/vars/<varname>.<type>
newtype ResPath  = ResPath  FilePath deriving (Read, Show, Eq, Typeable) -- ~/.ortholang/vars/result[.<hash>.<type>]
newtype PathDigest = PathDigest String deriving (Read, Show, Eq, Ord, Typeable)

-- this isn't really needed, but makes passing globalenv around easier
-- TODO use it? or remove it
-- for now, same as GlobalEnv. but not sure if refs are needed
type RulesEnv = (Config, LocksRef, IDsRef, DigestsRef) -- TODO remove locks? ids? put script back?
type RulesR a = ReaderT RulesEnv Rules a

-- the problem with this is it seems like the digests map wouldn't
-- persist/share between invocations of runRulesR2, and we want all actions to
-- get the whole thing! better to pass it around like before, but via ioref for now
-- type RulesEnv2 = (Config, LocksRef, IDsRef)
-- type RulesR2 a = ReaderT RulesEnv2 (StateT DigestMap Rules) a

runRulesR :: RulesEnv -> RulesR a -> Rules a
runRulesR env act = runReaderT act env

-- runRulesR2 :: RulesEnv -> RulesR2 a -> Rules a
-- runRulesR2 env act = execStateT M.empty (runReaderT act env)
-- runRulesR2 env act = runReaderT env (execStateT emptyDigests act)
-- runRulesR2 env act = execStateT emptyDigests undefined -- (runReaderT act env)

-- runRulesR2 :: RulesEnv2 -> RulesR2 a -> Rules (a, DigestMap)
-- runRulesR2 env act = runStateT (runReaderT act env) emptyDigests -- (runReaderT act env)

-- Filename extension, which in OrthoLang is equivalent to variable type
-- TODO can this be done better with phantom types?
-- data Ext = ListOf Ext | Ext String
  -- deriving (Eq, Show, Read)

-- A digest identifying which replace_* call the variable is part of.
-- TODO This isn't very elegant; can it be removed?
newtype RepID = RepID (Maybe String) deriving (Eq, Show, Read, Typeable)

-- A number that can be incremented to change the expression's hash, causing repeat evaluation.
newtype Salt = Salt Int deriving (Eq, Show, Read, Typeable)

data Var = Var RepID String
  deriving (Eq, Show, Read, Typeable)

-- the common fields are:
-- * return type
-- * salt, which can be changed to force re-evaluation of an expr + all depends
--   (it should start at 0 and be incremented, though that doesn't really matter)
-- TODO start from 1 instead of 0?
-- TODO test that it works correctly! in particular, it should go thru refs!
--      (do we need to add salts of subepxressions or something? or use randoms?)
-- * list of dependencies (except lits don't have any)
data Expr
  = Lit Type String
  | Ref Type (Maybe Salt) [Var] Var -- do refs need a salt? yes! (i think?)
  | Bop Type (Maybe Salt) [Var] String  Expr Expr -- TODO remove salt?
  | Fun Type (Maybe Salt) [Var] String [Expr] -- TODO is the Eq instance wrong?
  | Lst Type [Var] [Expr] -- TODO maybe salt?
  | Com CompiledExpr -- wrapper around previously-compiled rules (see below)
  deriving (Eq, Show, Typeable)

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
  show (CompiledExpr t p _) = "CompiledExpr " ++ ext t ++ " " ++ show p ++ " <<Rules ExprPath>>"

-- CompiledExprs are compared by the expressions they were compiled from.
instance Eq CompiledExpr where
  (CompiledExpr _ p1 _) == (CompiledExpr _ p2 _) = p1 == p2

-- TODO is this not actually needed? seems "show expr" handles it?
saltOf :: Expr -> Maybe Salt
saltOf (Lit _ _)                = Nothing
saltOf (Lst _ _ _)              = Nothing -- TODO was the salt important?
saltOf (Com (CompiledExpr _ _ _)) = Nothing -- TODO this makes sense right?
saltOf (Ref _ ms _ _)     = ms
saltOf (Bop _ ms _ _ _ _) = ms
saltOf (Fun _ ms _ _ _)   = ms

-- TODO this needs to be recursive?
-- TODO would a recursive version be able to replace addPrefixes in ReplaceEach?
setSalt :: Int -> Expr -> Expr
setSalt r e@(Lit t s)     = e
setSalt r e@(Lst t ds es) = e
setSalt r (Ref t ms ds v)       = Ref  t (fmap (const $ Salt r) ms) ds v
setSalt r (Bop t ms ds s e1 e2) = Bop  t (fmap (const $ Salt r) ms) ds s e1 e2
setSalt r (Fun t ms ds s es)    = Fun  t (fmap (const $ Salt r) ms) ds s es
setSalt _ (Com (CompiledExpr _ _ _)) = error "setSalt" "not implemented for compiled rules" -- TODO should it be?

-- TODO add names to the Bops themselves... or associate with prefix versions?
prefixOf :: Expr -> String
prefixOf (Lit rtn _     ) = ext rtn
prefixOf (Fun _ _ _ name _) = name
prefixOf (Lst _ _ _    ) = "list"
prefixOf (Ref _ _ _ _     ) = error "prefixOf" "Refs don't need a prefix"
prefixOf (Com (CompiledExpr _ _ _)) = error "prefixOf" "CompiledExprs don't need a prefix"
prefixOf (Bop _ _ _ n _ _ ) = case n of
                                   "+" -> "add"
                                   "-" -> "subtract"
                                   "*" -> "multiply"
                                   "/" -> "divide"
                                   "|" -> "any"
                                   "&" -> "all"
                                   "~" -> "diff"
                                   x   -> error "prefixOf" $ "unknown Bop: \"" ++ x ++ "\""


-- TODO have a separate Assign for "result"?
type Assign = (Var, Expr)
type Script = [Assign]
  -- { sAssigns :: [Assign]  -- ^ Structured representation of the code suitable for printing, transforming, etc.
  -- , sDigests :: DigestMap -- ^ Map suitable for auto-discovery of dependencies from expression paths
  -- }

-- TODO is totally ignoring sDigests OK here?
-- instance Show Script where
  -- show scr = show scr

emptyScript :: Script
emptyScript = []

emptyDigests :: DigestMap
emptyDigests = M.empty

ensureResult :: Script -> Script
ensureResult as = if null as then noRes else scr'
  where
    resVar = Var (RepID Nothing) "result"
    noRes  = as ++ [(resVar, Lit str "no result")]
    scr'   = as ++ [(resVar, snd $ last as)               ]

lookupResult :: [(Var, b)] -> Maybe b
lookupResult scr = if null matches
  then (if null scr then Nothing else Just (snd $ last scr))
  else Just (snd $ last matches)
  where
    matches = filter (\(Var _ v, _) -> v == "result") scr

-- TODO tExt etc aren't well defined for the other constructors... is that a problem?
-- TODO how to make the record fields not partial functions?
-- TODO remove tShow and make a separate typeclass for "showable from filepath" so this can be Eq, Ord, ...
data Type
  = Empty -- ^ used in (ListOf Empty) to denote empty lists
  | ListOf    Type
  | ScoresOf  Type
  | EncodedAs Encoding Type
  | Type
    { tExt  :: String
    , tDesc :: String -- TODO include a longer help text too
    , tShow :: Config -> LocksRef -> FilePath -> IO String
    }
  -- deriving (Eq, Show, Read)

-- TODO is it dangerous to just assume they're the same by extension?
--      maybe we need to assert no duplicates while loading modules?
-- TODO should this use typeSigsMatch?
instance Eq Type where
  Empty              == Empty              = True
  (ListOf t1)        == (ListOf t2)        = t1 == t2
  (ScoresOf t1)      == (ScoresOf t2)      = t1 == t2
  (EncodedAs e1 t1)  == (EncodedAs e2 t2)  = e1 == e2 && t1 == t2
  (Type {tExt = e1}) == (Type {tExt = e2}) = e1 == e2
  _                  ==                  _ = False

-- TODO don't call this Show! maybe Pretty?
instance Show Type where
  show = ext

-- ^ tarballs, blast dbs, etc. where both format and wrapped type matter
-- TODO can it be unified with Type using typeclasses or something? redesign this part
data Encoding = Encoding
  { enExt :: String
  , enDesc :: String
  , enShow :: Config -> LocksRef -> FilePath -> IO String
  }

instance Show Encoding where
  show = enExt

instance Eq Encoding where
  (Encoding {enExt = e1}) == (Encoding {enExt = e2}) = e1 == e2

{-|
These are used to specify the input + output types of functions.
During parsing they are checked and used to determine the 'Type' for each 'Expr'.
Constructors are from vague to specific.
-}
data TypeSig

  -- these are analagous to their Type equivalents above:
  = ListSigs   TypeSig        -- ^ like ListOf with possibly ambiguous sigs inside
  | ScoresSigs TypeSig        -- ^ like ScoresOf with possibly ambiguous sigs inside
  | EncodedSig Encoding TypeSig -- ^ like EncodedAs with possibly ambiguous sigs inside

  -- these are new:
  | AnyType String        -- ^ generic placeholder. string used like in Some
  | Some TypeGroup String -- ^ the string is used for equality and in the help text
  | Exactly Type          -- ^ one regular Type wrapped for use in type signatures

  deriving (Eq, Show)

--instance Show TypeSig where
--  show AnyType = "AnyType"
--  show (Some g s) = "Some " ++ tgExt g ++ " " ++ show s
--  show (Exactly t) = "Exactly " ++ show t

{-|
These are kind of like simpler, less extensible typeclasses. They're just a
list of types that can be treated similarly in some circumstances, for example
"files whose length is their number of lines" or "FASTA files (faa or fna)".
-}
data TypeGroup = TypeGroup
  { tgExt   :: String
  , tgDesc  :: String
  , tgMembers :: [TypeSig]
  }
  deriving (Show)

-- TODO is it dangerous to just assume they're the same by extension?
--      maybe we need to assert no duplicates while loading modules?
instance Eq TypeGroup where
  (TypeGroup {tgExt = e1}) == (TypeGroup {tgExt = e2}) = e1 == e2

-- | types which have a file extension
class Ext a where
  ext  :: a -> String
  desc :: a -> String

-- note that traceShow in here can cause an infinite loop
-- and that there will be an issue if it's called on Empty alone
instance Ext Type where
  ext Empty             = "empty" -- special case for empty lists with no element type
  ext (ListOf        t) = ext t ++ ".list"
  ext (ScoresOf      t) = ext t ++ ".scores"
  ext (EncodedAs   e t) = ext t ++ "." ++ ext e
  ext (Type {tExt = e}) = e

  desc Empty           = "empty list" -- for lists with nothing in them yet
  desc (ListOf      t) = "list of " ++ desc t
  desc (ScoresOf    t) = "scores for " ++ desc t
  desc (EncodedAs e t) = desc t ++ " encoded as " ++ desc e
  desc (Type {tDesc = d}) = d

instance Ext TypeGroup where
  ext  = tgExt
  desc = tgDesc

instance Ext Encoding where
  ext  = enExt
  desc = enDesc

-- TODO either use this in the core compilers or remove it
lit :: TypeGroup
lit = TypeGroup
  { tgExt = "lit"
  , tgDesc = "basic literal (str or num)"
  , tgMembers = [Exactly str, Exactly num]
  }

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

typeOf :: Expr -> Type
typeOf (Lit   t _      ) = t
typeOf (Ref   t _ _ _    ) = t
typeOf (Bop   t _ _ _ _ _) = t
typeOf (Fun   t _ _ _ _  ) = t
typeOf (Lst  t _ _    ) = ListOf t
typeOf (Com (CompiledExpr t _ _)) = t
-- typeOf (Lst _ _ _ ts     ) = ListOf $ firstNonEmpty $ map typeOf ts
-- typeOf (Lst _ _ _ []     ) = Empty
-- typeOf (Lst _ _ _ []     ) = ListOf Empty

-- Works around a bug where if the first element is an empty list but others
-- have elements, it would call the whole thing an "emptylist.list".
-- Note no typechecking happens here; heterogenous lists won't be noticed.
-- firstNonEmpty :: [Expr] -> Type
-- firstNonEmpty    []  = Empty
-- firstNonEmpty (x:[]) = typeOf x -- catches (ListOf Empty)
-- firstNonEmpty (_:xs) = firstNonEmpty xs

-- ext :: Type -> String
-- ext Empty        = "empty" -- special case for empty lists with no element type
-- ext (ListOf   t) = ext t ++ ".list"
-- ext (ScoresOf t) = ext t ++ ".scores"
-- ext (EncodedAs e t) = ext t ++ "." ++ enExt e
-- ext (Type {tExt = e}) = e

-- TODO equivalent needed for type groups, right?
-- TODO is this needed for anything other than repl :help? if not, could use IO to load docs
-- desc :: Type -> String
-- desc Empty           = "empty list" -- for lists with nothing in them yet
-- desc (ListOf      t) = "list of " ++ desc t
-- desc (ScoresOf    t) = "scores for " ++ desc t
-- desc (EncodedAs e t) = desc t ++ " encoded as " ++ enDesc e
-- desc (Type {tDesc = d}) = d

varOf :: Expr -> [Var]
varOf (Ref _ _ _ v) = [v]
varOf _                = [ ]

depsOf :: Expr -> [Var]
depsOf (Lit  _ _         ) = []
depsOf (Ref  _ _ vs v      ) = v:vs -- TODO redundant?
depsOf (Bop  _ _ vs _ e1 e2) = nub $ vs ++ concatMap varOf [e1, e2]
depsOf (Fun  _ _ vs _ es   ) = nub $ vs ++ concatMap varOf es
depsOf (Lst _ vs   es   ) = nub $ vs ++ concatMap varOf es
depsOf (Com (CompiledExpr _ _ _)) = [] -- TODO should this be an error instead? their deps are accounted for

rDepsOf :: Script -> Var -> [Var]
rDepsOf scr var = map fst rDeps
  where
    rDeps = filter (\(_,e) -> isRDep e) scr
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
  , tDesc = "number in regular or scientific notation"
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

{-|
Options loaded from files + CLI, and edited in the REPL.
Field names Haskell naming conventions to simplify the config file.
They should be consistent the Docopts flags in docs/usage.txt
-}
data Config = Config
  { script      :: Maybe FilePath
  , interactive :: Bool
  , tmpdir      :: FilePath
  , workdir     :: FilePath
  , logfile     :: FilePath
  , debugregex  :: Maybe String
  , wrapper     :: Maybe FilePath
  , outfile     :: Maybe FilePath
  , shared      :: Maybe String -- can be a filepath or url
  , report      :: Maybe FilePath
  , history     :: Maybe FilePath
  , termcolumns :: Maybe Int -- for testing
  , shellaccess :: Bool
  , progressbar :: Bool
  , showhidden  :: Bool
  }
  deriving (Show, Typeable)

-- note: only lists the first name of each function,
--       which for binary operators will be the single-char one
listFunctionNames :: [Module] -> [String]
listFunctionNames mods = map fName $ concatMap mFunctions mods

findModule :: [Module] -> String -> Maybe Module
findModule mods name = find (\m -> map toLower (mName m) == map toLower name) mods

-- used by the compiler and repl
-- TODO find bops by char or name too
-- TODO filter to get a list and assert length == 1fs
-- TODO remove in favor of findFun below?
findFunction :: [Module] -> String -> Maybe Function
findFunction mods name = find (\f -> map toLower (fName f) == n
                      || fmap (\c -> [toLower c]) (fOpChar f) == Just n) fs
  where
    n = map toLower name
    fs = concatMap mFunctions mods

findFun :: [Module] -> String -> Either String Function
findFun mods name =
  let n   = map toLower name
      fns = concat $ map mFunctions mods
  in case filter (\f -> map toLower (fName f) == n) fns of
       []     -> Left $ "no function found with name \"" ++ name ++ "\""
       (f:[]) -> Right f
       _      -> Left $ "function name collision! multiple fns match \"" ++ name ++ "\""

findType :: [Module] -> String -> Maybe Type
findType mods e = find (\t -> ext t == e') ts
  where
    e' = map toLower e
    ts = concatMap mTypes mods

findGroup :: [Module] -> String -> Maybe TypeGroup
findGroup mods e = find (\g -> ext g == e') ts
  where
    e' = map toLower e
    ts = concatMap mGroups mods

findEncoding :: [Module] -> String -> Maybe Encoding
findEncoding mods e = find (\g -> ext g == e') ts
  where
    e' = map toLower e
    ts = concatMap mEncodings mods

listFunctions :: [Module] -> [Function]
listFunctions mods = concatMap mFunctions mods

-- Now with guard against accidentally including parts of prefix fn names!
operatorChars :: [Module] -> [Char]
operatorChars mods = catMaybes $ map fOpChar $ listFunctions mods

-----------------
-- Parse monad --
-----------------

type ParseM a = ParsecT String Script (ReaderT [Module] (Except String)) a

-- we sanitize the input fasta files to prevent various bugs,
-- then use this hash -> seqid map to put the original ids back at the end
-- hFiles is for making paths generic
-- hSeqIDs is the main one, and stores hash -> seqid maps indexed by their (generic) hFiles path
-- hSeqHashes is for reverse lookups of the seqid_... hash from actual seqid (for example from gene lists)
-- hExprs is for decoding exprs/<hash>/<hash>/... paths
-- TODO use bytestring-tries rather than maps with string keys?
-- TODO should this be ActionIDs in general? aka all the stuff that might be needed in Action
data IDs = IDs
  { hFiles  :: M.Map String String
  , hSeqIDs :: M.Map String (M.Map String String)
  -- , hSeqHashes :: M.Map String (M.Map String String)
  -- , hExprs  :: DigestMap
  }
  deriving (Show)

emptyIDs :: IDs
emptyIDs = IDs M.empty M.empty

-- this lets me cheat and not bother threading the ID map through all the monad stuff
-- TODO go back and do it right
type IDsRef = IORef IDs

type DigestMap = M.Map PathDigest (Type, Path)
type DigestsRef = IORef DigestMap

-- TODO config always first? or make a record + ask* fns
type GlobalEnv = (Script, Config, LocksRef, IDsRef, DigestsRef)

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
  | ReadsFile  -- do not repeat, do cache/share TODO ReadsFiles
  | ReadsURL   -- do not repeat, do not cache/share?
  | Broken     -- remove from functions list when loading
  | Hidden     -- remove from user-facing lists
  deriving (Eq, Read, Show, Typeable)

-- TODO does eq make sense here? should i just be comparing names??
-- TODO pretty instance like "union: [set, set] -> set"? just "union" for now
data Function = Function
  { fName      :: String      -- ^ main (prefix) function name
  , fOpChar    :: Maybe Char  -- ^ infix operator symbol, if any

  -- TODO remove these
  -- , fTypeCheck :: TypeChecker -- ^ checks input types, returning an error message or return type
  -- , fTypeDesc  :: String      -- ^ human-readable description

  -- TODO write these, then remove the old descs + typecheckers above
  , fInputs :: [TypeSig] -- ^ new input (argument) types TODO any way to make it a variable length tuple?
  , fOutput ::  TypeSig  -- ^ new output (return) type

  , fTags      :: [FnTag]     -- ^ function tags (TODO implement these)
  , fOldRules  :: RulesFn     -- ^ old-style rules (TODO deprecate, then remove)
  , fNewRules  :: NewRules    -- ^ new-style rules (TODO write them all, then remove the Maybe)
  }
  -- , fHidden    :: Bool -- hide "internal" functions like reverse blast
  -- deriving (Eq, Read)

-- TODO is this safe enough?
instance Eq Function where
  f1 == f2 = fName   f1 == fName   f2
          && fOpChar f1 == fOpChar f2
          && fInputs f1 == fInputs f2
          && fOutput f1 == fOutput f2
          && fTags   f1 == fTags   f2

data NewRules
  = NewRules (Rules ())
  | NewMacro (Script -> Expr -> Expr) -- type alias in NewRules.hs for now
  | NewNotImplemented -- TODO remove

mkTypeDesc :: String -> [Type] -> Type -> String
mkTypeDesc n is o = unwords $ [n, ":"] ++ map ext is ++ ["->", ext o]

-- TODO does eq make sense here?
data Module = Module
  { mName      :: String
  , mDesc      :: String
  , mTypes     :: [Type]
  , mGroups    :: [TypeGroup]
  , mEncodings :: [Encoding]
  , mFunctions :: [Function]
  }
  -- deriving (Eq, Read)
  deriving (Typeable)

-- TODO what about prettyShow in Pretty.hs?
instance Show Module where
  show = mName

-- TODO what if it's a function call?
-- do we have to make a rule that you can't use those?
-- (uuuugly! but not a show-stopper for now)
extractExprs :: Script -> Expr -> [Expr]
extractExprs  _  (Lst _ _ es) = es
extractExprs scr (Ref _ _ _ v ) = case lookup v scr of
                                       Nothing -> error "extractExprs" $ "no such var " ++ show v
                                       Just e  -> extractExprs scr e
extractExprs _   (Fun _ _ _ _ _) = error "extractExprs" explainFnBug
extractExprs scr (Bop _ _ _ _ l r) = extractExprs scr l ++ extractExprs scr r
extractExprs  _   e               = error "extractExprs" $ "bad arg: " ++ show e

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
-- note: this function isn't associative! expected types on left, actual types on right
-- TODO do we still need an empty list case here?
typeSigMatches :: TypeSig -> Type -> Bool
typeSigMatches (AnyType _)       _                = True
typeSigMatches (Exactly (ListOf _)) (ListOf Empty) = True
typeSigMatches (Exactly t1)      t2               = t1 == t2
typeSigMatches (ListSigs _)      (ListOf Empty)   = True
typeSigMatches (ListSigs s)      (ListOf t)       = typeSigMatches s t
typeSigMatches (ScoresSigs s)    (ScoresOf t)     = typeSigMatches s t
typeSigMatches (EncodedSig e1 s) (EncodedAs e2 t) = e1 == e2 && typeSigMatches s t
typeSigMatches (ListSigs _)      _                = False
typeSigMatches (ScoresSigs _)    _                = False
typeSigMatches (EncodedSig _ _)  _                = False
typeSigMatches (Some g _)        t                = any (\s -> typeSigMatches s t) (tgMembers g)

-- TODO remove?
-- typeSigsMatch :: [TypeSig] -> [Type] -> Bool
-- typeSigsMatch as bs = sameLength && allMatch
--   where
--     sameLength = length as == length bs
--     allMatch   = all (\(s,t) -> typeSigMatches s t) (zip as bs)

-----------------
-- Repl monad --
----------------

type ReplM = StateT GlobalEnv IO

runReplM :: Settings ReplM -> InputT ReplM (Maybe String) -> GlobalEnv -> IO ()
runReplM settings myLoop st@(_, cfg, _, _, _) = do
  _ <- execStateT (runInputT settings myLoop) st
  return ()

type ReplCmd = [Module] -> GlobalEnv -> Handle -> String -> IO GlobalEnv
