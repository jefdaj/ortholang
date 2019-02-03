module Detourrr.Core.Types
  -- type aliases and newtypes
  ( RrrPath(..)
  , Action1
  , Action2
  , Action3
  , ActionFn
  , RulesFn
  , TypeChecker
  -- data structures
  , RrrAssign
  , RrrExpr(..)
  , CompiledExpr(..)
  , RrrConfig(..)
  , findType
  , findFunction
  , listFunctions
  , listFunctionNames
  , operatorChars
  -- , WrapperConfig(..)
  , RrrType(..)
  , RrrVar(..)
  , RrrScript
  , Locks
  , HashedSeqIDs
  , HashedSeqIDsRef
  , RrrState
  -- , Assoc(..) -- we reuse this from Parsec
  , RrrFixity(..)
  -- parse monad
  , ParseM
  , runParseM
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
  , depsOf
  , rDepsOf
  , defaultShow
  -- module stuff (in flux)
  , RrrFunction(..)
  , mkTypeDesc
  , RrrModule(..)
  , saltOf
  , setSalt
  , prefixOf
  -- wrappers to prevent confusing the various paths
  , CacheDir(..)
  , ExprPath(..)
  , VarPath(..)
  , ResPath(..)
  -- misc experimental stuff
  , extractExprs
  , typeMatches
  , typesMatch
  , nonEmptyType
  , isNonEmpty
  )
  where

-- import Prelude hiding (print)
import qualified Data.Map    as M
import qualified Text.Parsec as P

import Detourrr.Core.Locks (Locks, withReadLock)
import Detourrr.Core.Util  (readFileStrict, readFileLazy)

import Development.Shake              (Rules, Action, Resource)
import Control.Monad.State.Lazy       (StateT, execStateT, lift)
import Control.Monad.Trans.Maybe      (MaybeT(..), runMaybeT)
import Data.List                      (nub, find)
import System.Console.Haskeline       (InputT, getInputLine, runInputT, Settings)
import Text.Parsec                    (ParseError)
import Development.Shake.FilePath (makeRelative)
import Data.IORef                     (IORef)
-- import Text.PrettyPrint.HughesPJClass (Doc, text, doubleQuotes)

import Debug.Trace

newtype RrrPath = RrrPath FilePath deriving (Eq, Ord, Show)

-- Note that each ActionN takes N+1 RrrPaths, because the first is the output
-- TODO take the output last instead?
type Action1 = RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> RrrPath -> Action ()
type Action2 = RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> RrrPath -> RrrPath -> Action ()
type Action3 = RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> RrrPath -> RrrPath -> RrrPath -> Action ()

-- TODO remove when able in favor of well-typed versions above
type ActionFn    = RrrConfig -> CacheDir -> [ExprPath] -> Action ()

type RulesFn     = RrrState -> RrrExpr -> Rules ExprPath
type TypeChecker = [RrrType] -> Either String RrrType

newtype CacheDir = CacheDir FilePath deriving Show -- ~/.detourrr/cache/<modname>
newtype ExprPath = ExprPath FilePath deriving Show -- ~/.detourrr/exprs/<fnname>/<hash>.<type>
newtype VarPath  = VarPath  FilePath deriving Show -- ~/.detourrr/vars/<varname>.<type>
newtype ResPath  = ResPath  FilePath deriving Show -- ~/.detourrr/vars/result[.<hash>.<type>]

-- Filename extension, which in Detourrr is equivalent to variable type
-- TODO can this be done better with phantom types?
-- data Ext = ListOf Ext | Ext String
  -- deriving (Eq, Show, Read)

newtype RrrVar = RrrVar String deriving (Eq, Show, Read)
 
-- the common fields are:
-- * return type
-- * salt, which can be changed to force re-evaluation of an expr + all depends
--   (it should start at 0 and be incremented, though that doesn't really matter)
-- TODO start from 1 instead of 0?
-- TODO test that it works correctly! in particular, it should go thru refs!
--      (do we need to add salts of subepxressions or something? or use randoms?)
-- * list of dependencies (except lits don't have any)
data RrrExpr
  = RrrLit RrrType Int String
  | RrrRef RrrType Int [RrrVar] RrrVar -- do refs need a salt? yes! (i think?)
  | RrrBop RrrType Int [RrrVar] String  RrrExpr RrrExpr
  | RrrFun RrrType Int [RrrVar] String [RrrExpr]
  | RrrList RrrType Int [RrrVar] [RrrExpr]
  | RrrRules CompiledExpr -- wrapper around previously-compiled rules (see below)
  deriving (Eq, Show)

-- An expression that has already been compiled to Rules, wrapped so it can be
-- passed to another function. Because Rules can't be shown or compared, we
-- also carry around the original RrrExpr. TODO is the expr necessary? helpful?
-- The CompiledExpr constructor is just here so we can customize the Show and Eq instances.
data CompiledExpr = CompiledExpr RrrExpr (Rules ExprPath)

-- TODO is it a bad idea to hide the compiled-ness?
instance Show CompiledExpr where
  show (CompiledExpr e _) = "Compiled (" ++ show e ++ ")"

-- CompiledExprs are compared by the expressions they were compiled from.
instance Eq CompiledExpr where
  (CompiledExpr a _) == (CompiledExpr b _) = a == b

-- TODO is this not actually needed? seems "show expr" handles it?
saltOf :: RrrExpr -> Int
saltOf (RrrLit _ n _)       = n
saltOf (RrrRef _ n _ _)     = n
saltOf (RrrBop _ n _ _ _ _) = n
saltOf (RrrFun _ n _ _ _)   = n
saltOf (RrrList _ n _ _)     = n
saltOf (RrrRules (CompiledExpr e _)) = saltOf e

-- TODO this needs to be recursive?
setSalt :: Int -> RrrExpr -> RrrExpr
setSalt n (RrrLit t _ s)          = RrrLit t n s
setSalt n (RrrRef t _ ds v)       = RrrRef t n ds v
setSalt n (RrrBop t _ ds s e1 e2) = RrrBop t n ds s e1 e2
setSalt n (RrrFun t _ ds s es)    = RrrFun t n ds s es
setSalt n (RrrList t _ ds es)      = RrrList t n ds es
setSalt _ (RrrRules (CompiledExpr _ _)) = error "setSalt not implemented for compiled rules yet"

-- TODO add names to the RrrBops themselves... or associate with prefix versions?
prefixOf :: RrrExpr -> String
prefixOf (RrrLit rtn _ _     ) = extOf rtn
prefixOf (RrrFun _ _ _ name _) = name
prefixOf (RrrList _ _ _ _    ) = "list"
prefixOf (RrrRef _ _ _ _     ) = error  "RrrRefs don't need a prefix"
prefixOf (RrrRules (CompiledExpr e _)) = prefixOf e
prefixOf (RrrBop _ _ _ n _ _ ) = case n of
                                   "+" -> "add"
                                   "-" -> "subtract"
                                   "*" -> "multiply"
                                   "/" -> "divide"
                                   "~" -> "difference"
                                   "&" -> "intersection"
                                   "|" -> "union"
                                   _   -> error "unknown RrrBop"


-- TODO have a separate RrrAssign for "result"?
type RrrAssign = (RrrVar, RrrExpr)
type RrrScript = [RrrAssign]

-- TODO tExt etc aren't well defined for the other constructors... is that a problem?
data RrrType
  = Empty -- TODO remove this? should never be a need to define an empty list
  | ListOf RrrType
  | ScoresOf RrrType
  | RrrType
    { tExt  :: String
    , tDesc :: String -- TODO include a longer help text too
    , tShow :: RrrConfig -> Locks -> FilePath -> IO String
    }
  -- deriving (Eq, Show, Read)

defaultShow :: RrrConfig -> Locks -> FilePath -> IO String
defaultShow _ locks = fmap (unlines . fmtLines . lines) . (readFileLazy locks)
  where
    nLines      = 5
    fmtLine  l  = if length l > 80 then take 77 l ++ "..." else l
    fmtLines ls = let nPlusOne = map fmtLine $ take (nLines + 1) ls
                  in if length nPlusOne > nLines
                    then init nPlusOne ++ ["..."]
                    else nPlusOne

-- TODO is it dangerous to just assume they're the same by extension?
--      maybe we need to assert no duplicates while loading modules?
-- TODO should this use typesMatch?
instance Eq RrrType where
  Empty        == Empty        = True
  (ListOf a)   == (ListOf b)   = a == b
  (ScoresOf a) == (ScoresOf b) = a == b
  t1           == t2           = extOf t1 == extOf t2

instance Show RrrType where
  show = extOf

typeOf :: RrrExpr -> RrrType
typeOf (RrrLit   t _ _      ) = t
typeOf (RrrRef   t _ _ _    ) = t
typeOf (RrrBop   t _ _ _ _ _) = t
typeOf (RrrFun   t _ _ _ _  ) = t
typeOf (RrrList  t _ _ _    ) = ListOf t -- t can be Empty
typeOf (RrrRules (CompiledExpr e _)) = typeOf e
-- typeOf (RrrList _ _ _ ts     ) = ListOf $ nonEmptyType $ map typeOf ts
-- typeOf (RrrList _ _ _ []     ) = Empty
-- typeOf (RrrList _ _ _ []     ) = ListOf Empty

-- Works around a bug where if the first element is an empty list but others
-- have elements, it would call the whole thing an "emptylist.list".
-- Note no typechecking happens here; heterogenous lists won't be noticed.
-- nonEmptyType :: [RrrExpr] -> RrrType
-- nonEmptyType    []  = Empty
-- nonEmptyType (x:[]) = typeOf x -- catches (ListOf Empty)
-- nonEmptyType (_:xs) = nonEmptyType xs

-- note that traceShow in here can cause an infinite loop
-- and that there will be an issue if it's called on Empty alone
extOf :: RrrType -> String
extOf Empty        = "empty" -- for lists with nothing in them yet
extOf (ListOf   t) = extOf t ++ ".list"
extOf (ScoresOf t) = extOf t ++ ".scores"
extOf t            = tExt t

varOf :: RrrExpr -> [RrrVar]
varOf (RrrRef _ _ _ v) = [v]
varOf _                = [ ]

depsOf :: RrrExpr -> [RrrVar]
depsOf (RrrLit  _ _ _         ) = []
depsOf (RrrRef  _ _ vs v      ) = v:vs -- TODO redundant?
depsOf (RrrBop  _ _ vs _ e1 e2) = nub $ vs ++ concatMap varOf [e1, e2]
depsOf (RrrFun  _ _ vs _ es   ) = nub $ vs ++ concatMap varOf es
depsOf (RrrList _ _ vs   es   ) = nub $ vs ++ concatMap varOf es
depsOf (RrrRules (CompiledExpr e _)) = depsOf e

rDepsOf :: RrrScript -> RrrVar -> [RrrVar]
rDepsOf scr var = map fst rDeps
  where
    rDeps = filter (\(_,e) -> isRDep e) scr
    isRDep expr = elem var $ depsOf expr

-- TODO move to modules as soon as parsing works again
-- TODO keep literals in the core along with refs and stuff? seems reasonable
-- TODO how about lists/sets, are those core too?

str :: RrrType
str = RrrType
  { tExt  = "str"
  , tDesc = "string"
  -- TODO make one of the read functions be IO for this instead
  , tShow = \_ ls f -> do
      -- putStrLn $ "reading " ++ f
      txt <- fmap init $ withReadLock ls f $ readFileStrict ls f
      let txt' = if txt == "<<emptystr>>" then "" else txt
      return $ "\"" ++ txt' ++ "\""
  }

num :: RrrType
num = RrrType
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
data RrrConfig = RrrConfig
  { cfgScript  :: Maybe FilePath
  , cfgTmpDir  :: FilePath
  , cfgWorkDir :: FilePath
  , cfgDebug   :: Bool
  , cfgModules :: [RrrModule]
  , cfgWrapper :: Maybe FilePath
  , cfgReport  :: Maybe String
  , cfgTestPtn :: Maybe String
  , cfgWidth   :: Maybe Int -- for testing
  , cfgSecure  :: Bool
  , cfgParLock :: Resource
  }
  deriving Show

listFunctionNames :: RrrConfig -> [String]
listFunctionNames cfg = map fName $ concat $ map mFunctions $ cfgModules cfg

-- used by the compiler and repl
-- TODO find bops by char or name too
-- TODO filter to get a list and assert length == 1fs
findFunction :: RrrConfig -> String -> Maybe RrrFunction
findFunction cfg name = find (\f -> fName f == name) fs
  where
    ms = cfgModules cfg
    fs = concatMap mFunctions ms

findType :: RrrConfig -> String -> Maybe RrrType
findType cfg ext = find (\t -> tExt t == ext) ts
  where
    ms = cfgModules cfg
    ts = concatMap mTypes ms

listFunctions :: RrrConfig -> [RrrFunction]
listFunctions cfg = concat $ map mFunctions $ cfgModules cfg

-- Now with guard against accidentally including parts of prefix fn names!
operatorChars :: RrrConfig -> [Char]
operatorChars cfg = if cfgDebug cfg then chars' else chars
  where
    bops    = filter (\f -> fFixity f == Infix) $ listFunctions cfg
    bChar n = if length n == 1 then head n else error $ "bad bop name: " ++ n
    chars   = map (bChar . fName) bops
    chars'  = trace ("operatorChars: '" ++ chars ++ "'") chars

-----------------
-- Parse monad --
-----------------

-- we sanitize the input fasta files to prevent various bugs,
-- then use this hash -> seqid map to put the original ids back at the end
type HashedSeqIDs = M.Map String String

-- this lets me cheat and not bother threading the ID map through all the monad stuff
-- TODO go back and do it right
type HashedSeqIDsRef = IORef HashedSeqIDs

type RrrState = (RrrScript, RrrConfig, Locks, HashedSeqIDsRef)
type ParseM a = P.Parsec String RrrState a

runParseM :: ParseM a -> RrrState -> String -> Either ParseError a
runParseM p s@(_, cfg, _, _) = P.runParser p s file
  where
    file = case cfgScript cfg of
             Nothing -> "repl"
             Just f  -> makeRelative (cfgWorkDir cfg) f

----------------
-- Repl monad --
----------------

type ReplM a = StateT RrrState (MaybeT (InputT IO)) a

-- TODO use useFile(Handle) for stdin?
-- TODO use getExternalPrint to safely print during Tasty tests!
runReplM :: Settings IO -> ReplM a -> RrrState -> IO (Maybe RrrState)
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

-- TODO replace current RrrType with something like this:
-- TODO does eq make sense here?
-- data RrrType = RrrType
--   { tName :: String
--   , tExt  :: String
--   , tDesc :: String
--   }
--   deriving (Eq, Show, Read)

-- TODO should there be any more fundamental difference between fns and bops?
data RrrFixity = Prefix | Infix
  deriving (Eq, Show, Read)

-- TODO does eq make sense here? should i just be comparing names??
-- TODO pretty instance like "union: [set, set] -> set"? just "union" for now
data RrrFunction = RrrFunction
  { fName      :: String
  , fTypeCheck :: [RrrType] -> Either String RrrType
  , fDesc      :: Maybe String -- TODO take out the maybe once they're written
  , fTypeDesc  :: String
  , fFixity    :: RrrFixity
  , fRules     :: RrrState -> RrrExpr -> Rules ExprPath
  -- , fHidden    :: Bool -- hide "internal" functions like reverse blast
  }
  -- deriving (Eq, Read)

mkTypeDesc :: String -> [RrrType] -> RrrType -> String
mkTypeDesc n is o = unwords $ [n, ":"] ++ map extOf is ++ ["->", extOf o]

-- TODO does eq make sense here?
data RrrModule = RrrModule
  { mName :: String
  , mDesc :: String
  , mTypes     :: [RrrType]
  , mFunctions :: [RrrFunction]
  }
  -- deriving (Eq, Read)

-- TODO what about prettyShow in Pretty.hs?
instance Show RrrModule where
  show = mName

-- TODO what if it's a function call?
-- do we have to make a rule that you can't use those?
-- (uuuugly! but not a show-stopper for now)
extractExprs :: RrrScript -> RrrExpr -> [RrrExpr]
extractExprs  _  (RrrList _ _ _ es) = es
extractExprs scr (RrrRef  _ _ _ v ) = case lookup v scr of
                                        Nothing -> error $ "no such var " ++ show v
                                        Just e  -> extractExprs scr e
extractExprs _   (RrrFun _ _ _ _ _) = error explainFnBug
extractExprs scr (RrrBop _ _ _ _ l r) = extractExprs scr l ++ extractExprs scr r
extractExprs  _   e               = error $ "bad arg to extractExprs: " ++ show e

-- TODO will this get printed, or will there just be a parse error?
explainFnBug :: String
explainFnBug =
  "You've stumbled on an outstanding bug. Sorry about that! \
  \The problem is that when doing transformations involving lists \
  \like repeat or map, Detourrr can't \"see\" through future function calls; \
  \it can only manipulate lists whose elements are known *before* running the \
  \program. If you want Jeff to consider rewriting some things to fix that, \
  \drop him a line!"

-- this mostly checks equality, but also has to deal with how an empty list can
-- be any kind of list
-- TODO is there any more elegant way? this seems error-prone...
typeMatches :: RrrType -> RrrType -> Bool
typeMatches Empty _ = True
typeMatches _ Empty = True
typeMatches (ListOf   a) (ListOf   b) = typeMatches a b
typeMatches (ScoresOf a) (ScoresOf b) = typeMatches a b
typeMatches a b = a == b

typesMatch :: [RrrType] -> [RrrType] -> Bool
typesMatch as bs = sameLength && allMatch
  where
    sameLength = length as == length bs
    allMatch   = all (\(a,b) -> a `typeMatches` b) (zip as bs)

nonEmptyType :: [RrrType] -> Either String RrrType
nonEmptyType ts = if typesOK then Right elemType else Left errorMsg
  where
    nonEmpty = filter isNonEmpty ts
    elemType = if      null ts       then Empty
               else if null nonEmpty then head ts -- for example (ListOf Empty)
               else    head nonEmpty
    typesOK  = all (typeMatches elemType) ts
    errorMsg = "all elements of a list must have the same type"

isNonEmpty :: RrrType -> Bool
isNonEmpty Empty      = False
isNonEmpty (ListOf t) = isNonEmpty t
isNonEmpty _          = True
