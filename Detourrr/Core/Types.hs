module Detourrr.Core.Types
  -- type aliases and newtypes
  ( DtrPath(..)
  , Action1
  , Action2
  , Action3
  , ActionFn
  , RulesFn
  , TypeChecker
  -- data structures
  , DtrAssign
  , DtrExpr(..)
  , CompiledExpr(..)
  , DtrConfig(..)
  , findType
  , findFunction
  , listFunctions
  , listFunctionNames
  , operatorChars
  -- , WrapperConfig(..)
  , DtrType(..)
  , DtrVar(..)
  , DtrScript
  , Locks
  , HashedSeqIDs
  , HashedSeqIDsRef
  , DtrState
  -- , Assoc(..) -- we reuse this from Parsec
  , DtrFixity(..)
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
  , DtrFunction(..)
  , mkTypeDesc
  , DtrModule(..)
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

newtype DtrPath = DtrPath FilePath deriving (Eq, Ord, Show)

-- Note that each ActionN takes N+1 DtrPaths, because the first is the output
-- TODO take the output last instead?
type Action1 = DtrConfig -> Locks -> HashedSeqIDsRef -> DtrPath -> DtrPath -> Action ()
type Action2 = DtrConfig -> Locks -> HashedSeqIDsRef -> DtrPath -> DtrPath -> DtrPath -> Action ()
type Action3 = DtrConfig -> Locks -> HashedSeqIDsRef -> DtrPath -> DtrPath -> DtrPath -> DtrPath -> Action ()

-- TODO remove when able in favor of well-typed versions above
type ActionFn    = DtrConfig -> CacheDir -> [ExprPath] -> Action ()

type RulesFn     = DtrState -> DtrExpr -> Rules ExprPath
type TypeChecker = [DtrType] -> Either String DtrType

newtype CacheDir = CacheDir FilePath deriving Show -- ~/.detourrr/cache/<modname>
newtype ExprPath = ExprPath FilePath deriving Show -- ~/.detourrr/exprs/<fnname>/<hash>.<type>
newtype VarPath  = VarPath  FilePath deriving Show -- ~/.detourrr/vars/<varname>.<type>
newtype ResPath  = ResPath  FilePath deriving Show -- ~/.detourrr/vars/result[.<hash>.<type>]

-- Filename extension, which in Detourrr is equivalent to variable type
-- TODO can this be done better with phantom types?
-- data Ext = ListOf Ext | Ext String
  -- deriving (Eq, Show, Read)

newtype DtrVar = DtrVar String deriving (Eq, Show, Read)
 
-- the common fields are:
-- * return type
-- * salt, which can be changed to force re-evaluation of an expr + all depends
--   (it should start at 0 and be incremented, though that doesn't really matter)
-- TODO start from 1 instead of 0?
-- TODO test that it works correctly! in particular, it should go thru refs!
--      (do we need to add salts of subepxressions or something? or use randoms?)
-- * list of dependencies (except lits don't have any)
data DtrExpr
  = DtrLit DtrType Int String
  | DtrRef DtrType Int [DtrVar] DtrVar -- do refs need a salt? yes! (i think?)
  | DtrBop DtrType Int [DtrVar] String  DtrExpr DtrExpr
  | DtrFun DtrType Int [DtrVar] String [DtrExpr]
  | DtrList DtrType Int [DtrVar] [DtrExpr]
  | DtrRules CompiledExpr -- wrapper around previously-compiled rules (see below)
  deriving (Eq, Show)

-- An expression that has already been compiled to Rules, wrapped so it can be
-- passed to another function. Because Rules can't be shown or compared, we
-- also carry around the original DtrExpr. TODO is the expr necessary? helpful?
-- The CompiledExpr constructor is just here so we can customize the Show and Eq instances.
data CompiledExpr = CompiledExpr DtrExpr (Rules ExprPath)

-- TODO is it a bad idea to hide the compiled-ness?
instance Show CompiledExpr where
  show (CompiledExpr e _) = "Compiled (" ++ show e ++ ")"

-- CompiledExprs are compared by the expressions they were compiled from.
instance Eq CompiledExpr where
  (CompiledExpr a _) == (CompiledExpr b _) = a == b

-- TODO is this not actually needed? seems "show expr" handles it?
saltOf :: DtrExpr -> Int
saltOf (DtrLit _ n _)       = n
saltOf (DtrRef _ n _ _)     = n
saltOf (DtrBop _ n _ _ _ _) = n
saltOf (DtrFun _ n _ _ _)   = n
saltOf (DtrList _ n _ _)     = n
saltOf (DtrRules (CompiledExpr e _)) = saltOf e

-- TODO this needs to be recursive?
setSalt :: Int -> DtrExpr -> DtrExpr
setSalt n (DtrLit t _ s)          = DtrLit t n s
setSalt n (DtrRef t _ ds v)       = DtrRef t n ds v
setSalt n (DtrBop t _ ds s e1 e2) = DtrBop t n ds s e1 e2
setSalt n (DtrFun t _ ds s es)    = DtrFun t n ds s es
setSalt n (DtrList t _ ds es)      = DtrList t n ds es
setSalt _ (DtrRules (CompiledExpr _ _)) = error "setSalt not implemented for compiled rules yet"

-- TODO add names to the DtrBops themselves... or associate with prefix versions?
prefixOf :: DtrExpr -> String
prefixOf (DtrLit rtn _ _     ) = extOf rtn
prefixOf (DtrFun _ _ _ name _) = name
prefixOf (DtrList _ _ _ _    ) = "list"
prefixOf (DtrRef _ _ _ _     ) = error  "DtrRefs don't need a prefix"
prefixOf (DtrRules (CompiledExpr e _)) = prefixOf e
prefixOf (DtrBop _ _ _ n _ _ ) = case n of
                                   "+" -> "add"
                                   "-" -> "subtract"
                                   "*" -> "multiply"
                                   "/" -> "divide"
                                   "~" -> "difference"
                                   "&" -> "intersection"
                                   "|" -> "union"
                                   _   -> error "unknown DtrBop"


-- TODO have a separate DtrAssign for "result"?
type DtrAssign = (DtrVar, DtrExpr)
type DtrScript = [DtrAssign]

-- TODO tExt etc aren't well defined for the other constructors... is that a problem?
data DtrType
  = Empty -- TODO remove this? should never be a need to define an empty list
  | ListOf DtrType
  | ScoresOf DtrType
  | DtrType
    { tExt  :: String
    , tDesc :: String -- TODO include a longer help text too
    , tShow :: DtrConfig -> Locks -> FilePath -> IO String
    }
  -- deriving (Eq, Show, Read)

defaultShow :: DtrConfig -> Locks -> FilePath -> IO String
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
instance Eq DtrType where
  Empty        == Empty        = True
  (ListOf a)   == (ListOf b)   = a == b
  (ScoresOf a) == (ScoresOf b) = a == b
  t1           == t2           = extOf t1 == extOf t2

instance Show DtrType where
  show = extOf

typeOf :: DtrExpr -> DtrType
typeOf (DtrLit   t _ _      ) = t
typeOf (DtrRef   t _ _ _    ) = t
typeOf (DtrBop   t _ _ _ _ _) = t
typeOf (DtrFun   t _ _ _ _  ) = t
typeOf (DtrList  t _ _ _    ) = ListOf t -- t can be Empty
typeOf (DtrRules (CompiledExpr e _)) = typeOf e
-- typeOf (DtrList _ _ _ ts     ) = ListOf $ nonEmptyType $ map typeOf ts
-- typeOf (DtrList _ _ _ []     ) = Empty
-- typeOf (DtrList _ _ _ []     ) = ListOf Empty

-- Works around a bug where if the first element is an empty list but others
-- have elements, it would call the whole thing an "emptylist.list".
-- Note no typechecking happens here; heterogenous lists won't be noticed.
-- nonEmptyType :: [DtrExpr] -> DtrType
-- nonEmptyType    []  = Empty
-- nonEmptyType (x:[]) = typeOf x -- catches (ListOf Empty)
-- nonEmptyType (_:xs) = nonEmptyType xs

-- note that traceShow in here can cause an infinite loop
-- and that there will be an issue if it's called on Empty alone
extOf :: DtrType -> String
extOf Empty        = "empty" -- for lists with nothing in them yet
extOf (ListOf   t) = extOf t ++ ".list"
extOf (ScoresOf t) = extOf t ++ ".scores"
extOf t            = tExt t

varOf :: DtrExpr -> [DtrVar]
varOf (DtrRef _ _ _ v) = [v]
varOf _                = [ ]

depsOf :: DtrExpr -> [DtrVar]
depsOf (DtrLit  _ _ _         ) = []
depsOf (DtrRef  _ _ vs v      ) = v:vs -- TODO redundant?
depsOf (DtrBop  _ _ vs _ e1 e2) = nub $ vs ++ concatMap varOf [e1, e2]
depsOf (DtrFun  _ _ vs _ es   ) = nub $ vs ++ concatMap varOf es
depsOf (DtrList _ _ vs   es   ) = nub $ vs ++ concatMap varOf es
depsOf (DtrRules (CompiledExpr e _)) = depsOf e

rDepsOf :: DtrScript -> DtrVar -> [DtrVar]
rDepsOf scr var = map fst rDeps
  where
    rDeps = filter (\(_,e) -> isRDep e) scr
    isRDep expr = elem var $ depsOf expr

-- TODO move to modules as soon as parsing works again
-- TODO keep literals in the core along with refs and stuff? seems reasonable
-- TODO how about lists/sets, are those core too?

str :: DtrType
str = DtrType
  { tExt  = "str"
  , tDesc = "string"
  -- TODO make one of the read functions be IO for this instead
  , tShow = \_ ls f -> do
      -- putStrLn $ "reading " ++ f
      txt <- fmap init $ withReadLock ls f $ readFileStrict ls f
      let txt' = if txt == "<<emptystr>>" then "" else txt
      return $ "\"" ++ txt' ++ "\""
  }

num :: DtrType
num = DtrType
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
data DtrConfig = DtrConfig
  { cfgScript  :: Maybe FilePath
  , cfgTmpDir  :: FilePath
  , cfgWorkDir :: FilePath
  , cfgDebug   :: Bool
  , cfgModules :: [DtrModule]
  , cfgWrapper :: Maybe FilePath
  , cfgReport  :: Maybe String
  , cfgTestPtn :: Maybe String
  , cfgWidth   :: Maybe Int -- for testing
  , cfgSecure  :: Bool
  , cfgParLock :: Resource
  }
  deriving Show

listFunctionNames :: DtrConfig -> [String]
listFunctionNames cfg = map fName $ concat $ map mFunctions $ cfgModules cfg

-- used by the compiler and repl
-- TODO find bops by char or name too
-- TODO filter to get a list and assert length == 1fs
findFunction :: DtrConfig -> String -> Maybe DtrFunction
findFunction cfg name = find (\f -> fName f == name) fs
  where
    ms = cfgModules cfg
    fs = concatMap mFunctions ms

findType :: DtrConfig -> String -> Maybe DtrType
findType cfg ext = find (\t -> tExt t == ext) ts
  where
    ms = cfgModules cfg
    ts = concatMap mTypes ms

listFunctions :: DtrConfig -> [DtrFunction]
listFunctions cfg = concat $ map mFunctions $ cfgModules cfg

-- Now with guard against accidentally including parts of prefix fn names!
operatorChars :: DtrConfig -> [Char]
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

type DtrState = (DtrScript, DtrConfig, Locks, HashedSeqIDsRef)
type ParseM a = P.Parsec String DtrState a

runParseM :: ParseM a -> DtrState -> String -> Either ParseError a
runParseM p s@(_, cfg, _, _) = P.runParser p s file
  where
    file = case cfgScript cfg of
             Nothing -> "repl"
             Just f  -> makeRelative (cfgWorkDir cfg) f

----------------
-- Repl monad --
----------------

type ReplM a = StateT DtrState (MaybeT (InputT IO)) a

-- TODO use useFile(Handle) for stdin?
-- TODO use getExternalPrint to safely print during Tasty tests!
runReplM :: Settings IO -> ReplM a -> DtrState -> IO (Maybe DtrState)
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

-- TODO replace current DtrType with something like this:
-- TODO does eq make sense here?
-- data DtrType = DtrType
--   { tName :: String
--   , tExt  :: String
--   , tDesc :: String
--   }
--   deriving (Eq, Show, Read)

-- TODO should there be any more fundamental difference between fns and bops?
data DtrFixity = Prefix | Infix
  deriving (Eq, Show, Read)

-- TODO does eq make sense here? should i just be comparing names??
-- TODO pretty instance like "union: [set, set] -> set"? just "union" for now
data DtrFunction = DtrFunction
  { fName      :: String
  , fTypeCheck :: [DtrType] -> Either String DtrType
  , fDesc      :: Maybe String -- TODO take out the maybe once they're written
  , fTypeDesc  :: String
  , fFixity    :: DtrFixity
  , fRules     :: DtrState -> DtrExpr -> Rules ExprPath
  -- , fHidden    :: Bool -- hide "internal" functions like reverse blast
  }
  -- deriving (Eq, Read)

mkTypeDesc :: String -> [DtrType] -> DtrType -> String
mkTypeDesc n is o = unwords $ [n, ":"] ++ map extOf is ++ ["->", extOf o]

-- TODO does eq make sense here?
data DtrModule = DtrModule
  { mName :: String
  , mDesc :: String
  , mTypes     :: [DtrType]
  , mFunctions :: [DtrFunction]
  }
  -- deriving (Eq, Read)

-- TODO what about prettyShow in Pretty.hs?
instance Show DtrModule where
  show = mName

-- TODO what if it's a function call?
-- do we have to make a rule that you can't use those?
-- (uuuugly! but not a show-stopper for now)
extractExprs :: DtrScript -> DtrExpr -> [DtrExpr]
extractExprs  _  (DtrList _ _ _ es) = es
extractExprs scr (DtrRef  _ _ _ v ) = case lookup v scr of
                                        Nothing -> error $ "no such var " ++ show v
                                        Just e  -> extractExprs scr e
extractExprs _   (DtrFun _ _ _ _ _) = error explainFnBug
extractExprs scr (DtrBop _ _ _ _ l r) = extractExprs scr l ++ extractExprs scr r
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
typeMatches :: DtrType -> DtrType -> Bool
typeMatches Empty _ = True
typeMatches _ Empty = True
typeMatches (ListOf   a) (ListOf   b) = typeMatches a b
typeMatches (ScoresOf a) (ScoresOf b) = typeMatches a b
typeMatches a b = a == b

typesMatch :: [DtrType] -> [DtrType] -> Bool
typesMatch as bs = sameLength && allMatch
  where
    sameLength = length as == length bs
    allMatch   = all (\(a,b) -> a `typeMatches` b) (zip as bs)

nonEmptyType :: [DtrType] -> Either String DtrType
nonEmptyType ts = if typesOK then Right elemType else Left errorMsg
  where
    nonEmpty = filter isNonEmpty ts
    elemType = if      null ts       then Empty
               else if null nonEmpty then head ts -- for example (ListOf Empty)
               else    head nonEmpty
    typesOK  = all (typeMatches elemType) ts
    errorMsg = "all elements of a list must have the same type"

isNonEmpty :: DtrType -> Bool
isNonEmpty Empty      = False
isNonEmpty (ListOf t) = isNonEmpty t
isNonEmpty _          = True
