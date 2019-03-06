module ShortCut.Core.Types
  -- type aliases and newtypes
  ( CutPath(..)
  , Action1
  , Action2
  , Action3
  , ActionFn
  , RulesFn
  , TypeChecker
  -- data structures
  , CutAssign
  , CutExpr(..)
  , CompiledExpr(..)
  , CutConfig(..)
  , findType
  , findFunction
  , listFunctions
  , listFunctionNames
  , operatorChars
  -- , WrapperConfig(..)
  , CutType(..)
  , ReplaceID(..)
  , RepeatSalt(..)
  , CutVar(..)
  , CutScript
  , Locks
  , HashedSeqIDs
  , HashedSeqIDsRef
  , CutState
  -- , Assoc(..) -- we reuse this from Parsec
  , CutFixity(..)
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
  , CutFunction(..)
  , mkTypeDesc
  , CutModule(..)
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

import ShortCut.Core.Locks (Locks, withReadLock)
import ShortCut.Core.Util  (readFileStrict, readFileLazy)

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

newtype CutPath = CutPath FilePath deriving (Eq, Ord, Show)

-- Note that each ActionN takes N+1 CutPaths, because the first is the output
-- TODO take the output last instead?
type Action1 = CutConfig -> Locks -> HashedSeqIDsRef -> CutPath -> CutPath -> Action ()
type Action2 = CutConfig -> Locks -> HashedSeqIDsRef -> CutPath -> CutPath -> CutPath -> Action ()
type Action3 = CutConfig -> Locks -> HashedSeqIDsRef -> CutPath -> CutPath -> CutPath -> CutPath -> Action ()

-- TODO remove when able in favor of well-typed versions above
type ActionFn    = CutConfig -> CacheDir -> [ExprPath] -> Action ()

type RulesFn     = CutState -> CutExpr -> Rules ExprPath
type TypeChecker = [CutType] -> Either String CutType

newtype CacheDir = CacheDir FilePath deriving (Read, Show, Eq) -- ~/.shortcut/cache/<modname>
newtype ExprPath = ExprPath FilePath deriving (Read, Show, Eq) -- ~/.shortcut/exprs/<fnname>/<hash>.<type>
newtype VarPath  = VarPath  FilePath deriving (Read, Show, Eq) -- ~/.shortcut/vars/<varname>.<type>
newtype ResPath  = ResPath  FilePath deriving (Read, Show, Eq) -- ~/.shortcut/vars/result[.<hash>.<type>]

-- Filename extension, which in ShortCut is equivalent to variable type
-- TODO can this be done better with phantom types?
-- data Ext = ListOf Ext | Ext String
  -- deriving (Eq, Show, Read)

-- A digest identifying which replace_* call the variable is part of.
-- TODO This isn't very elegant; can it be removed?
newtype ReplaceID = ReplaceID (Maybe String) deriving (Eq, Show, Read)

-- A number that can be incremented to change the expression's hash, causing repeat evaluation.
newtype RepeatSalt = RepeatSalt Int deriving (Eq, Show, Read)

data CutVar = CutVar ReplaceID String
  deriving (Eq, Show, Read)
 
-- the common fields are:
-- * return type
-- * salt, which can be changed to force re-evaluation of an expr + all depends
--   (it should start at 0 and be incremented, though that doesn't really matter)
-- TODO start from 1 instead of 0?
-- TODO test that it works correctly! in particular, it should go thru refs!
--      (do we need to add salts of subepxressions or something? or use randoms?)
-- * list of dependencies (except lits don't have any)
data CutExpr
  = CutLit CutType RepeatSalt String
  | CutRef CutType RepeatSalt [CutVar] CutVar -- do refs need a salt? yes! (i think?)
  | CutBop CutType RepeatSalt [CutVar] String  CutExpr CutExpr
  | CutFun CutType RepeatSalt [CutVar] String [CutExpr]
  | CutList CutType RepeatSalt [CutVar] [CutExpr]
  | CutRules CompiledExpr -- wrapper around previously-compiled rules (see below)
  deriving (Eq, Show)

-- An expression that has already been compiled to Rules, wrapped so it can be
-- passed to another function. Because Rules can't be shown or compared, we
-- also carry around the original CutExpr. TODO is the expr necessary? helpful?
-- The CompiledExpr constructor is just here so we can customize the Show and Eq instances.
-- The extra ExprPath is weird, but seems to be required since we can't get at the second one.
data CompiledExpr = CompiledExpr CutType ExprPath (Rules ExprPath)

-- TODO is it a bad idea to hide the compiled-ness?
-- TODO can this be made into a CutPath?
-- TODO is show ever really needed?
instance Show CompiledExpr where
  show (CompiledExpr t p _) = "CompiledExpr " ++ extOf t ++ " " ++ show p ++ " <<Rules ExprPath>>"

-- CompiledExprs are compared by the expressions they were compiled from.
instance Eq CompiledExpr where
  (CompiledExpr _ p1 _) == (CompiledExpr _ p2 _) = p1 == p2

-- TODO is this not actually needed? seems "show expr" handles it?
saltOf :: CutExpr -> RepeatSalt
saltOf (CutLit _ n _)       = n
saltOf (CutRef _ n _ _)     = n
saltOf (CutBop _ n _ _ _ _) = n
saltOf (CutFun _ n _ _ _)   = n
saltOf (CutList _ n _ _)     = n
saltOf (CutRules (CompiledExpr _ _ _)) = error "CompiledExprs don't have salts" -- TODO is that OK?

-- TODO this needs to be recursive?
-- TODO would a recursive version be able to replace addPrefixes in ReplaceEach?
setSalt :: Int -> CutExpr -> CutExpr
setSalt r (CutLit t _ s)          = CutLit  t (RepeatSalt r) s
setSalt r (CutRef t _ ds v)       = CutRef  t (RepeatSalt r) ds v
setSalt r (CutBop t _ ds s e1 e2) = CutBop  t (RepeatSalt r) ds s e1 e2
setSalt r (CutFun t _ ds s es)    = CutFun  t (RepeatSalt r) ds s es
setSalt r (CutList t _ ds es)     = CutList t (RepeatSalt r) ds es
setSalt _ (CutRules (CompiledExpr _ _ _)) = error "setSalt not implemented for compiled rules" -- TODO should it be?

-- TODO add names to the CutBops themselves... or associate with prefix versions?
prefixOf :: CutExpr -> String
prefixOf (CutLit rtn _ _     ) = extOf rtn
prefixOf (CutFun _ _ _ name _) = name
prefixOf (CutList _ _ _ _    ) = "list"
prefixOf (CutRef _ _ _ _     ) = error  "CutRefs don't need a prefix"
prefixOf (CutRules (CompiledExpr _ _ _)) = error "CompiledExprs don't need a prefix"
prefixOf (CutBop _ _ _ n _ _ ) = case n of
                                   "+" -> "add"
                                   "-" -> "subtract"
                                   "*" -> "multiply"
                                   "/" -> "divide"
                                   "~" -> "difference"
                                   "&" -> "intersection"
                                   "|" -> "union"
                                   _   -> error "unknown CutBop"


-- TODO have a separate CutAssign for "result"?
type CutAssign = (CutVar, CutExpr)
type CutScript = [CutAssign]

-- TODO tExt etc aren't well defined for the other constructors... is that a problem?
data CutType
  = Empty -- TODO remove this? should never be a need to define an empty list
  | ListOf CutType
  | ScoresOf CutType
  | CutType
    { tExt  :: String
    , tDesc :: String -- TODO include a longer help text too
    , tShow :: CutConfig -> Locks -> FilePath -> IO String
    }
  -- deriving (Eq, Show, Read)

defaultShow :: CutConfig -> Locks -> FilePath -> IO String
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
instance Eq CutType where
  Empty        == Empty        = True
  (ListOf a)   == (ListOf b)   = a == b
  (ScoresOf a) == (ScoresOf b) = a == b
  t1           == t2           = extOf t1 == extOf t2

instance Show CutType where
  show = extOf

typeOf :: CutExpr -> CutType
typeOf (CutLit   t _ _      ) = t
typeOf (CutRef   t _ _ _    ) = t
typeOf (CutBop   t _ _ _ _ _) = t
typeOf (CutFun   t _ _ _ _  ) = t
typeOf (CutList  t _ _ _    ) = ListOf t -- t can be Empty
typeOf (CutRules (CompiledExpr t _ _)) = t
-- typeOf (CutList _ _ _ ts     ) = ListOf $ nonEmptyType $ map typeOf ts
-- typeOf (CutList _ _ _ []     ) = Empty
-- typeOf (CutList _ _ _ []     ) = ListOf Empty

-- Works around a bug where if the first element is an empty list but others
-- have elements, it would call the whole thing an "emptylist.list".
-- Note no typechecking happens here; heterogenous lists won't be noticed.
-- nonEmptyType :: [CutExpr] -> CutType
-- nonEmptyType    []  = Empty
-- nonEmptyType (x:[]) = typeOf x -- catches (ListOf Empty)
-- nonEmptyType (_:xs) = nonEmptyType xs

-- note that traceShow in here can cause an infinite loop
-- and that there will be an issue if it's called on Empty alone
extOf :: CutType -> String
extOf Empty        = "empty" -- for lists with nothing in them yet
extOf (ListOf   t) = extOf t ++ ".list"
extOf (ScoresOf t) = extOf t ++ ".scores"
extOf t            = tExt t

varOf :: CutExpr -> [CutVar]
varOf (CutRef _ _ _ v) = [v]
varOf _                = [ ]

depsOf :: CutExpr -> [CutVar]
depsOf (CutLit  _ _ _         ) = []
depsOf (CutRef  _ _ vs v      ) = v:vs -- TODO redundant?
depsOf (CutBop  _ _ vs _ e1 e2) = nub $ vs ++ concatMap varOf [e1, e2]
depsOf (CutFun  _ _ vs _ es   ) = nub $ vs ++ concatMap varOf es
depsOf (CutList _ _ vs   es   ) = nub $ vs ++ concatMap varOf es
depsOf (CutRules (CompiledExpr _ _ _)) = [] -- TODO should this be an error instead? their deps are accounted for

rDepsOf :: CutScript -> CutVar -> [CutVar]
rDepsOf scr var = map fst rDeps
  where
    rDeps = filter (\(_,e) -> isRDep e) scr
    isRDep expr = elem var $ depsOf expr

-- TODO move to modules as soon as parsing works again
-- TODO keep literals in the core along with refs and stuff? seems reasonable
-- TODO how about lists/sets, are those core too?

str :: CutType
str = CutType
  { tExt  = "str"
  , tDesc = "string"
  -- TODO make one of the read functions be IO for this instead
  , tShow = \_ ls f -> do
      -- putStrLn $ "reading " ++ f
      txt <- fmap init $ withReadLock ls f $ readFileStrict ls f
      let txt' = if txt == "<<emptystr>>" then "" else txt
      return $ "\"" ++ txt' ++ "\""
  }

num :: CutType
num = CutType
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
data CutConfig = CutConfig
  { cfgScript   :: Maybe FilePath
  , cfgTmpDir   :: FilePath
  , cfgShareDir :: Maybe FilePath
  , cfgWorkDir  :: FilePath
  , cfgDebug    :: Bool
  , cfgModules  :: [CutModule]
  , cfgWrapper  :: Maybe FilePath
  , cfgReport   :: Maybe String
  , cfgTestPtn  :: Maybe String
  , cfgWidth    :: Maybe Int -- for testing
  , cfgSecure   :: Bool
  , cfgParLock  :: Resource
  }
  deriving Show

listFunctionNames :: CutConfig -> [String]
listFunctionNames cfg = map fName $ concat $ map mFunctions $ cfgModules cfg

-- used by the compiler and repl
-- TODO find bops by char or name too
-- TODO filter to get a list and assert length == 1fs
findFunction :: CutConfig -> String -> Maybe CutFunction
findFunction cfg name = find (\f -> fName f == name) fs
  where
    ms = cfgModules cfg
    fs = concatMap mFunctions ms

findType :: CutConfig -> String -> Maybe CutType
findType cfg ext = find (\t -> tExt t == ext) ts
  where
    ms = cfgModules cfg
    ts = concatMap mTypes ms

listFunctions :: CutConfig -> [CutFunction]
listFunctions cfg = concat $ map mFunctions $ cfgModules cfg

-- Now with guard against accidentally including parts of prefix fn names!
operatorChars :: CutConfig -> [Char]
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

type CutState = (CutScript, CutConfig, Locks, HashedSeqIDsRef)
type ParseM a = P.Parsec String CutState a

runParseM :: ParseM a -> CutState -> String -> Either ParseError a
runParseM p s@(_, cfg, _, _) = P.runParser p s file
  where
    file = case cfgScript cfg of
             Nothing -> "repl"
             Just f  -> makeRelative (cfgWorkDir cfg) f

----------------
-- Repl monad --
----------------

type ReplM a = StateT CutState (MaybeT (InputT IO)) a

-- TODO use useFile(Handle) for stdin?
-- TODO use getExternalPrint to safely print during Tasty tests!
runReplM :: Settings IO -> ReplM a -> CutState -> IO (Maybe CutState)
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

-- TODO replace current CutType with something like this:
-- TODO does eq make sense here?
-- data CutType = CutType
--   { tName :: String
--   , tExt  :: String
--   , tDesc :: String
--   }
--   deriving (Eq, Show, Read)

-- TODO should there be any more fundamental difference between fns and bops?
data CutFixity = Prefix | Infix
  deriving (Eq, Show, Read)

-- TODO does eq make sense here? should i just be comparing names??
-- TODO pretty instance like "union: [set, set] -> set"? just "union" for now
data CutFunction = CutFunction
  { fName      :: String
  , fTypeCheck :: [CutType] -> Either String CutType
  , fDesc      :: Maybe String -- TODO take out the maybe once they're written
  , fTypeDesc  :: String
  , fFixity    :: CutFixity
  , fRules     :: CutState -> CutExpr -> Rules ExprPath
  -- , fHidden    :: Bool -- hide "internal" functions like reverse blast
  }
  -- deriving (Eq, Read)

mkTypeDesc :: String -> [CutType] -> CutType -> String
mkTypeDesc n is o = unwords $ [n, ":"] ++ map extOf is ++ ["->", extOf o]

-- TODO does eq make sense here?
data CutModule = CutModule
  { mName :: String
  , mDesc :: String
  , mTypes     :: [CutType]
  , mFunctions :: [CutFunction]
  }
  -- deriving (Eq, Read)

-- TODO what about prettyShow in Pretty.hs?
instance Show CutModule where
  show = mName

-- TODO what if it's a function call?
-- do we have to make a rule that you can't use those?
-- (uuuugly! but not a show-stopper for now)
extractExprs :: CutScript -> CutExpr -> [CutExpr]
extractExprs  _  (CutList _ _ _ es) = es
extractExprs scr (CutRef  _ _ _ v ) = case lookup v scr of
                                        Nothing -> error $ "no such var " ++ show v
                                        Just e  -> extractExprs scr e
extractExprs _   (CutFun _ _ _ _ _) = error explainFnBug
extractExprs scr (CutBop _ _ _ _ l r) = extractExprs scr l ++ extractExprs scr r
extractExprs  _   e               = error $ "bad arg to extractExprs: " ++ show e

-- TODO will this get printed, or will there just be a parse error?
explainFnBug :: String
explainFnBug =
  "You've stumbled on an outstanding bug. Sorry about that! \
  \The problem is that when doing transformations involving lists \
  \like repeat or map, ShortCut can't \"see\" through future function calls; \
  \it can only manipulate lists whose elements are known *before* running the \
  \program. If you want Jeff to consider rewriting some things to fix that, \
  \drop him a line!"

-- this mostly checks equality, but also has to deal with how an empty list can
-- be any kind of list
-- TODO is there any more elegant way? this seems error-prone...
typeMatches :: CutType -> CutType -> Bool
typeMatches Empty _ = True
typeMatches _ Empty = True
typeMatches (ListOf   a) (ListOf   b) = typeMatches a b
typeMatches (ScoresOf a) (ScoresOf b) = typeMatches a b
typeMatches a b = a == b

typesMatch :: [CutType] -> [CutType] -> Bool
typesMatch as bs = sameLength && allMatch
  where
    sameLength = length as == length bs
    allMatch   = all (\(a,b) -> a `typeMatches` b) (zip as bs)

nonEmptyType :: [CutType] -> Either String CutType
nonEmptyType ts = if typesOK then Right elemType else Left errorMsg
  where
    nonEmpty = filter isNonEmpty ts
    elemType = if      null ts       then Empty
               else if null nonEmpty then head ts -- for example (ListOf Empty)
               else    head nonEmpty
    typesOK  = all (typeMatches elemType) ts
    errorMsg = "all elements of a list must have the same type"

isNonEmpty :: CutType -> Bool
isNonEmpty Empty      = False
isNonEmpty (ListOf t) = isNonEmpty t
isNonEmpty _          = True
