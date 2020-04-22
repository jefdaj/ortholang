{-# LANGUAGE FlexibleInstances #-}

module OrthoLang.Types
  (
    Action1
  , Action2
  , Action3
  , Assign
  , CacheDir(..)
  , CompiledExpr(..)
  , Config(..)
  , DigestMap
  , DigestsRef
  , Encoding(..)
  , Expr(..)
  , ExprPath(..)
  , Ext(..)
  , FnTag(..)
  , Function(..)
  , GlobalEnv
  , IDs(..)
  , IDsRef
  , LocksRef
  , Module(..)
  , NewRules(..)
  , ParseM
  , runParseM
  , Path(..)
  , PathDigest(..)
  , RepID(..)
  , ReplCmd
  , ReplM
  , ResPath(..)
  , RulesEnv
  , RulesFn
  , RulesR
  , Salt(..)
  , Script
  , Type(..)
  , TypeGroup(..)
  , TypeSig(..)
  , Var(..)
  , VarPath(..)
  , defaultShow
  , defaultShowN -- same but lets you pick how many lines to show
  , depsOf
  , emptyDigests
  , emptyIDs
  , emptyScript
  , ensureResult
  , extractExprs
  , findEncoding
  , findFun
  , findFunction -- TODO remove?
  , findGroup
  , findModule
  , findType
  , listFunctionNames
  , listFunctions
  , lit
  , lookupResult
  , num
  , operatorChars
  , rDepsOf
  , runReplM
  , runRulesR
  , saltOf
  , setSalt
  , str
  , typeOf
  , typeSigMatches
  , Pretty
  , prettyNum -- TODO remove
  , prettyShow
  , render
  , renderIO
  , pPrint
  , pPrintHdl
  )
  where


-- still crashes, but prints a message to the logfile first
import Prelude  hiding (error, (<>))
import OrthoLang.Debug (error)

import qualified Data.Text.Lazy   as T
import qualified Text.PrettyPrint as PP

import OrthoLang.Locks (LocksRef, withReadLock)
import OrthoLang.Util  (readFileStrict, readFileLazy)

import Control.Monad.Reader       (ReaderT, runReaderT)
import Control.Monad.State.Strict (StateT, execStateT, lift, get, put)
import Control.Monad.Trans        (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Data.Char                  (toLower)
import Data.IORef                 (IORef)
import Data.List                  (nub, find, isPrefixOf)
import Data.Map.Strict            (Map, empty)
import Data.Maybe                 (fromJust, catMaybes)
import Development.Shake          (Rules, Action, Resource)
import Development.Shake.FilePath (makeRelative)
import System.Console.Haskeline   (Settings, InputT, runInputT)
import System.FilePath.Posix      ((</>))
import System.IO                  (Handle, hPutStrLn, stdout)
import Text.Parsec                (ParsecT, runPT)

import System.Console.Terminal.Size   (Window(..), size)
import Text.PrettyPrint.HughesPJClass (Pretty, pPrint, prettyShow)
import Text.PrettyPrint               (Doc, (<>), (<+>), render)
import Data.Scientific                (Scientific(), toBoundedInteger)
import Text.Pretty.Simple             (pShowNoColor)


-----------
-- paths --
-----------

newtype Path = Path FilePath deriving (Eq, Ord, Read, Show)

-- Note that each ActionN takes N+1 Paths, because the first is the output
-- TODO put the output last instead?
type Action1 = Path -> Path -> Action ()
type Action2 = Path -> Path -> Path -> Action ()
type Action3 = Path -> Path -> Path -> Path -> Action ()

newtype CacheDir = CacheDir FilePath deriving (Eq, Ord, Read, Show) -- ~/.ortholang/cache/<modname>
newtype ExprPath = ExprPath FilePath deriving (Eq, Ord, Read, Show) -- ~/.ortholang/exprs/<fnname>/<h1>/<h2>/../result
newtype VarPath  = VarPath  FilePath deriving (Eq, Ord, Read, Show) -- ~/.ortholang/vars/<varname>.<type>
newtype ResPath  = ResPath  FilePath deriving (Eq, Ord, Read, Show) -- ~/.ortholang/vars/result[.<hash>.<type>]


-----------
-- rules --
-----------

type RulesFn = Script -> Expr -> Rules ExprPath

-- for now, same as GlobalEnv. but not sure if refs are needed
type RulesEnv = (Config, LocksRef, IDsRef, DigestsRef) -- TODO remove locks? ids? put script back?
type RulesR a = ReaderT RulesEnv Rules a

runRulesR :: RulesEnv -> RulesR a -> Rules a
runRulesR env act = runReaderT act env


-----------------
-- expressions --
-----------------

-- A digest identifying which replace_* call the variable is part of.
-- TODO This isn't very elegant; can it be removed?
newtype RepID = RepID (Maybe String) deriving (Eq, Show, Read)

-- A number that can be incremented to change the expression's hash, causing repeat evaluation.
newtype Salt = Salt Int deriving (Eq, Show, Read)

data Var = Var RepID String
  deriving (Eq, Show, Read)

instance Pretty Var where
  pPrint (Var _ s) = PP.text s -- TODO show the salt?

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
  deriving (Eq, Show)

-- An expression that has already been compiled to Rules, wrapped so it can be
-- passed to another function. Because Rules can't be shown or compared, we
-- also carry around the original Expr. TODO is the expr necessary? helpful?
-- The CompiledExpr constructor is just here so we can customize the Show and Eq instances.
-- The extra ExprPath is weird, but seems to be required since we can't get at the second one.
-- TODO can it be removed? seems like a crutch/kludge
-- TODO but try re-implementing map first and see if it's useful for that
data CompiledExpr = CompiledExpr Type ExprPath (Rules ExprPath)

-- TODO is it a bad idea to hide the compiled-ness?
-- TODO can this be made into a Path?
-- TODO is show ever really needed?
-- TODO remove show instance!
instance Show CompiledExpr where
  show (CompiledExpr t p _) = "CompiledExpr " ++ ext t ++ " " ++ show p ++ " <<Rules ExprPath>>"

-- CompiledExprs are compared by the expressions they were compiled from.
instance Eq CompiledExpr where
  (CompiledExpr _ p1 _) == (CompiledExpr _ p2 _) = p1 == p2

-- TODO actual Eq instance, or what? how do we compare types?
instance Pretty Expr where
  pPrint e@(Lit _ s)
    | typeOf e == num = prettyNum s
    | otherwise = PP.text $ show s
  pPrint (Ref _ _ _ v)    = pPrint v
  pPrint (Fun _ _ _ s es) = PP.text s <+> PP.sep (map pNested es)
  pPrint (Lst _ _ es)  = pList es
  pPrint (Com (CompiledExpr t (ExprPath p) _)) = PP.text $ "Compiled " ++ ext t ++ " " ++ p

  -- this is almost right except it breaks lines too early (always nesting),
  -- which looks super weird for short bops:
  -- pPrint (Bop _ _ _ c e1 e2) = pPrint e1 $$ nest (-2) (PP.text c) $$ pPrint e2

  -- this one is a little better: the first line is right and *then* it starts doing that
  -- TODO ask on stackoverflow if there's any better way, but later
  pPrint (Bop _ _ _ c e1 e2) = PP.sep $ PP.punctuate (PP.text $ " " ++ c) [pPrint e1, pPrint e2]

pList :: (Pretty a) => [a] -> Doc
pList es = PP.text "[" <> PP.sep (PP.punctuate (PP.text ",") (map pPrint es)) <> PP.text "]"

prettyNum :: String -> Doc
prettyNum s = PP.text $
  case toBoundedInteger n of
    Just i  -> show (i :: Int)
    Nothing -> show n -- as decimal
  where
    n = read s :: Scientific

-- this adds parens around nested function calls
-- without it things can get really messy!
pNested :: Expr -> Doc
pNested e@(Fun  _ _ _ _ _  ) = PP.parens $ pPrint e
pNested e@(Bop  _ _ _ _ _ _) = PP.parens $ pPrint e
pNested e = pPrint e

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


-------------
-- scripts --
-------------

-- TODO have a separate Assign for "result"?
type Assign = (Var, Expr)
type Script = [Assign]

-- TODO newtype to prevent the overlap?
instance {-# OVERLAPPING #-} Pretty Assign where
  pPrint (v, e) = pPrint v <+> PP.text "=" <+> pPrint e
  -- this adds type info, but makes the pretty-print not valid source code
  -- pPrint (v, e) = PP.text (render (pPrint v) ++ "." ++ render (pPrint $ typeExt e))

-- TODO is totally ignoring the sDigests part OK here?
instance {-# OVERLAPPING #-} Pretty Script where
  pPrint [] = PP.empty
  pPrint as = PP.vcat $ map pPrint as

emptyScript :: Script
emptyScript = []

emptyDigests :: DigestMap
emptyDigests = empty

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

typeOf :: Expr -> Type
typeOf (Lit   t _      ) = t
typeOf (Ref   t _ _ _    ) = t
typeOf (Bop   t _ _ _ _ _) = t
typeOf (Fun   t _ _ _ _  ) = t
typeOf (Lst  t _ _    ) = ListOf t
typeOf (Com (CompiledExpr t _ _)) = t

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


-----------
-- types --
-----------

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
-- TODO use the Ext instance for this and remove Show if possible
-- TODO in fact, remove all the Show and Read except the ones explicitly needed
instance Show Type where
  show = ext

instance Pretty Type where
  pPrint Empty           = error "type.prettytype" "should never need to print Empty"
  pPrint (ListOf  Empty) = PP.text "empty list"
  pPrint (ListOf      t) = PP.text "list of" <+> pPrint t <> PP.text "s"
  pPrint (ScoresOf    t) = PP.text "list of" <+> pPrint t <> PP.text "s with scores"
  pPrint (EncodedAs e t) = pPrint t <+> PP.text "encoded as" <+> PP.text (enExt e)
  -- pPrint (Some (TypeGroup {tgExt = t, tgDesc = d}) s) = PP.text t <+> PP.parens (PP.text d) <+> parens (PP.text s) -- TODO refine this
  pPrint (Type            { tExt = t,  tDesc = d}   ) = PP.text t <+> PP.parens (PP.text d)

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

instance Pretty Encoding where
  pPrint e = PP.text $ enExt e

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

instance Pretty TypeSig where
  pPrint (ListSigs s)     = pPrint s <> PP.text ".list"
  pPrint (ScoresSigs s)   = pPrint s <> PP.text ".scores"
  pPrint (EncodedSig e s) = pPrint s <> PP.text ("." ++ enExt e)
  pPrint (AnyType _)      = PP.text "anytype" -- TODO does this make sense? might have to do variables
  pPrint (Some g _)       = pPrint g
  pPrint (Exactly t)      = pPrint t

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

instance Pretty TypeGroup where
  pPrint g = PP.text $ tgExt g

-- | types which have a file extension
class Ext a where
  ext  :: a -> String

class Desc a where
  desc :: a -> String

-- note that traceShow in here can cause an infinite loop
-- and that there will be an issue if it's called on Empty alone
instance Ext Type where
  ext Empty             = "empty" -- special case for empty lists with no element type
  ext (ListOf        t) = ext t ++ ".list"
  ext (ScoresOf      t) = ext t ++ ".scores"
  ext (EncodedAs   e t) = ext t ++ "." ++ ext e
  ext (Type {tExt = e}) = e

-- TODO separate Desc typeclass would be more obvious, and easy
instance Desc Type where
  desc Empty           = "empty list" -- for lists with nothing in them yet
  desc (ListOf      t) = "list of " ++ desc t
  desc (ScoresOf    t) = "scores for " ++ desc t
  desc (EncodedAs e t) = desc t ++ " encoded as " ++ desc e
  desc (Type {tDesc = d}) = d

instance Ext TypeGroup where
  ext  = tgExt -- TODO move defition here

instance Ext TypeSig where
  ext = undefined -- TODO write this

instance Desc TypeGroup where
  desc = tgDesc

instance Ext Encoding where
  ext = enExt -- TODO move defition here

instance Desc Encoding where
  desc = enDesc -- TODO move defition here

-- TODO either use this in the core compilers or remove it
lit :: TypeGroup
lit = TypeGroup
  { tgExt = "lit"
  , tgDesc = "basic literal (str or num)"
  , tgMembers = [Exactly str, Exactly num]
  }

-- TODO make a typeclass for this... PrettyCat?
defaultShow :: Config -> LocksRef -> FilePath -> IO String
defaultShow = defaultShowN 5

-- TODO remove? or make part of a typeclass
defaultShowN :: Int -> Config -> LocksRef -> FilePath -> IO String
defaultShowN nLines _ locks = fmap (unlines . fmtLines . lines) . (readFileLazy locks)
  where
    fmtLine  l  = if length l > 80 then take 77 l ++ "..." else l
    fmtLines ls = let nPlusOne = map fmtLine $ take (nLines + 1) ls
                  in if length nPlusOne > nLines
                    then init nPlusOne ++ ["..."]
                    else nPlusOne

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
  , showvartypes :: Bool
  }
  deriving (Show)

-- TODO update this by mapping over the fields
instance Pretty Config where
  pPrint = PP.text . T.unpack . pShowNoColor


-------------
-- modules --
-------------

-- TODO does eq make sense here?
data Module = Module
  { mName      :: String
  , mDesc      :: String
  , mTypes     :: [Type]
  , mGroups    :: [TypeGroup]
  , mEncodings :: [Encoding]
  , mFunctions :: [Function]
  }

-- TODO what about prettyShow in Pretty.hs?
instance Show Module where
  show = mName

-- TODO change this to something useful
instance Pretty Module where
  pPrint fn = PP.text $ "Module \"" ++ mName fn ++ "\""

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


------------
-- parser --
------------

type ParseM a = ParsecT String Script (ReaderT [Module] (Except String)) a

-- TODO is cfg still needed? if so, add mods
-- originally based on https://stackoverflow.com/a/54089987/429898
runParseM :: [Module] -> ParseM a -> Config -> Script -> String -> Either String a
runParseM ms op cfg scr input = case runExcept (runReaderT (runPT op scr sn input) ms) of
  Left s          -> Left s        -- parseFail; return the String
  Right (Left  e) -> Left (show e) -- Parsec error; convert to String
  Right (Right r) -> Right r
  where
    sn = case script cfg of
           Nothing -> "repl"
           Just f  -> makeRelative (workdir cfg) f

------------
-- seqids --
------------

-- we sanitize the input fasta files to prevent various bugs,
-- then use this hash -> seqid map to put the original ids back at the end
-- hFiles is for making paths generic
-- hSeqIDs is the main one, and stores hash -> seqid maps indexed by their (generic) hFiles path
-- TODO use bytestring-tries rather than maps with string keys?
-- TODO should this be ActionIDs in general? aka all the stuff that might be needed in Action
data IDs = IDs
  { hFiles  :: Map String String
  , hSeqIDs :: Map String (Map String String)
  }
  deriving (Show)

emptyIDs :: IDs
emptyIDs = IDs empty empty

-- this lets me cheat and not bother threading the ID map through all the monad stuff
-- TODO go back and do it right
type IDsRef = IORef IDs

newtype PathDigest = PathDigest String deriving (Read, Show, Eq, Ord)

type DigestMap = Map PathDigest (Type, Path)
type DigestsRef = IORef DigestMap

instance Pretty DigestMap where
  pPrint m = PP.text $ show m


---------------
-- functions --
---------------

data FnTag
  = Stochastic -- do repeat, do cache/share
  | ReadsDirs  -- do not repeat, do not cache/share
  | ReadsFile  -- do not repeat, do cache/share TODO ReadsFiles
  | ReadsURL   -- do not repeat, do not cache/share?
  | Broken     -- remove from functions list when loading
  | Hidden     -- remove from user-facing lists
  deriving (Eq, Read, Show)

-- TODO does eq make sense here? should i just be comparing names??
-- TODO pretty instance like "union: [set, set] -> set"? just "union" for now
data Function = Function
  { fName     :: String     -- ^ main (prefix) function name
  , fOpChar   :: Maybe Char -- ^ infix operator symbol, if any
  , fInputs   :: [TypeSig]  -- ^ new input (argument) types TODO any way to make it a variable length tuple?
  , fOutput   ::  TypeSig   -- ^ new output (return) type
  , fTags     :: [FnTag]    -- ^ function tags (TODO implement these)
  , fOldRules :: RulesFn    -- ^ old-style rules (TODO deprecate, then remove)
  , fNewRules :: NewRules   -- ^ new-style rules (TODO write them all, then remove the Maybe)
  }

-- TODO is this safe enough?
instance Eq Function where
  f1 == f2 = fName   f1 == fName   f2
          && fOpChar f1 == fOpChar f2
          && fInputs f1 == fInputs f2
          && fOutput f1 == fOutput f2
          && fTags   f1 == fTags   f2

instance Show Function where
  show = prettyShow

-- TODO change this to something useful
instance Pretty Function where
  pPrint fn = PP.text $ "Function \"" ++ fName fn ++ "\""

data NewRules
  = NewRules (Rules ())
  | NewMacro (Script -> Expr -> Expr) -- type alias in NewRules.hs for now
  | NewNotImplemented -- TODO remove

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

----------
-- repl --
----------

-- TODO config always first? or make a record + ask* fns
-- TODO can script be removed?
-- TODO rename ReplEnv? EvalEnv?
type GlobalEnv = (Script, Config, LocksRef, IDsRef, DigestsRef)

type ReplM = StateT GlobalEnv IO

runReplM :: Settings ReplM -> InputT ReplM (Maybe String) -> GlobalEnv -> IO ()
runReplM settings myLoop st@(_, cfg, _, _, _) = do
  _ <- execStateT (runInputT settings myLoop) st
  return ()

type ReplCmd = [Module] -> GlobalEnv -> Handle -> String -> IO GlobalEnv


---------------------
-- pretty-printing --
---------------------

-- TODO move to config?
getWidth :: IO Int
getWidth = do
  s <- size
  return $ case s of
    Nothing -> 120
    Just (Window {width = w}) -> w

-- Render with my custom style (just width so far)
-- Needs to have the optional constant width for the REPL tests
renderIO :: Config -> Doc -> IO String
renderIO cfg doc = do
  currentWidth <- getWidth
  let renderWidth = case termcolumns cfg of
                      Nothing -> currentWidth
                      Just w  -> w
  let s = PP.style {PP.lineLength = renderWidth, PP.ribbonsPerLine = 1.0}
  -- let s = style {lineLength = renderWidth}
  return $ PP.renderStyle s doc

-- Print something pretty to a handle, rendering with custom style from Pretty.hs
-- TODO move to Pretty.hs?
pPrintHdl :: Pretty a => Config -> Handle -> a -> IO ()
pPrintHdl cfg hdl thing = renderIO cfg (pPrint thing) >>= hPutStrLn hdl
