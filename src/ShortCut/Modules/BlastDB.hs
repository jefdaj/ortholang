module ShortCut.Modules.BlastDB where

import Development.Shake
import ShortCut.Core.Types

-- import Control.Monad           (when)
import ShortCut.Core.Compile   (cExpr)
import ShortCut.Core.Config    (wrappedCmd)
import ShortCut.Core.Debug     (debugReadFile, debugWriteFile, debugReadLines,
                                debugWriteLines)
import ShortCut.Core.ModuleAPI (defaultTypeCheck)
import ShortCut.Core.Paths     (exprPath, exprPathExplicit, cacheDir)
import ShortCut.Core.Util      (stripWhiteSpace)
import ShortCut.Modules.SeqIO  (faa, fna)
import System.FilePath         (takeFileName, takeExtension, (</>), (<.>),
                                makeRelative)
import System.Directory        (createDirectoryIfMissing)
-- import System.FilePath.Glob    (compile, globDir1)
import Data.List               (isInfixOf)
import Data.Char               (toLower)
-- import System.Exit             (ExitCode(..))

{- There are a few types of BLAST database files. For nucleic acids:
 - <prefix>.nhr, <prefix>.nin, <prefix>.nog, ...
 -
 - And for proteins:
 - <prefix>.phr, <prefix>.pin, <prefix>.pog, ...
 -
 - The BLAST programs just expect to be passed the prefix, which is fine for
 - most purposes but difficult in Shake; since it's not actually a file Shake
 - will complain that the Action failed to generate it. My solution for
 - now is to make a text file with the prefix pattern in it. The contents are
 - passed to BLAST functions.
 -
 - TODO does it work properly when the input fasta file changes and the database
 -      needs to be rebuilt?
 -}

cutModule :: CutModule
cutModule = CutModule
  { mName = "blastdb"
  , mFunctions =
    [ loadNuclDB
    , loadProtDB
    , loadNuclDBEach
    , loadProtDBEach
    , makeblastdb
    , blastdbget -- TODO mapped version so you can list -> git at once?
    , blastdblist
    -- , TODO write loadBlastDB
    ]
  }

ndb :: CutType
ndb = CutType
  { tExt  = "ndb"
  , tDesc = "BLAST nucleotide database"
  , tShow  = \f -> return $ "BLAST nucleotide database " ++ f
  }

-- TODO will people confuse this with PDB files for viewing molecules?
pdb :: CutType
pdb = CutType
  { tExt  = "pdb"
  , tDesc = "BLAST protein database"
  , tShow  = \f -> return $ "BLAST protein database " ++ f
  }

---------------------
-- load from files --
---------------------

{- These are a little different from normal, because rather than linking
 - directly to a database file (there isn't one!), they create separate text
 - files that you can read to get the proper prefix pattern.
 -}

mkLoadDB :: String -> CutType -> CutFunction
mkLoadDB name rtn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str] rtn
  , fFixity    = Prefix
  , fCompiler  = cLoadDB
  }

mkLoadDBEach :: String -> CutType -> CutFunction
mkLoadDBEach name rtn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf str] (ListOf rtn)
  , fFixity    = Prefix
  , fCompiler  = undefined -- TODO write this!
  }

cLoadDB :: RulesFn
cLoadDB st@(_,cfg) e@(CutFun _ _ _ _ [s]) = do
  (ExprPath sPath) <- cExpr st s
  let (ExprPath oPath) = exprPath cfg e []
  oPath %> \_ -> do
    pattern <- debugReadFile cfg sPath
    let pattern' = makeRelative (cfgTmpDir cfg) pattern
    debugWriteFile cfg oPath pattern'
  return (ExprPath oPath)
cLoadDB _ _ = error "bad argument to cLoadDB"

loadNuclDB :: CutFunction
loadNuclDB = mkLoadDB "load_nucl_db" ndb

loadProtDB :: CutFunction
loadProtDB = mkLoadDB "load_prot_db" pdb

loadNuclDBEach :: CutFunction
loadNuclDBEach = mkLoadDBEach "load_nucl_db_each" ndb

loadProtDBEach :: CutFunction
loadProtDBEach = mkLoadDBEach "load_prot_db_each" pdb

------------------------
-- download from NCBI --
------------------------

-- takes a filter string
blastdblist :: CutFunction
blastdblist = CutFunction
  { fName      = "blastdblist"
  , fTypeCheck = defaultTypeCheck [str] (ListOf str)
  , fFixity    = Prefix
  , fCompiler  = cBlastdblist
  }

filterNames :: String -> [String] -> [String]
filterNames s cs = filter matchFn cs
  where
    matchFn c = (map toLower s) `isInfixOf` (map toLower c)

cBlastdblist :: RulesFn
cBlastdblist s@(_,cfg) e@(CutFun _ _ _ _ [f]) = do
  (ExprPath fPath) <- cExpr s f
  let (CacheDir tmpDir) = cacheDir cfg "blastdbget"
      (ExprPath oPath ) = exprPath cfg e []
      stdoutTmp = tmpDir </> "stdout" <.> "txt"
  oPath %> \_ -> do
    wrappedCmd cfg [oPath] [Shell] "blastdbget" [tmpDir, ">", fPath]
    -- TODO should this strip newlines on its own? seems important
    filterStr <- debugReadFile  cfg fPath
    out       <- debugReadLines cfg stdoutTmp
    let names = if null filterStr || null out then []
                else filterNames (init filterStr) (tail out)
    -- toShortCutListStr cfg str (ExprPath oPath) names
    debugWriteLines cfg oPath names
  return (ExprPath oPath)
cBlastdblist _ _ = error "bad argument to cBlastdblist"

-- TODO do I need to adjust the timeout? try on the cluster first
blastdbget :: CutFunction
blastdbget = CutFunction
  { fName      = "blastdbget"
  , fTypeCheck = defaultTypeCheck [str] ndb -- TODO are there protein ones too?
  , fFixity    = Prefix
  , fCompiler  = cBlastdbget
  }

cBlastdbget :: RulesFn
cBlastdbget st@(_,cfg) e@(CutFun _ _ _ _ [name]) = do
  (ExprPath nPath) <- cExpr st name
  let (CacheDir tmpDir  ) = cacheDir cfg "blastdbget"
      (ExprPath dbPrefix) = exprPath cfg e [] -- final prefix
  dbPrefix %> \_ -> do
    need [nPath]
    dbName <- fmap stripWhiteSpace $ debugReadFile cfg nPath -- TODO need to strip?
    liftIO $ createDirectoryIfMissing True tmpDir -- TODO remove?
    unit $ quietly $ wrappedCmd cfg [dbPrefix ++ ".*"] [Cwd tmpDir]
      "blastdbget" ["-d", dbName, "."] -- TODO was taxdb needed for anything else?
    debugWriteFile cfg dbPrefix $ (tmpDir </> dbName) ++ "\n"
  return (ExprPath dbPrefix)
cBlastdbget _ _ = error "bad argument to cBlastdbget"

-- TODO is this actually used anywhere?
linkDBFile :: CutConfig -> FilePath -> FilePath -> Action ()
linkDBFile cfg dbf prefix =
  unit $ quietly $ wrappedCmd cfg [dst, dst ++ ".*"] [] "ln" ["-fs", dbf, dst]
  where
    dst  = prefix <.> takeExtension dbf

--------------------------
-- make from FASTA file --
--------------------------

-- TODO silence output?
-- TODO does this have an error where db path depends on the outer expression
--      in addition to actual inputs?
makeblastdb :: CutFunction
makeblastdb = CutFunction
  { fName      = "makeblastdb"
  , fTypeCheck = tMakeblastdb
  , fFixity    = Prefix
  , fCompiler  = cMakeblastdb
  }

tMakeblastdb :: TypeChecker
tMakeblastdb [x]
  | x == fna = Right ndb
  | x == faa = Right pdb
tMakeblastdb _ = error "makeblastdb requires a fasta file"

-- TODO why does this get rebuilt one extra time, but *only* one?
cMakeblastdb :: RulesFn
cMakeblastdb s@(_,cfg) (CutFun rtn _ _ _ [fa]) = do
  (ExprPath faPath) <- cExpr s fa
  let (ExprPath dbPrefix) = exprPathExplicit cfg rtn "makeblastdb" [faPath]
      dbPrefixRel = makeRelative (cfgTmpDir cfg) dbPrefix
      dbType = if rtn == ndb then "nucl" else "prot"
  dbPrefix %> \_ -> do
    need [faPath]
    quietly $ wrappedCmd cfg [dbPrefix, dbPrefix ++ ".*"] [] "makeblastdb"
      [ "-in"    , faPath
      , "-out"   , dbPrefix
      , "-title" , takeFileName dbPrefix -- TODO does this make sense?
      , "-dbtype", dbType
      ]
    -- TODO put back if you can figure out how with the new wrappedCmd
    -- when (cfgDebug cfg) (liftIO $ putStrLn $ out)
    debugWriteFile cfg dbPrefix dbPrefixRel
  return (ExprPath dbPrefix)
cMakeblastdb _ _ = error "bad argument to makeblastdb"
