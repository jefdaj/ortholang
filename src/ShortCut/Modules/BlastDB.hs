module ShortCut.Modules.BlastDB where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Compile   (cExpr, toShortCutListStr)
import ShortCut.Core.Config    (wrappedCmd)
import ShortCut.Core.Debug     (debugReadFile, debugTrackWrite)
import ShortCut.Core.ModuleAPI (mkLoad, mkLoadList, defaultTypeCheck)
import ShortCut.Core.Paths     (exprPath, cacheDir)
import ShortCut.Modules.SeqIO  (faa, fna)
import System.FilePath         (takeBaseName, takeExtension, (<.>))
import System.Directory        (createDirectoryIfMissing)
import System.FilePath.Glob    (compile, globDir1)
import Data.List               (isInfixOf)
import Data.Char               (toLower)
-- import System.Exit             (ExitCode(..))

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

-- TODO how to handle the prefix issue? guess we have to check + touch here?
--      or should i expect one compressed file instead?

loadNuclDB :: CutFunction
loadNuclDB = mkLoad "load_nucl_db" ndb

loadProtDB :: CutFunction
loadProtDB = mkLoad "load_prot_db" pdb

loadNuclDBEach :: CutFunction
loadNuclDBEach = mkLoadList "load_nucl_db_each" ndb

loadProtDBEach :: CutFunction
loadProtDBEach = mkLoadList "load_prot_db_each" pdb

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
  oPath %> \_ -> do
    (Exit _, Stdout out) <- wrappedCmd cfg [] "blastdbget" [tmpDir]
    -- TODO should this strip newlines on its own? seems important
    filterStr <- debugReadFile cfg fPath
    let names = if null filterStr || null out then []
                else filterNames (init filterStr) (tail $ lines out)
    toShortCutListStr cfg str (ExprPath oPath) names
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

-- TODO abort on nonzero exit code, which seems to happen a lot!
--      wait, is the check just broken? maybe remove it
cBlastdbget :: RulesFn
cBlastdbget st@(_,cfg) e@(CutFun _ _ _ _ [name]) = do
  (ExprPath nPath) <- cExpr st name
  let (CacheDir tmpDir  ) = cacheDir cfg "blastdbget"
      (ExprPath dbPrefix) = exprPath cfg e [] -- final prefix
  dbPrefix %> \_ -> do
    need [nPath]
    dbName <- debugReadFile cfg nPath -- TODO need to strip?
    liftIO $ createDirectoryIfMissing True tmpDir -- TODO remove?
    -- Exit code <- quietly $ wrappedCmd cfg [Cwd tmpDir]
    unit $ quietly $ wrappedCmd cfg [Cwd tmpDir]
      "blastdbget" ["-d", "taxdb", "-d", dbName, tmpDir]

    -- case code of
      -- ExitFailure n -> error $ "blastdbget reported error code " ++ show n
      -- ExitSuccess -> do

    -- Final dbPrefix has to be independent of the name because we don't
    -- know the name during compilation. So I guess the thing to do is link
    -- all the db files next to their final location and touch the prefix
    -- itself.
    dbFiles <- liftIO $ globDir1 (compile $ dbName ++ ".*") tmpDir
    mapM_ (\f -> linkDBFile cfg f dbPrefix) dbFiles
    unit $ quietly $ wrappedCmd cfg [] "touch" [dbPrefix]
    debugTrackWrite cfg [dbPrefix]

  return (ExprPath dbPrefix)
cBlastdbget _ _ = error "bad argument to cBlastdbget"

linkDBFile :: CutConfig -> FilePath -> FilePath -> Action ()
linkDBFile cfg dbf prefix =
  unit $ quietly $ wrappedCmd cfg [] "ln" ["-fs", dbf, dst]
  where
    dst = prefix <.> takeExtension dbf

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

{- There are a few types of BLAST database files. For nucleic acids:
 - <prefix>.nhr, <prefix>.nin, <prefix>.nog, ...
 -
 - And for proteins:
 - <prefix>.phr, <prefix>.pin, <prefix>.pog, ...
 -
 - The BLAST programs just expect to be passed the prefix, which is fine for
 - most purposes but difficult in Shake; since it's not actually a file Shake
 - will complain that the Action failed to generate it. My hacky solution for
 - now is just to `touch` the prefix itself. BLAST doesn't seem to mind one
 - extra file, and Shake doesn't mind several.
 -
 - TODO does it work properly when the input fasta file changes and the database
 -      needs to be rebuilt?
 -}
cMakeblastdb :: RulesFn
cMakeblastdb s@(_,cfg) (CutFun rtn _ _ _ [fa]) = do
  (ExprPath faPath) <- cExpr s fa
  let (ExprPath dbPrefix) = exprPath cfg fa []
      dbType = if rtn == ndb then "nucl" else "prot"
  dbPrefix %> \_ -> do
    need [faPath]
    unit $ quietly $ wrappedCmd cfg [] "makeblastdb"
      [ "-in"    , faPath
      , "-out"   , dbPrefix
      , "-title" , takeBaseName dbPrefix -- TODO does this make sense?
      , "-dbtype", dbType
      ]
    unit $ quietly $ wrappedCmd cfg [] "touch" [dbPrefix]
    debugTrackWrite cfg [dbPrefix]
  return (ExprPath dbPrefix)
cMakeblastdb _ _ = error "bad argument to makeblastdb"
