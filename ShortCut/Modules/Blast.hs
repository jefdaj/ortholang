module ShortCut.Modules.Blast where

import Development.Shake
import ShortCut.Core.Types

import Data.Scientific            (formatScientific, FPFormat(..))
import Development.Shake.FilePath ((</>))
import ShortCut.Core.Paths        (exprPath)
import ShortCut.Core.Compile      (cExpr)
import ShortCut.Core.Debug        (debugReadFile, debugTrackWrite)
import ShortCut.Core.ModuleAPI    (defaultTypeCheck)
import ShortCut.Core.Util         (digest)
import ShortCut.Modules.SeqIO     (faa, fna)
import System.Directory           (createDirectoryIfMissing)

cutModule :: CutModule
cutModule = CutModule
  { mName = "blast"
  , mFunctions =
    [ mkBlastFn  "blastn" fna fna -- TODO why doesn't this one work??
    , mkBlastFn  "blastp" faa faa
    , mkBlastFn  "blastx" fna faa
    , mkBlastFn "tblastn" faa fna
    , mkBlastFn "tblastx" fna fna
    -- TODO expose makeblastdb?
    -- TODO vectorized versions
    -- TODO psiblast, dbiblast, deltablast, rpsblast, rpsblastn?
    -- TODO extract_queries, extract_targets
    ]
  }

{- The most straightforward way I can now think to do this is having a
 - top-level db dir cache/blastdb, and in there is a folder for each database.
 - The folder can be named by the hash of the fasta file it's made from, and
 - inside are whatever files blast wants to make.
 -
 - I don't think this needs to be exposed to ShortCut users as an actual
 - CutType yet, but that could happen at some point. Maybe it will seem more
 - like the thing to do once I get folder-based types figured out in a standard
 - way? (They'll probably come up elsewhere, like with tree-making programs) It
 - also might be needed to use the NCBI nr database; not sure yet.
 -}

-- tsv with these columns:
-- qseqid sseqid pident length mismatch gapopen
-- qstart qend sstart send evalue bitscore
bht :: CutType
bht = CutType
  { tExt  = "bht"
  , tDesc = "tab-separated table of reciprocal blast hits"
  , tCat  = defaultCat
  }

mkBlastFn :: String -> CutType -> CutType -> CutFunction
mkBlastFn cmd qType tType = CutFunction
  { fName      = cmd
  , fTypeCheck = defaultTypeCheck [qType, tType, num] bht
  , fFixity    = Prefix
  , fCompiler  = rBlast cmd
  }

-- TODO move to Util?
-- listFiles :: FilePath -> Action [FilePath]
-- listFiles dir = fmap (map (dir </>)) (getDirectoryFiles dir ["*"])

-- The extra hash command is needed to determine oDir
-- TODO silence stdout
-- rBlastDB :: CutConfig -> CutType -> FilePath -> Action FilePath
-- rBlastDB cfg faType faPath = do
--   need [faPath]
--   hash <- fmap digest $ liftIO $ readFile faPath
--   let dbDir = cacheDir cfg </> "blastdb" </> hash -- TODO need faType too?
--   liftIO $ createDirectoryIfMissing True dbDir
--   unit $ quietly $ cmd "makeblastdb" (Cwd dbDir)
--     [ "-in"    , faPath
--     , "-out"   , "db" -- TODO is this right?
--     , "-title" , hash
--     , "-dbtype", if faType == faa then "prot" else "nucl"
--     ]
--   files <- listFiles dbDir
--   debugTrackWrite cfg files
--   -- TODO need to communicate that files were written in dbDir?
--   return dbDir

-- TODO use hashed name rather than varname for better caching
-- TODO are databases unneeded, or just automatically made in the working directory?
-- see https://www.ncbi.nlm.nih.gov/books/NBK279675/
rBlast :: String -> (CutState -> CutExpr -> Rules ExprPath)
rBlast bCmd s@(_,cfg) e@(CutFun _ _ _ _ [query, subject, evalue]) = do
  (ExprPath qPath) <- cExpr s query
  (ExprPath sPath) <- cExpr s subject
  (ExprPath ePath) <- cExpr s evalue
  let (ExprPath oPath) = exprPath cfg e []
  oPath %> \_ -> do
    -- dbDir   <- rBlastDB cfg (typeOf target) sPath
    -- dbFiles <- listFiles dbDir
    need $ [qPath, sPath, ePath] -- ++ dbFiles
    eStr <- fmap init $ debugReadFile cfg ePath
    let eDec = formatScientific Fixed Nothing (read eStr) -- format as decimal
    unit $ quietly $ cmd bCmd -- (AddEnv "BLASTDB" dbDir) -- TODO Cwd?
      -- [ "-db"     , "db" -- TODO anything useful needed here?
      [ "-query"  , qPath
      , "-subject", sPath -- TODO is this different from target?
      , "-out"    , oPath
      , "-evalue" , eDec
      , "-outfmt" , "6"
      -- , "-num_threads", "4" -- TODO how to pick this? should I even use it?
      -- TODO support -remote?
      ]
    debugTrackWrite cfg [oPath]
  return (ExprPath oPath)
rBlast _ _ _ = error "bad argument to rBlast"
