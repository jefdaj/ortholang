module OrthoLang.Modules.Blast
  ( orthoLangModule
  , bht
  -- the rest is for blastrbh, which is pretty intimately related:
  , BlastDesc
  , blastDescs
  , mkBlastFromFa
  , aMkBlastFromDb
  )
  where

-- TODO _all versions to go with the _each ones?
-- TODO can i reuse most of this code for DIAMOND?

import Development.Shake
import OrthoLang.Core

import Data.Scientific           (formatScientific, FPFormat(..))
import OrthoLang.Modules.BlastDB (ndb, pdb) -- TODO import rMakeBlastDB too?
import OrthoLang.Modules.SeqIO   (faa, fna, mkConcat, mkConcatEach)
import System.Exit               (ExitCode(..))
import System.FilePath           (replaceBaseName)

orthoLangModule :: Module
orthoLangModule = Module
  { mName = "BLAST+"
  , mDesc = "Standard NCBI BLAST+ functions"
  , mTypes = [ndb, pdb, bht]
  , mFunctions =
    -- TODO remove the ones that don't apply to each fn type!
    -- TODO psiblast, dbiblast, deltablast, rpsblast, rpsblastn?
    map mkBlastFromFa     blastDescs ++
    map mkBlastFromFaEach blastDescs ++
    map mkBlastFromDb     blastDescs ++
    map mkBlastFromDbEach blastDescs ++
    [mkConcat bht, mkConcatEach bht]
  }

-- tsv with these columns:
-- qseqid sseqid pident length mismatch gapopen
-- qstart qend sstart send evalue bitscore
bht :: Type
bht = Type
  { tExt  = "bht"
  , tDesc = "tab-separated table of blast hits (outfmt 6)"
  , tShow  = defaultShow
  }

-- TODO need a separate db type for reverse fns?
type BlastDesc =
  ( String  -- name and also system command to call
  , Type -- query fasta type
  , Type -- subject type when starting from fasta
  , Type -- subject type when starting from db
  )

blastDescs :: [BlastDesc]
blastDescs =
  [ ( "blastn"   , fna, fna, ndb) -- old blastn default for more distant sequences
  , ( "megablast", fna, fna, ndb) -- new blastn default (highly similar sequences)
  , ( "blastp"   , faa, faa, pdb)
  , ( "blastx"   , fna, faa, pdb)
  , ("tblastn"   , faa, fna, ndb)
  , ("tblastx"   , fna, fna, ndb)
  ]

----------------
-- *blast*_db --
----------------

mkBlastFromDb :: BlastDesc -> Function
mkBlastFromDb d@(bCmd, qType, _, dbType) = Function
  { fOpChar = Nothing
  , fName = name
  , fTypeCheck = defaultTypeCheck name [num, qType, dbType] bht
  , fTypeDesc  = mkTypeDesc name  [num, qType, dbType] bht
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMkBlastFromDb d
  }
  where
    name = bCmd ++ "_db"

-- TODO remove tmp?
rMkBlastFromDb :: BlastDesc -> RulesFn
rMkBlastFromDb (bCmd, _, _, _) = rSimple $ aMkBlastFromDb bCmd

aMkBlastFromDb :: String -> ([Path] -> Action ())
aMkBlastFromDb bCmd cfg ref _ [o, e, q, p] = do
  eStr   <- readLit cfg ref e'
  prefix <- readPath cfg ref p'
  let eDec    = formatScientific Fixed Nothing (read eStr) -- format as decimal
      prefix' = fromPath cfg prefix
      -- cDir    = cfgTmpDir cfg </> takeDirectory prefix' -- TODO remove?
      ptn     = prefix' ++ "*"
      -- args    = [ "-db", takeFileName prefix'
      --           , "-evalue", eDec
      --           , "-outfmt", "6" -- tab-separated values
      --           , "-query", "-"
      --           ]
      -- NCBI defaults to megablast for speed at the expense of sensitivity. My
      -- view is users should have to ask for that explicitly, so in OrthoLang
      -- "blastn" is actual blastn! Use "megablast" if you want faster results.
      -- See http://www.sixthresearcher.com/when-blast-is-not-blast/
      bCmd' = case bCmd of
        "blastn" -> "blastn -task blastn"
        "megablast" -> "blastn"
        _ -> bCmd
      -- (bCmd', args') = case bCmd of
        -- "blastn"    -> ("blastn", ["-task","blastn"] ++ args)
        -- "megablast" -> ("blastn", args)
        -- TODO is this possible/helpful in newer version of blast?
        -- "blastp"    -> ("blastp", ["-f'm S'", "-s T"] ++ args) -- see doi:10.1093/bioinformatics/btm585
        -- _           -> (bCmd, args)
      -- Terrible hack, but seems to parallelize BLAST commands without error.
      -- It should also allow each part of the overall BLAST to be run with srun.
      -- TODO proper quoting of args' at least
      -- jobl = o'' <.> "log"
      -- pCmd = [ "parallel"
      --        , "--pipe"
      --        , "--round-robin"
      --        , "--line-buffer"
      --        -- , "-N1"
      --        , "--block-size", "100"
      --        -- , "-j8" -- TODO match number of cores
      --        -- , "--block-size", "1k"
      --        , "--joblog", jobl
      --        -- , "--resume TODO can this work without making many more tmpfiles?
      --        -- , "--resume-failed" -- TODO does this work with --pipe?
      --        , "--halt now,fail=1" -- TODO be more lax in production?
      --        , "--recstart", "'>'"
      --        -- , "-k" -- preserve order in the output (more deterministic)
      --        , "--will-cite"
      --        ]
      -- args'' = [q', "|"] ++ pCmd ++ [escape $ unwords (bCmd':args'), ">", o'']
  -- debugModule $ "args'': " ++ show args''
  -- TODO full path to prefix'?
  -- wrappedCmdWrite False True cfg ref o'' [ptn] [] [] "blast.sh" [o'', prefix', bCmd', eDec, q', p']
  -- want to be real sure not to accidentally mistake these for done:
  let stdoutPath = replaceBaseName o'' "out"
      stderrPath = replaceBaseName o'' "err"
  liftIO $ removeIfExists ref stdoutPath
  liftIO $ removeIfExists ref stderrPath
  runCmd cfg ref $ CmdDesc
    { cmdBinary = "blast.sh"
    , cmdArguments = [stdoutPath, bCmd', eDec, q', prefix']
    , cmdFixEmpties = False
    , cmdParallel = False -- TODO make it parallel again?
    , cmdOptions = []
    , cmdInPatterns = [ptn]
    , cmdOutPath = stdoutPath
    , cmdExtraOutPaths = [] -- TODO stderrPath?
    , cmdSanitizePaths = []
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [o'' ++ "*", stdoutPath, stderrPath]
    }
  symlink cfg ref o (toPath cfg stdoutPath)
  where
    o'  = fromPath cfg o
    q'  = fromPath cfg q
    p'  = fromPath cfg p
    e'  = fromPath cfg e
    o'' = traceA "aMkBlastFromDb" o' [bCmd, e', o', q', p']
aMkBlastFromDb _ _ _ _ _ = error $ "bad argument to aMkBlastFromDb"

-------------
-- *blast* --
-------------

mkBlastFromFa :: BlastDesc -> Function
mkBlastFromFa d@(bCmd, qType, sType, _) = Function
  { fOpChar = Nothing
  , fName = bCmd
  , fTypeCheck = defaultTypeCheck bCmd [num, qType, sType] bht
  , fTypeDesc  = mkTypeDesc bCmd  [num, qType, sType] bht
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMkBlastFromFa d -- TODO rewrite in new rFun3 style like Psiblast?
  }

-- inserts a "makeblastdb" call and reuses the _db compiler from above
-- TODO check this works after writing the new non- _all makeblastdb fns
rMkBlastFromFa :: BlastDesc -> RulesFn
rMkBlastFromFa d@(_, _, _, dbType) st (Fun rtn salt deps _ [e, q, s])
  = rules st (Fun rtn salt deps name1 [e, q, dbExpr])
  where
    rules = fOldRules $ mkBlastFromDb d
    name1 = fName $ mkBlastFromDb d
    name2 = "makeblastdb" ++ if dbType == ndb then "_nucl" else "_prot"
    dbExpr = Fun dbType salt (depsOf s) name2 [s] 
rMkBlastFromFa _ _ _ = fail "bad argument to rMkBlastFromFa"

---------------------
-- *blast*_db_each --
---------------------

mkBlastFromDbEach :: BlastDesc -> Function
mkBlastFromDbEach d@(bCmd, qType, _, dbType) = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [num, qType, ListOf dbType] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, qType, ListOf dbType] (ListOf bht)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMkBlastFromDbEach d
  }
  where
    name = bCmd ++ "_db_each"

rMkBlastFromDbEach :: BlastDesc -> RulesFn
rMkBlastFromDbEach (bCmd, _, _, _) = rMap 3 $ aMkBlastFromDb bCmd

------------------
-- *blast*_each --
------------------

mkBlastFromFaEach :: BlastDesc -> Function
mkBlastFromFaEach d@(bCmd, qType, faType, _) = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [num, qType, ListOf faType] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, qType, ListOf faType] (ListOf bht)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMkBlastFromFaEach d
  }
  where
    name = bCmd ++ "_each"

-- combination of the two above: insert the makeblastdbcall, then map
rMkBlastFromFaEach :: BlastDesc -> RulesFn
rMkBlastFromFaEach d@(_, _, _, dbType) st (Fun rtn salt deps _   [e, q, ss])
  =                              rules st (Fun rtn salt deps fn2 [e, q, ss'])
  where
    rules = rMkBlastFromDbEach d
    ss'   = Fun (ListOf dbType) salt (depsOf ss) fn1 [ss]
    fn1   = "makeblastdb" ++ (if dbType == ndb then "_nucl" else "_prot") ++ "_each"
    fn2   = (fName $ mkBlastFromFa d) ++ "_db_each"
rMkBlastFromFaEach _ _ _ = fail "bad argument to rMkBlastFromFaEach"
