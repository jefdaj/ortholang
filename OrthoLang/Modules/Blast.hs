module OrthoLang.Modules.Blast
  ( olModule
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
import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Modules.SeqIO   (faa, fna, mkConcat, mkConcatEach)
import OrthoLang.Modules.BlastDB (blastdb)

import Data.Scientific           (formatScientific, FPFormat(..))
-- import OrthoLang.Modules.BlastDB (ndb, pdb) -- TODO import rMakeBlastDB too?
import System.Exit               (ExitCode(..))
import System.FilePath           (replaceBaseName)
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "BLAST+"
  , mDesc = "Standard NCBI BLAST+ functions"
  , mTypes = [bht] -- TODO ndb, pdb?
  , mGroups = []
  , mEncodings = [blastdb]
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
  [ ( "blastn"   , fna, fna, fna) -- old blastn default for more distant sequences
  , ( "megablast", fna, fna, fna) -- new blastn default (highly similar sequences)
  , ( "blastp"   , faa, faa, faa)
  , ( "blastx"   , fna, faa, faa)
  , ("tblastn"   , faa, fna, fna)
  , ("tblastx"   , fna, fna, fna)
  ]

----------------
-- *blast*_db --
----------------

mkBlastFromDb :: BlastDesc -> Function
mkBlastFromDb d@(bCmd, qType, _, sType) = Function
  { fOpChar = Nothing
  , fName = bCmd ++ "_db"
  , fInputs = [Exactly num, Exactly qType, Exactly (EncodedAs blastdb sType)]
  , fOutput = Exactly bht
  , fTags = [Nondeterministic]
  , fNewRules = NewNotImplemented, fOldRules = rMkBlastFromDb d
  }

-- TODO remove tmp?
rMkBlastFromDb :: BlastDesc -> RulesFn
rMkBlastFromDb (bCmd, _, _, _) = rSimple $ aMkBlastFromDb bCmd

aMkBlastFromDb :: String -> ([Path] -> Action ())
aMkBlastFromDb bCmd [o, e, q, p] = do
  cfg <- fmap fromJust getShakeExtra
  let o'  = fromPath loc cfg o
      q'  = fromPath loc cfg q
      p'  = fromPath loc cfg p
      e'  = fromPath loc cfg e
      loc = "modules.blast.aMkBlastFromDb"
      o'' = traceA loc o' [bCmd, e', o', q', p']
  eStr   <- readLit  loc e'
  prefix <- readPath loc p'
  let eDec    = formatScientific Fixed Nothing (read eStr) -- format as decimal
      prefix' = fromPath loc cfg prefix
      -- cDir    = tmpdir cfg </> takeDirectory prefix' -- TODO remove?
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
  ref <- fmap fromJust getShakeExtra
  liftIO $ removeIfExists ref stdoutPath
  liftIO $ removeIfExists ref stderrPath
  runCmd $ CmdDesc
    { cmdBinary = "blast.sh"
    , cmdArguments = [stdoutPath, bCmd', eDec, q', prefix']
    , cmdFixEmpties = False
    , cmdParallel = False -- TODO make it parallel again?
    , cmdOptions = []
    , cmdInPatterns = [ptn]
    , cmdOutPath = o''
    , cmdExtraOutPaths = [] -- TODO stderrPath?
    , cmdSanitizePaths = []
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [o'' ++ "*", stdoutPath, stderrPath]
    }
  symlink o (toPath loc cfg stdoutPath)
aMkBlastFromDb _ _ = error $ "bad argument to aMkBlastFromDb"

-------------
-- *blast* --
-------------

mkBlastFromFa :: BlastDesc -> Function
mkBlastFromFa d@(bCmd, qType, sType, _) = Function
  { fOpChar = Nothing
  , fName = bCmd
  , fInputs = [Exactly num, Exactly qType, Exactly sType]
  , fOutput = Exactly bht
  , fTags = [Nondeterministic]
  , fNewRules = NewNotImplemented, fOldRules = rMkBlastFromFa d
  }

-- inserts a "makeblastdb" call and reuses the _db compiler from above
-- TODO check this works after writing the new non- _all makeblastdb fns
rMkBlastFromFa :: BlastDesc -> RulesFn
rMkBlastFromFa d@(_, _, _, sType) st (Fun rtn seed deps _ [e, q, s])
  = rules st (Fun rtn seed deps name1 [e, q, dbExpr])
  where
    rules = fOldRules $ mkBlastFromDb d
    name1 = fName $ mkBlastFromDb d
    name2 = "makeblastdb_" ++ ext sType
    dbExpr = Fun (EncodedAs blastdb sType) seed (depsOf s) name2 [s] 
rMkBlastFromFa _ _ _ = fail "bad argument to rMkBlastFromFa"

---------------------
-- *blast*_db_each --
---------------------

mkBlastFromDbEach :: BlastDesc -> Function
mkBlastFromDbEach d@(bCmd, qType, _, sType) = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly num, Exactly qType, Exactly (ListOf (EncodedAs blastdb sType))]
  , fOutput = Exactly (ListOf bht)
  , fTags = [Nondeterministic]
  , fNewRules = NewNotImplemented, fOldRules = rMkBlastFromDbEach d
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
  , fInputs = [Exactly num, Exactly qType, Exactly (ListOf faType)]
  , fOutput = Exactly (ListOf bht)
  , fTags = [Nondeterministic]
  , fNewRules = NewNotImplemented, fOldRules = rMkBlastFromFaEach d
  }
  where
    name = bCmd ++ "_each"

-- combination of the two above: insert the makeblastdbcall, then map
rMkBlastFromFaEach :: BlastDesc -> RulesFn
rMkBlastFromFaEach d@(_, _, _, sType) st (Fun rtn seed deps _   [e, q, ss])
  =                              rules st (Fun rtn seed deps fn2 [e, q, ss'])
  where
    rules = rMkBlastFromDbEach d
    ss'   = Fun (ListOf (EncodedAs blastdb sType)) seed (depsOf ss) fn1 [ss]
    fn1   = "makeblastdb_" ++ ext sType ++ "_each"
    fn2   = (fName $ mkBlastFromFa d) ++ "_db_each"
rMkBlastFromFaEach _ _ _ = fail "bad argument to rMkBlastFromFaEach"
