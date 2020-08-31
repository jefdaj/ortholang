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
  , Type   -- query fasta type
  , Type   -- subject type when starting from fasta
  , String -- subject type when starting from db
  )

blastDescs :: [BlastDesc]
blastDescs =
  [ ( "blastn"   , fna, fna, "nucl") -- old blastn default for more distant sequences
  , ( "megablast", fna, fna, "nucl") -- new blastn default (highly similar sequences)
  , ( "blastp"   , faa, faa, "prot")
  , ( "blastx"   , fna, faa, "prot")
  , ("tblastn"   , faa, fna, "nucl")
  , ("tblastx"   , fna, fna, "nucl")
  ]

----------------
-- *blast*_db --
----------------

mkBlastFromDb :: BlastDesc -> Function
mkBlastFromDb (bCmd, qType, _, np) =
  let dbType = if np == "nucl" then fna else faa
  in newFnA3
       (bCmd ++ "_db")
       (Exactly num, Exactly qType, Exactly (EncodedAs blastdb dbType))
       (Exactly bht)
       (aMkBlastFromDb2 bCmd)
       [Nondeterministic]

-- TODO remove tmp?
-- rMkBlastFromDb :: BlastDesc -> RulesFn
-- rMkBlastFromDb (bCmd, _, _, _) = rSimple $ aMkBlastFromDb bCmd

-- TODO unify with aMkBlastFromDb below when ready
aMkBlastFromDb2 :: String -> NewAction3
aMkBlastFromDb2 bCmd (ExprPath o') e' q' p' = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.blast.aMkBlastFromDb2"
      o = toPath loc cfg o'
      q = toPath loc cfg q'
      p = toPath loc cfg p'
      e = toPath loc cfg e'
  aMkBlastFromDb bCmd [o, e, q, p]

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
  need' loc [prefix']
  liftIO $ removeIfExists ref stdoutPath
  liftIO $ removeIfExists ref stderrPath
  runCmd $ CmdDesc
    { cmdBinary = "blast.sh"
    , cmdArguments = [stdoutPath, bCmd', eDec, q', prefix']
    , cmdFixEmpties = False
    , cmdParallel = False -- TODO make it parallel again?
    , cmdOptions = []
    , cmdInPatterns = [ptn]
    , cmdNoNeedDirs = []
    , cmdOutPath = o''
    , cmdExtraOutPaths = [] -- TODO stderrPath?
    , cmdSanitizePaths = []
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [o'' ++ "*", stdoutPath, stderrPath]
    }
  -- symlink o (toPath loc cfg stdoutPath) -- TODO reverse it? move/copy instead?
aMkBlastFromDb _ _ = error $ "bad argument to aMkBlastFromDb"

-------------
-- *blast* --
-------------

mkBlastFromFa :: BlastDesc -> Function
mkBlastFromFa d@(bCmd, qType, faType, _) = newExprExpansion
  bCmd
  [Exactly num, Exactly qType, Exactly faType]
  (Exactly bht)
  (mMkBlastFromFa d)
  [Nondeterministic]

mMkBlastFromFa :: BlastDesc -> ExprExpansion
mMkBlastFromFa (bCmd, _, _, np) _ _ (Fun r ms ds _ [e,q,s]) = Fun r ms ds name1 [e,q,expr]
  where
    dbType = if np == "nucl" then fna else faa
    name1 = bCmd ++ "_db"
    name2 = "makeblastdb_" ++ np -- TODO would the _all version withSingleton arg be better?
    expr = Fun (EncodedAs blastdb dbType) Nothing (depsOf s) name2 [s]
mMkBlastFromFa _ _ _ e = error "ortholang.modules.blast.mkBlastFromFa" $ "bad arg: " ++ show e


---------------------
-- *blast*_db_each --
---------------------

-- TODO why no re-running bug here?
mkBlastFromDbEach :: BlastDesc -> Function
mkBlastFromDbEach (bCmd, qType, _, np) =
  let dbType = if np == "nucl" then fna else faa
  in newFnA3
       (bCmd ++ "_db_each")
       (Exactly num, Exactly qType, Exactly (ListOf (EncodedAs blastdb dbType)))
       (Exactly (ListOf bht))
       (newMap3of3 $ bCmd ++ "_db")
       [Nondeterministic]


------------------
-- *blast*_each --
------------------

-- TODO find the re-running bug here
mkBlastFromFaEach :: BlastDesc -> Function
mkBlastFromFaEach d@(bCmd, qType, faType, _) = newExprExpansion
  (bCmd ++ "_each")
  [Exactly num, Exactly qType, Exactly (ListOf faType)]
  (Exactly (ListOf bht))
  (mBlastFromFaEach d)
  [Nondeterministic]

mBlastFromFaEach :: BlastDesc -> ExprExpansion
mBlastFromFaEach d@(_, _, _, np) _ _ (Fun rtn seed deps _   [e, q, ss])
  =                                  (Fun rtn seed deps fn2 [e, q, ss'])
  where
    dbType = if np == "nucl" then fna else faa
    ss'   = Fun (ListOf (EncodedAs blastdb dbType)) Nothing (depsOf ss) fn1 [ss]
    fn1   = "makeblastdb_" ++ np ++ "_each"
    fn2   = (fName $ mkBlastFromFa d) ++ "_db_each"
mBlastFromFaEach _ _ _ e = error "ortholang.modules.blast.mkBlastFromFaEach" $  "bad arg: " ++ show e
