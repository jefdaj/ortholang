module OrthoLang.Modules.Diamond
  where

-- TODO any point in adding a daa type?
-- TODO can't just cat the .dmnd files!

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Locks

import Data.List.Utils         (replace)
import OrthoLang.Modules.Blast (bht)
import OrthoLang.Modules.SeqIO (fna, faa)
import System.Exit             (ExitCode(..))
import System.FilePath         (replaceBaseName)
import System.Process          (readProcess)
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "Diamond"
  , mDesc = "Accelerated BLAST compatible local sequence aligner."
  , mTypes = [fna, faa, dmnd]
  , mGroups = []
  , mEncodings = []
  , mFunctions =
      [ diamondmakedb
      , diamondmakedbEach
      , diamondmakedbAll
      ]
      ++ map mkDiamondBlast variants -- includes the _each ones too
  }

-- TODO figure out how to prettyCat/show/whatever the encoded types, probably with a typeclass
dmnd :: Type
dmnd = Type
  { tExt  = "dmnd"
  , tDesc = "DIAMOND database"
  , tShow = \_ ref path -> do
      path' <- resolveSymlinks Nothing path
      out <- withReadLock ref path' $ readProcess "diamond_dbinfo.sh" [path'] []
      let d = unlines $ ("DIAMOND database " ++ path) : (drop 4 $ lines out)
      return d
  }

--------------------
-- diamond_makedb --
--------------------

diamondmakedb :: Function
diamondmakedb = let name = "diamond_makedb" in Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly faa]
  , fOutput = Exactly dmnd
  , fTags = [Nondeterministic] -- TODO double check: is it deterministic?
  , fNewRules = NewNotImplemented, fOldRules = rSimpleScriptPar "diamond_makedb.sh"
  }

-------------------------
-- diamond_makedb_each --
-------------------------

-- TODO does this only work when --debug is disabled because of a Map .args issue?

diamondmakedbEach :: Function
diamondmakedbEach = let name = "diamond_makedb_each" in Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly (ListOf faa)]
  , fOutput = Exactly (ListOf dmnd)
  , fTags = [Nondeterministic] -- TODO double check: is it deterministic?
  , fNewRules = NewNotImplemented, fOldRules = rMapSimpleScript 1 "diamond_makedb.sh"
  }
 
------------------------
-- diamond_makedb_all --
------------------------

diamondmakedbAll :: Function
diamondmakedbAll = let name = "diamond_makedb_all" in Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly (ListOf faa)]
  , fOutput = Exactly dmnd
  , fTags = [Nondeterministic] -- TODO double check: is it deterministic?
  , fNewRules = NewNotImplemented, fOldRules = rDiamondmakedbAll
  }

-- TODO should the reading the list + paths thing be included in rSimpleScript?
rDiamondmakedbAll :: RulesFn
rDiamondmakedbAll scr e@(Fun _ _ _ _ [fas]) = do
  (ExprPath fasPath) <- rExpr scr fas
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let out  = exprPath cfg dRef scr e
      loc = "modules.diamond.aDiamondmakedbAll"
      out' = debugRules loc e $ fromPath loc cfg out
  out' %> \_ -> do
    faPaths <- readPaths loc fasPath
    aSimpleScriptPar "diamond_makedb_all.sh" (out:faPaths)
  return (ExprPath out')
rDiamondmakedbAll _ e = error $ "bad argument to rDiamondmakedbAll: " ++ show e
 
--------------------
-- diamond_blast* --
--------------------

-- type RulesFn     = RulesFn
type DiamondBlastDesc = (String, [String] -> RulesFn, [String], Type, Type, Type)

-- TODO can some of these be replaced by a numeric sensitivity arg?
variants :: [DiamondBlastDesc]
variants =
  [ ("blastp"                       , rDiamondFromFa, ["blastp"                    ], faa, faa , bht)
  , ("blastp_sensitive"             , rDiamondFromFa, ["blastp", "--sensitive"     ], faa, faa , bht)
  , ("blastp_more_sensitive"        , rDiamondFromFa, ["blastp", "--more-sensitive"], faa, faa , bht)
  , ("blastx"                       , rDiamondFromFa, ["blastx"                    ], fna, faa , bht)
  , ("blastx_sensitive"             , rDiamondFromFa, ["blastx", "--sensitive"     ], fna, faa , bht)
  , ("blastx_more_sensitive"        , rDiamondFromFa, ["blastx", "--more-sensitive"], fna, faa , bht)

  , ("blastp_rev"                   , rFlip23 . rDiamondFromFa, ["blastp"                    ], faa, faa , bht)
  , ("blastp_sensitive_rev"         , rFlip23 . rDiamondFromFa, ["blastp", "--sensitive"     ], faa, faa , bht)
  , ("blastp_more_sensitive_rev"    , rFlip23 . rDiamondFromFa, ["blastp", "--more-sensitive"], faa, faa , bht)
  , ("blastx_rev"                   , rFlip23 . rDiamondFromFa, ["blastx"                    ], faa, fna , bht)
  , ("blastx_sensitive_rev"         , rFlip23 . rDiamondFromFa, ["blastx", "--sensitive"     ], faa, fna , bht)
  , ("blastx_more_sensitive_rev"    , rFlip23 . rDiamondFromFa, ["blastx", "--more-sensitive"], faa, fna , bht)

  , ("blastp_db"                    , rSimple . aDiamondFromDb, ["blastp"                    ], faa, dmnd, bht)
  , ("blastp_db_sensitive"          , rSimple . aDiamondFromDb, ["blastp", "--sensitive"     ], faa, dmnd, bht)
  , ("blastp_db_more_sensitive"     , rSimple . aDiamondFromDb, ["blastp", "--more-sensitive"], faa, dmnd, bht)
  , ("blastx_db"                    , rSimple . aDiamondFromDb, ["blastx"                    ], fna, dmnd, bht)
  , ("blastx_db_sensitive"          , rSimple . aDiamondFromDb, ["blastx", "--sensitive"     ], fna, dmnd, bht)
  , ("blastx_db_more_sensitive"     , rSimple . aDiamondFromDb, ["blastx", "--more-sensitive"], fna, dmnd, bht)

  , ("blastp_db_rev"                , rFlip23 . rSimple . aDiamondFromDb, ["blastp"                    ], dmnd, faa, bht)
  , ("blastp_db_sensitive_rev"      , rFlip23 . rSimple . aDiamondFromDb, ["blastp", "--sensitive"     ], dmnd, faa, bht)
  , ("blastp_db_more_sensitive_rev" , rFlip23 . rSimple . aDiamondFromDb, ["blastp", "--more-sensitive"], dmnd, faa, bht)
  , ("blastx_db_rev"                , rFlip23 . rSimple . aDiamondFromDb, ["blastx"                    ], dmnd, fna, bht)
  , ("blastx_db_sensitive_rev"      , rFlip23 . rSimple . aDiamondFromDb, ["blastx", "--sensitive"     ], dmnd, fna, bht)
  , ("blastx_db_more_sensitive_rev" , rFlip23 . rSimple . aDiamondFromDb, ["blastx", "--more-sensitive"], dmnd, fna, bht)

  , ("blastp_each"                  , rDiamondFromFaEach, ["blastp"                    ], faa, ListOf faa , ListOf bht)
  , ("blastp_sensitive_each"        , rDiamondFromFaEach, ["blastp", "--sensitive"     ], faa, ListOf faa , ListOf bht)
  , ("blastp_more_sensitive_each"   , rDiamondFromFaEach, ["blastp", "--more-sensitive"], faa, ListOf faa , ListOf bht)
  , ("blastx_each"                  , rDiamondFromFaEach, ["blastx"                    ], fna, ListOf faa , ListOf bht)
  , ("blastx_sensitive_each"        , rDiamondFromFaEach, ["blastx", "--sensitive"     ], fna, ListOf faa , ListOf bht)
  , ("blastx_more_sensitive_each"   , rDiamondFromFaEach, ["blastx", "--more-sensitive"], fna, ListOf faa , ListOf bht)

  -- TODO will this work with the mapping? test!
  , ("blastp_rev_each"                  , rDiamondFromFaRevEach, ["blastp"                    ], faa, ListOf faa , ListOf bht)
  , ("blastp_sensitive_rev_each"        , rDiamondFromFaRevEach, ["blastp", "--sensitive"     ], faa, ListOf faa , ListOf bht)
  , ("blastp_more_sensitive_rev_each"   , rDiamondFromFaRevEach, ["blastp", "--more-sensitive"], faa, ListOf faa , ListOf bht)
  , ("blastx_rev_each"                  , rDiamondFromFaRevEach, ["blastx"                    ], faa, ListOf fna , ListOf bht)
  , ("blastx_sensitive_rev_each"        , rDiamondFromFaRevEach, ["blastx", "--sensitive"     ], faa, ListOf fna , ListOf bht)
  , ("blastx_more_sensitive_rev_each"   , rDiamondFromFaRevEach, ["blastx", "--more-sensitive"], faa, ListOf fna , ListOf bht)

  , ("blastp_db_each"               , rMap 3 . aDiamondFromDb, ["blastp"                    ], faa, ListOf dmnd, ListOf bht)
  , ("blastp_db_sensitive_each"     , rMap 3 . aDiamondFromDb, ["blastp", "--sensitive"     ], faa, ListOf dmnd, ListOf bht)
  , ("blastp_db_more_sensitive_each", rMap 3 . aDiamondFromDb, ["blastp", "--more-sensitive"], faa, ListOf dmnd, ListOf bht)
  , ("blastx_db_each"               , rMap 3 . aDiamondFromDb, ["blastx"                    ], fna, ListOf dmnd, ListOf bht)
  , ("blastx_db_sensitive_each"     , rMap 3 . aDiamondFromDb, ["blastx", "--sensitive"     ], fna, ListOf dmnd, ListOf bht)
  , ("blastx_db_more_sensitive_each", rMap 3 . aDiamondFromDb, ["blastx", "--more-sensitive"], fna, ListOf dmnd, ListOf bht)

  -- TODO these seem to work, but do they make any sense to include?
  -- , ("blastp_db_rev_each"               , rFlip23 . rMap 2 . aDiamondFromDb, ["blastp"                    ], dmnd, ListOf faa, ListOf bht)
  -- , ("blastp_db_sensitive_rev_each"     , rFlip23 . rMap 2 . aDiamondFromDb, ["blastp", "--sensitive"     ], dmnd, ListOf faa, ListOf bht)
  -- , ("blastp_db_more_sensitive_rev_each", rFlip23 . rMap 2 . aDiamondFromDb, ["blastp", "--more-sensitive"], dmnd, ListOf faa, ListOf bht)
  -- , ("blastx_db_rev_each"               , rFlip23 . rMap 2 . aDiamondFromDb, ["blastx"                    ], dmnd, ListOf fna, ListOf bht)
  -- , ("blastx_db_sensitive_rev_each"     , rFlip23 . rMap 2 . aDiamondFromDb, ["blastx", "--sensitive"     ], dmnd, ListOf fna, ListOf bht)
  -- , ("blastx_db_more_sensitive_rev_each", rFlip23 . rMap 2 . aDiamondFromDb, ["blastx", "--more-sensitive"], dmnd, ListOf fna, ListOf bht)
  ]

-- TODO make into a more general utility?
rFlip23 :: RulesFn -> RulesFn
rFlip23 rFn scr (Fun rtn seed deps ids args) = rFn scr (Fun rtn seed deps ids $ fn args)
  where
    fn (one:two:three:rest) = (one:three:two:rest)
    fn as = error $ "bad argument to rFlip23: " ++ show as
rFlip23 _ _ e = error $ "bad argument to rFlip23: " ++ show e

mkDiamondBlast :: DiamondBlastDesc -> Function
mkDiamondBlast (name, rFn, dCmd, qType, sType, rType) = let name' = "diamond_" ++ name in Function
  { fOpChar = Nothing, fName = name'
  , fInputs = [Exactly num, Exactly qType, Exactly sType]
  , fOutput = Exactly rType
  , fTags = [Nondeterministic] -- TODO double check: is it deterministic?
  , fNewRules = NewNotImplemented, fOldRules = rFn dCmd
  }

aDiamondFromDb :: [String] -> [Path] -> Action ()
aDiamondFromDb dCmd [o, e, q, db] = do
  -- wrappedCmdWrite True True cfg ref o'' [] [] [] "diamond.sh" $ [o'', q', eStr, db'] ++ dCmd
  cfg <- fmap fromJust getShakeExtra
  let o'  = fromPath loc cfg o
      e'  = fromPath loc cfg e
      q'  = fromPath loc cfg q
      db' = fromPath loc cfg db
      loc = "modules.diamond.aDiamondblastpdb"
      o'' = traceA loc o' $ dCmd ++ [e', o', q', db']
  eStr <- readLit loc e'
  runCmd $ CmdDesc
    { cmdBinary = "diamond.sh"
    , cmdArguments = [o'', q', eStr, db'] ++ dCmd
    , cmdFixEmpties = True
    , cmdParallel = False -- TODO fix the resource bug
    , cmdOptions = []
    , cmdInPatterns = []
    , cmdOutPath = o''
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = [replaceBaseName o'' "out"]
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [o'', replaceBaseName o'' "out"]
    }
  sanitizeFileInPlace o'
aDiamondFromDb _ _ = error $ "bad argument to aDiamondFromDb"

-- inserts a "makedb" call and reuses the _db compiler from above
-- based on the version in Blast.hs but a little simpler
rDiamondFromFa :: [String] -> RulesFn
rDiamondFromFa dCmd st (Fun rtn seed deps _ [e, q, s])
  = rules st (Fun rtn seed deps name1 [e, q, dbExpr])
  where
    rules  = rSimple $ aDiamondFromDb dCmd
    name1  = "diamond_" ++ headOrDie "failed to parse dCmd in rDiamondFromFa" dCmd
    dbExpr = Fun dmnd seed (depsOf s) "diamond_makedb" [s]
rDiamondFromFa _ _ _ = fail "bad argument to rDiamondFromFa"

-- same, but inserts a "makedb_each" call and maps over it
rDiamondFromFaEach :: [String] -> RulesFn
rDiamondFromFaEach dCmd st (Fun rtn seed deps name [e, q, ss])
  = rules st (Fun rtn seed deps name_db [e, q, dbsExpr])
  where
    rules   = rMap 3 $ aDiamondFromDb dCmd
    name_db = replace "_each" "_db_each" name
    dbsExpr = Fun (ListOf dmnd) seed (depsOf ss) "diamond_makedb_each" [ss]
rDiamondFromFaEach _ _ _ = fail "bad argument to rDiamondFromFa"

-- TODO this is passing the dmnd and faa args backward to diamond.sh, but why?
-- TODO ah, error is in diamond_makedb_each?
-- TODO proper order of transformations should be:
--        diamond_blastp_rev_each e s qs
--        diamond_blastp_rev_db_each e (diamond_makedb s) qs
--        ...
rDiamondFromFaRevEach :: [String] -> RulesFn
rDiamondFromFaRevEach dCmd st (Fun rtn seed deps name [e, s, qs])
  = rules st (Fun rtn seed deps name' [e, dbExpr, qs])
  where
    rules   = rFlip23 . rMap 2 $ aDiamondFromDb dCmd
    -- name1   = "diamond_" ++ headOrDie "failed to parse dCmd in rDiamondFromFa" dCmd ++ "_db_each" -- TODO is this right? get explicitly?
    name'  = replace "_rev_each" "_db_rev_each" name
    -- dbsExpr = Fun (ListOf dmnd) seed (depsOf ss) "diamond_makedb_each" [ss]
    dbExpr = Fun dmnd seed (depsOf s) "diamond_makedb" [s]
rDiamondFromFaRevEach _ _ _ = fail "bad argument to rDiamondFromFa"
