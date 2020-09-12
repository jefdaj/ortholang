module OrthoLang.Modules.Diamond
  where

-- TODO any point in adding a daa type?

-- TODO explain in the docs that this basically works like blastp or blastx (proteins) only. much simpler than it seems!

-- TODO generate _rbh variants from regular + _rev ones?
--      seems like a relatively straightforward edit using newExprExpansion now!

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Locks

import Data.List.Utils         (replace)
import OrthoLang.Modules.Blast (bht)
import OrthoLang.Modules.BlastDB (blastdb)
import OrthoLang.Modules.SeqIO (fna, faa)
import System.Exit             (ExitCode(..))
import System.FilePath         (replaceBaseName)
import System.Process          (readProcess)
import Data.Maybe (fromJust)
import Data.Scientific        (Scientific)

-- type RulesFn     = RulesFn
type OldDiamondBlastDesc =
  ( String              -- name
  , [String] -> RulesFn -- rules, which will take cli args
  , [String]            -- cli args
  , Type                -- query type
  , Type                -- subject type
  , Type                -- result type
  )

-- TODO anything else needed?
type NewDiamondBlastDesc =
  ( String -- ^ base name
  , Type   -- ^ query type
  , Type   -- ^ subject type
  , Type   -- ^ return type (TODO remove?)
  )

-- variants :: [NewDiamondDesc]
-- variants =
--   [ ("blastp", faa, dmnd)
--   , ("blastx", fna, dmnd)
--   ]

-- TODO generate base functions
-- TODO generate _rev variants
-- TODO generate _each variants

olModule :: Module
olModule = Module
  { mName = "Diamond"
  , mDesc = "Accelerated BLAST compatible local sequence aligner."
  , mTypes = [fna, faa, dmnd]
  , mGroups = []
  , mEncodings = []
  , mRules = []
  , mFunctions =

      -- database functions
      [ diamondmakedb
      , diamondmakedbEach
      , diamondmakedbAll

      -- single from db
      , diamondBlastpDb
      , diamondBlastxDb

      -- single from fa
      , diamondBlastp
      , diamondBlastx

      -- rev from db
      , diamondBlastpDbRev
      , diamondBlastxDbRev

      -- _each variants
      , diamondBlastpDbEach
      , diamondBlastxDbEach
      , diamondBlastpDbRevEach
      , diamondBlastxDbRevEach
      ]

--       ++ map mkDiamondEach
--          -- _db_each
--           [ ("blastp_db", ["blastp"], faa, dmnd, bht)
--           , ("blastx_db", ["blastx"], fna, dmnd, bht)
--           -- _each (fa variants)
--           , ("blastp", ["blastp"], faa, faa , bht)
--           , ("blastx", ["blastx"], fna, faa , bht)
--           -- TODO db rev?
--           -- _rev_each (fa variants)
--           , ("blastp_rev", ["blastp"], faa, faa , bht)
--           , ("blastx_rev", ["blastx"], faa, fna , bht)
--           ]

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


---------------------
-- diamond_makedb* --
---------------------

diamondmakedb :: Function
diamondmakedb = newFnS1
  "diamond_makedb"
  (Exactly faa)
  (Exactly dmnd)
  "diamond_makedb.sh"
  [Nondeterministic]
  id

diamondmakedbEach :: Function
diamondmakedbEach = newFnA1
  "diamond_makedb_each"
  (Exactly $ ListOf faa)
  (Exactly $ ListOf dmnd)
  (newMap1of1 "diamond_makedb")
  [Nondeterministic]
 
diamondmakedbAll :: Function
diamondmakedbAll = newFnA1
  "diamond_makedb_all"
  (Exactly $ ListOf faa)
  (Exactly dmnd)
  aDiamondmakedbAll
  [Nondeterministic]

aDiamondmakedbAll :: NewAction1
aDiamondmakedbAll (ExprPath out') fasPath' = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.blastdb.aDiamondmakedbAll2"
      out = toPath loc cfg out'
  faPaths <- readPaths loc fasPath'
  aSimpleScriptPar "diamond_makedb_all.sh" (out:faPaths)


-----------------
-- sensitivity --
-----------------

-- | From the Diamond docs:
--
--   > DIAMOND has a number of sensitivity settings to accomodate different
--   > applications. The default mode is the fastest and tailored towards
--   > finding homologies of >70% sequence identity, the --sensitive mode is
--   > tailored to hits of >40% identity, while the --very-sensitive and
--   > --ultra-sensitive modes provide sensitivity accross the whole range of
--   > pairwise alignments.
sensitivityFlags :: [(Int, String)]
sensitivityFlags =
  [ (0, ""                 ) -- "fast" is the default
  , (1, "--mid-sensitive"  )
  , (2, "--sensitive"      )
  , (3, "--more-sensitive" )
  , (4, "--very-sensitive" )
  , (5, "--ultra-sensitive")
  ]

readSensitivity :: FilePath -> Action String
readSensitivity numPath = do
  -- cfg <- fmap fromJust getShakeExtra
  let loc = "ortholang.modules.diamond.readSensitivity"
  n <- fmap (read :: String -> Scientific) $ readLit loc numPath
  let n' = if n <= 0 then 0
           else if n >= 5 then 5
           else floor n
      flag = fromJust $ lookup n' sensitivityFlags
  return flag

------------------------
-- old code to remove --
------------------------

-- TODO can some of these be replaced by a numeric sensitivity arg?
-- oldVariants :: [OldDiamondBlastDesc]
-- oldVariants =
-- 
--   -- TODO rewrite with a new api function "newFlip23"
--   [ ("blastp_rev"   , rFlip23 . rDiamondFromFa          , ["blastp"], faa, faa , bht)
--   , ("blastx_rev"   , rFlip23 . rDiamondFromFa          , ["blastx"], faa, fna , bht)
--   , ("blastp_db_rev", rFlip23 . rSimple . aDiamondFromDb, ["blastp"], dmnd, faa, bht)
--   , ("blastx_db_rev", rFlip23 . rSimple . aDiamondFromDb, ["blastx"], dmnd, fna, bht)
-- 
--   -- TODO these seem to work, but do they make any sense to include?
--   -- , ("blastp_db_rev_each"               , rFlip23 . rMap 2 . aDiamondFromDb, ["blastp"                    ], dmnd, ListOf faa, ListOf bht)
--   -- , ("blastx_db_rev_each"               , rFlip23 . rMap 2 . aDiamondFromDb, ["blastx"                    ], dmnd, ListOf fna, ListOf bht)
--   ]

-- type NewDiamondBlastDesc =
  -- ( String   -- name
  -- , [String] -- cli args
  -- , Type     -- query type
  -- , Type     -- subject type
  -- , Type     -- result type
  -- )

--------------------
-- single from db --
--------------------

diamondBlastpDb = mkDiamondFromDb ("blastp", faa, dmnd, bht)
diamondBlastxDb = mkDiamondFromDb ("blastx", fna, dmnd, bht)

mkDiamondFromDb :: NewDiamondBlastDesc -> Function
mkDiamondFromDb (name, qType, sType, rType) = newFnA4
  ("diamond_" ++ name ++ "_db")
  (Exactly num, Exactly num, Exactly qType, Exactly sType)
  (Exactly rType)
  (aDiamondFromDb name)
  [Nondeterministic]

aDiamondFromDb :: String -> NewAction4
aDiamondFromDb bCmd (ExprPath o') s' e' q' db' = do
  let loc = "ortholang.modules.diamond.aDiamondFromDb"
      o'' = traceA loc o' [bCmd, e', o', q', db']
  sFlag <- readSensitivity s' -- sensitivity value
  eStr  <- readLit loc e' -- e-value cutoff (TODO is it called that in diamond?)
  runCmd $ CmdDesc
    { cmdBinary = "diamond.sh"
    , cmdArguments = [o'', q', eStr, db', bCmd, sFlag]
    , cmdFixEmpties = True
    , cmdParallel = False -- TODO fix the resource bug
    , cmdOptions = []
    , cmdInPatterns = []
    , cmdNoNeedDirs = []
    , cmdOutPath = o''
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = [replaceBaseName o'' "out"]
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [o'', replaceBaseName o'' "out"]
    }
  sanitizeFileInPlace o'

------------------------
-- single fa variants --
------------------------

diamondBlastp = mkDiamondFa ("blastp", faa, faa, bht)
diamondBlastx = mkDiamondFa ("blastx", fna, faa, bht)

mkDiamondFa :: NewDiamondBlastDesc -> Function
mkDiamondFa (name, sType, qType, rType) = newExprExpansion
  ("diamond_" ++ name)
  [Exactly num, Exactly num, Exactly qType, Exactly sType]
  (Exactly rType)
  mDiamondMakedb
  [Nondeterministic]

mDiamondMakedb :: ExprExpansion
mDiamondMakedb _ _ (Fun r ms ds n [i1, i2, i3, i4]) = Fun r ms ds (n ++ "_db") [i1, i2, i3, fn]
  where
    fn = Fun dmnd ms (depsOf i4) "diamond_makedb" [i4]
mDiamondMakedb _ _ e = error "modules.diamond.mDiamondMakedb" $ "bad argument: " ++ show e

-- inserts a "makedb" call and reuses the _db compiler from above
-- based on the version in Blast.hs but a little simpler
-- rDiamondFromFa :: [String] -> RulesFn
-- rDiamondFromFa dCmd st (Fun rtn seed deps _ [e, q, s])
--   = rules st (Fun rtn seed deps name1 [e, q, dbExpr])
--   where
--     rules  = rSimple $ aDiamondFromDb dCmd
--     name1  = "diamond_" ++ headOrDie "failed to parse dCmd in rDiamondFromFa" dCmd
--     dbExpr = Fun dmnd seed (depsOf s) "diamond_makedb" [s]
-- rDiamondFromFa _ _ _ = fail "bad argument to rDiamondFromFa"


-------------------
-- _rev variants --
-------------------

diamondBlastpDbRev = mkDiamondRev ("blastp", faa, dmnd, bht)
diamondBlastxDbRev = mkDiamondRev ("blastx", fna, dmnd, bht)

mkDiamondRev :: NewDiamondBlastDesc -> Function
mkDiamondRev (name, qType, sType, rType) = newExprExpansion
  ("diamond_" ++ name ++ "_db_rev")
  [Exactly num, Exactly num, Exactly sType, Exactly qType]
  (Exactly rType)
  (mFlip34 "_db_rev" "_db")
  [Nondeterministic]

-- TODO move to NewRules.hs?
mFlip34 :: String -> String -> ExprExpansion
mFlip34 old new _ _ (Fun r ms ds n [i1, i2, i3, i4]) = Fun r ms ds (replace old new n) [i1, i2, i4, i3]
mFlip34 _ _ _ _ e = error "modules.diamond.mFlip34" $ "bad argument: " ++ show e

--------------------
-- _each variants --
--------------------

diamondBlastpDbEach    = mkDiamondEach "_db_each"     "_db"     ("blastp", faa, dmnd, bht)
diamondBlastxDbEach    = mkDiamondEach "_db_each"     "_db"     ("blastx", fna, dmnd, bht)
diamondBlastpDbRevEach = mkDiamondEach "_db_rev_each" "_db_rev" ("blastp", faa, dmnd, bht)
diamondBlastxDbRevEach = mkDiamondEach "_db_rev_each" "_db_rev" ("blastx", fna, dmnd, bht)

mkDiamondEach :: String -> String -> NewDiamondBlastDesc -> Function
mkDiamondEach old new (base, qType, sType, rType) =
  let name  = "diamond_" ++ base ++ old
      name' = replace old new name
  in newFnA4
       name
       (Exactly num, Exactly num, Exactly qType, Exactly $ ListOf sType)
       (Exactly $ ListOf rType)
       (newMap4of4 name')
       [Nondeterministic]

  -- _each variants
  -- map (\)

 -- TODO straighten out the _db naming stuff!
-- mkDiamondBlastFromFa :: String -> Type -> Type -> Type -> Function
-- mkDiamondBlastFromFa name qType faType dbType = newExprExpansion
--   (replace "_db" "" name)
--   [Exactly num, Exactly qType, Exactly faType]
--   (Exactly bht)
--   (mDiamondBlastFromFa name dbType)
--   [Nondeterministic]

-- TODO can this from BlastRBH be made into the flip fn?
-- mBlastFromFaRev :: ExprExpansion
-- mBlastFromFaRev _ _ (Fun r ms ds n [e, q, s]) = Fun r ms ds (replace "_rev" "" n) [e, s, q]
-- mBlastFromFaRev _ _ e = error "ortholang.modules.blastrbh.mBlastFromFaRev" $ "bad argument: " ++ show e

-- TODO straighten out the _db naming stuff!
-- mDiamondBlastFromFa :: String -> Type -> ExprExpansion
-- mDiamondBlastFromFa name dbType _ _ (Fun r ms ds _ [e,q,s]) = Fun r ms ds name1 [e,q,expr]
--   where
--     np = if dbType == fna then "nucl" else "prot"
--     name1 = replace "_db" "" name
--     name2 = "makeblastdb_" ++ np -- TODO would the _all version withSingleton arg be better?
--     expr = Fun (EncodedAs blastdb dbType) Nothing (depsOf s) name2 [s]
-- mDiamondBlastFromFa _ _ _ _ e = error "ortholang.modules.blast.mkDiamondBlastFromFa" $ "bad arg: " ++ show e

-- | This is the main entrypoint for generating an (old-style) OrthoLang function from the description
-- mkDiamondBlast :: OldDiamondBlastDesc -> Function
-- mkDiamondBlast (name, rFn, dCmd, qType, sType, rType) = let name' = "diamond_" ++ name in Function
--   { fOpChar = Nothing, fName = name'
--   , fInputs = [Exactly num, Exactly qType, Exactly sType]
--   , fOutput = Exactly rType
--   , fTags = [Nondeterministic] -- TODO double check: is it deterministic?
--   , fNewRules = NewNotImplemented, fOldRules = rFn dCmd
--   }

-- TODO rewrite as NewAction3 -> NewAction3 for here only for now
-- TODO make into a more general utility?
-- rFlip23 :: RulesFn -> RulesFn
-- rFlip23 rFn scr (Fun rtn seed deps ids args) = rFn scr (Fun rtn seed deps ids $ fn args)
--   where
--     fn (one:two:three:rest) = (one:three:two:rest)
--     fn as = error $ "bad argument to rFlip23: " ++ show as
-- rFlip23 _ _ e = error $ "bad argument to rFlip23: " ++ show e

-- aDiamondFromDb _ _ = error $ "bad argument to aDiamondFromDb"

-- inserts a "makedb" call and reuses the _db compiler from above
-- based on the version in Blast.hs but a little simpler
-- rDiamondFromFa :: [String] -> RulesFn
-- rDiamondFromFa dCmd st (Fun rtn seed deps _ [e, q, s])
--   = rules st (Fun rtn seed deps name1 [e, q, dbExpr])
--   where
--     rules  = rSimple $ aDiamondFromDb dCmd
--     name1  = "diamond_" ++ headOrDie "failed to parse dCmd in rDiamondFromFa" dCmd
--     dbExpr = Fun dmnd seed (depsOf s) "diamond_makedb" [s]
-- rDiamondFromFa _ _ _ = fail "bad argument to rDiamondFromFa"

-- TODO could this be rewritten to also work on other blast-like fns, for example in psiblast?
-- mDiamondFromFa :: ExprExpansion
-- mDiamondFromFa

-- TODO separate into two things: an exprExpansion and a newMap
-- same, but inserts a "makedb_each" call and maps over it
-- rDiamondFromFaEach :: [String] -> RulesFn
-- rDiamondFromFaEach dCmd st (Fun rtn seed deps name [e, q, ss])
--   = rules st (Fun rtn seed deps name_db [e, q, dbsExpr])
--   where
--     rules   = rMap 3 $ aDiamondFromDb dCmd
--     name_db = replace "_each" "_db_each" name
--     dbsExpr = Fun (ListOf dmnd) seed (depsOf ss) "diamond_makedb_each" [ss]
-- rDiamondFromFaEach _ _ _ = fail "bad argument to rDiamondFromFa"

-- TODO this is passing the dmnd and faa args backward to diamond.sh, but why?
-- TODO ah, error is in diamond_makedb_each?
-- TODO proper order of transformations should be:
--        diamond_blastp_rev_each e s qs
--        diamond_blastp_rev_db_each e (diamond_makedb s) qs
--        ...
-- rDiamondFromFaRevEach :: [String] -> RulesFn
-- rDiamondFromFaRevEach dCmd st (Fun rtn seed deps name [e, s, qs])
--   = rules st (Fun rtn seed deps name' [e, dbExpr, qs])
--   where
--     rules   = rFlip23 . rMap 2 $ aDiamondFromDb dCmd
--     -- name1   = "diamond_" ++ headOrDie "failed to parse dCmd in rDiamondFromFa" dCmd ++ "_db_each" -- TODO is this right? get explicitly?
--     name'  = replace "_rev_each" "_db_rev_each" name
--     -- dbsExpr = Fun (ListOf dmnd) seed (depsOf ss) "diamond_makedb_each" [ss]
--     dbExpr = Fun dmnd seed (depsOf s) "diamond_makedb" [s]
-- rDiamondFromFaRevEach _ _ _ = fail "bad argument to rDiamondFromFa"
