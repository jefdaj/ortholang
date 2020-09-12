module OrthoLang.Modules.Diamond
  where

-- TODO any point in adding a daa type?

-- TODO explain in the docs that this basically works like blastp or blastx (proteins) only. much simpler than it seems!

-- TODO generate _rbh variants from regular + _rev ones?
--      seems like a relatively straightforward edit using newExprExpansion now!
--      could probably be done via something exported from BlastRBH

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

type NewDiamondBlastDesc =
  ( String -- base name
  , Type   -- query type
  , Type   -- subject type
  , Type   -- return type (TODO remove?)
  )

------------
-- module --
------------

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
      -- TODO do these make sense?
      -- , diamondBlastpDbRev
      -- , diamondBlastxDbRev

      -- rev from fa
      , diamondBlastpRev
      , diamondBlastxRev

      -- _each variants of the other 3
      , diamondBlastpEach
      , diamondBlastxEach
      , diamondBlastpDbEach
      , diamondBlastxDbEach
      , diamondBlastpDbRevEach
      , diamondBlastxDbRevEach

      -- TODO _all variants
      -- TODO _rbh variants

      ]
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

---------------------------
-- diamond_blast{p,x}_db --
---------------------------

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
-- diamond_blast{p,x} --
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

-------------------------------
-- diamond_blast{p,x}_db_rev --
-------------------------------

-- TODO do these make sense to include? maybe for the _rev_each variants?

-- diamondBlastpDbRev = mkDiamondRev "_db_rev" "_db" ("blastp", faa, dmnd, bht)
-- diamondBlastxDbRev = mkDiamondRev "_db_rev" "_db" ("blastx", fna, dmnd, bht)

mkDiamondRev :: String -> String -> NewDiamondBlastDesc -> Function
mkDiamondRev old new (name, qType, sType, rType) = newExprExpansion
  ("diamond_" ++ name ++ old)
  [Exactly num, Exactly num, Exactly qType, Exactly sType]
  (Exactly rType)
  (mFlip34 old new)
  [Nondeterministic]

-- TODO move to NewRules.hs? if so, probably work on variable number of args?
mFlip34 :: String -> String -> ExprExpansion
mFlip34 old new _ _ (Fun r ms ds n [i1, i2, i3, i4]) = Fun r ms ds (replace old new n) [i1, i2, i4, i3]
mFlip34 _ _ _ _ e = error "modules.diamond.mFlip34" $ "bad argument: " ++ show e

----------------------------
-- diamond_blast{p,x}_rev --
----------------------------

diamondBlastpRev = mkDiamondRev "_rev" "" ("blastp", faa, dmnd, bht)
diamondBlastxRev = mkDiamondRev "_rev" "" ("blastp", faa, dmnd, bht)

-------------------------
-- diamond_blast*_each --
-------------------------

diamondBlastpEach      = mkDiamondEach "_each"        ""        ("blastp", faa, faa , bht)
diamondBlastxEach      = mkDiamondEach "_each"        ""        ("blastx", fna, faa , bht)
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

-------------------
-- _all variants --
-------------------

-- TODO write these

-------------------
-- _rbh variants --
-------------------

-- TODO write these

----------------------------
-- old code for reference --
----------------------------

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
