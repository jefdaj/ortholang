module ShortCut.Modules.BlastDB where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Compile  (cExpr)
import ShortCut.Core.Config   (wrappedCmd)
import ShortCut.Core.Debug    (debugTrackWrite)
import ShortCut.Core.Paths    (exprPath)
import ShortCut.Modules.SeqIO (faa, fna)
import System.FilePath        (takeBaseName)

cutModule :: CutModule
cutModule = CutModule
  { mName = "blastdb"
  , mFunctions =
    [ mkBlastDB
    -- , TODO write loadBlastDB
    ]
  }

bdb :: CutType
bdb = CutType
  { tExt  = "bdb"
  , tDesc = "blast database"
  , tShow  = defaultShow -- TODO will this work? maybe use a dummy one
  }

-- TODO silence output?
-- TODO does this have an error where db path depends on the outer expression
--      in addition to actual inputs?
mkBlastDB :: CutFunction
mkBlastDB = CutFunction
  { fName      = "makeblastdb"
  , fTypeCheck = tMkBlastDB
  , fFixity    = Prefix
  , fCompiler  = cMkBlastDB
  }

tMkBlastDB :: TypeChecker
tMkBlastDB [x] | x `elem` [faa, fna] = Right bdb
tMkBlastDB _ = error "makeblastdb requires a fasta file"

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
cMkBlastDB :: RulesFn
cMkBlastDB s@(_,cfg) (CutFun _ _ _ _ [fa]) = do
  (ExprPath faPath) <- cExpr s fa
  let (ExprPath dbPrefix) = exprPath cfg fa []
      dbType = if      typeOf fa == fna then "nucl"
               else if typeOf fa == faa then "prot"
               else    error $ "invalid FASTA type: " ++ show (typeOf fa)
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
cMkBlastDB _ _ = error "bad argument to mkBlastDB"
