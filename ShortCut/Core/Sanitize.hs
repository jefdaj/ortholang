module ShortCut.Core.Sanitize
  ( hashIDsFile
  )
  where

import Debug.Trace

{- This tries to prevent various programs from choking on badly formatted FASTA
 - sequence IDs.  It replaces them with hashes before running anything, keeps a
 - hash -> ID map, and puts them back in the output.
 -}

import Development.Shake
import ShortCut.Core.Types

import Data.Map              (fromList)
import ShortCut.Core.Util    (digest)
import ShortCut.Core.Locks   (withReadLock', withWriteLock')
import ShortCut.Core.Actions (debugTrackWrite)
import ShortCut.Core.Paths   (fromCutPath)

type HashedSeqIDList = [(String, String)]

-- if the line is a fasta sequence id we hash it, otherwise leave alone
-- TODO use the map itself as an accumulator instead
hashIDsLine :: String -> (String, HashedSeqIDList)
hashIDsLine ('>':seqID) = (">id:" ++ idHash, [(idHash, seqID)])
  where
    idHash = digest seqID
hashIDsLine text = (text, [])

-- return the FASTA content with hashed IDs, along with a map of hashes -> original IDs
hashIDsFasta :: String -> (String, HashedSeqIDs)
hashIDsFasta txt = joinFn $ foldl accFn ([], []) $ map hashIDsLine' $ lines txt
  where
    hashIDsLine' s = let s' = hashIDsLine s in trace ("'" ++ s ++ "' -> " ++ show s') s'
    accFn  (oldLines, oldIDs) (newLine, newIDs) = (oldLines ++ '\n':newLine, oldIDs ++ newIDs)
    joinFn (hashedLines, ids) = (hashedLines, fromList ids)

-- copy a fasta file to another path, replacing sequence ids with their hashes
hashIDsFile :: CutConfig -> Locks -> CutPath -> CutPath -> Action HashedSeqIDs
hashIDsFile cfg ref inPath outPath = do
  let inPath'  = fromCutPath cfg inPath
      outPath' = fromCutPath cfg outPath
  txt <- withReadLock' ref inPath' $ readFile' $ fromCutPath cfg inPath
  let (fasta', ids) = hashIDsFasta txt
  withWriteLock' ref outPath' $ liftIO $ writeFile outPath' fasta'
  debugTrackWrite cfg [outPath']
  return ids
