module ShortCut.Core.Sanitize
  ( hashIDsFile
  , writeHashedIDs
  )
  where

{- This tries to prevent various programs from choking on badly formatted FASTA
 - sequence IDs. It replaces them with hashes before running anything, keeps a
 - hash -> ID map, and puts them back in the final output.
 -}

-- TODO this is almost fast enough to be usable, but tweak performance some more
--      (getting rid of debug helps, as does DList)
-- TODO would having it be a set from the beginning be faster still?

-- import Debug.Trace
import qualified Data.DList as D
import qualified Data.Map   as M

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Util    (digest)
import ShortCut.Core.Locks   (withReadLock', withWriteLock')
import ShortCut.Core.Actions (debugTrackWrite)
import ShortCut.Core.Paths   (fromCutPath)

type HashedSeqIDList = D.DList (String, String)

-- if the line is a fasta sequence id we hash it, otherwise leave alone
-- TODO use the map itself as an accumulator instead
hashIDsLine :: String -> (String, HashedSeqIDList)
hashIDsLine ('>':seqID) = (">id:" ++ idHash, D.singleton (idHash, seqID))
  where
    idHash = digest seqID
hashIDsLine txt = (txt, D.empty)

-- return the FASTA content with hashed IDs, along with a map of hashes -> original IDs
hashIDsFasta :: String -> (String, HashedSeqIDs)
hashIDsFasta txt = joinFn $ foldl accFn (D.empty, D.empty) $ map hashIDsLine $ lines txt
  where
    -- hashIDsLine' s = let s' = hashIDsLine s in trace ("'" ++ s ++ "' -> " ++ show s') s'
    accFn  (oldLines, oldIDs) (newLine, newIDs) = (D.snoc oldLines newLine, D.append oldIDs newIDs)
    joinFn (hashedLines, ids) = (unlines $ D.toList hashedLines, M.fromList $ D.toList ids)

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

writeHashedIDs :: CutConfig -> Locks -> CutPath -> HashedSeqIDs -> Action ()
writeHashedIDs cfg ref path ids
  = withWriteLock' ref path'
  $ liftIO
  $ writeFile path'
  $ unlines
  $ map toLine
  $ M.toList ids
  where
    path' = fromCutPath cfg path
    toLine (h, i) = h ++ "\t" ++ i
