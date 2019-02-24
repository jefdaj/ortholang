module ShortCut.Core.Sanitize
  ( hashIDsFile
  , writeHashedIDs
  -- , unhashIDs
  , unhashIDs
  , unhashIDsFile
  , readHashedIDs
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
-- import qualified Text.Regex as R

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Util    (digest, digestLength)
import ShortCut.Core.Locks   (withReadLock', withWriteLock')
import ShortCut.Core.Actions (debugTrackWrite)
import ShortCut.Core.Paths   (fromCutPath)
import Data.List.Utils       (split)
import Data.Maybe (fromJust)

type HashedSeqIDList = D.DList (String, String)

-- if the line is a fasta sequence id we hash it, otherwise leave alone
-- TODO use the map itself as an accumulator instead
hashIDsLine :: String -> (String, HashedSeqIDList)
hashIDsLine ('>':seqID) = (">seqid_" ++ idHash, D.singleton (idHash, seqID)) -- TODO issue dropping newlines here?
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
writeHashedIDs cfg ref path ids = do
  withWriteLock' ref path'
    $ liftIO
    $ writeFile path'
    $ unlines
    $ map toLine
    $ M.toList ids
  debugTrackWrite cfg [path']
  where
    path' = fromCutPath cfg path
    toLine (h, i) = h ++ "\t" ++ i

-- see https://stackoverflow.com/q/48571481
-- unhashIDs :: HashedSeqIDs -> String -> String
-- unhashIDs ids txt = foldl (\acc (k, v) -> R.subRegex (R.mkRegex k) acc v) txt $ M.toList ids

-- hackier than the regex, but also faster
-- TODO add a config setting for long or short IDs
unhashIDs :: CutConfig -> HashedSeqIDs -> String -> String
unhashIDs _ ids txt = case split "seqid_" txt of
  (before:after) -> before ++ concatMap replaceOne after
    where
      replaceOne idAndRest = let (idAndTab, rest) = splitAt digestLength idAndRest
                                 origID  = fromJust $ M.lookup idAndTab ids
                                 shortID = head $ words origID
                             in shortID ++ rest
  _ -> txt

unhashIDsFile :: CutConfig -> Locks -> HashedSeqIDs -> CutPath -> CutPath -> Action ()
unhashIDsFile cfg ref ids inPath outPath = do
  let inPath'  = fromCutPath cfg inPath
      outPath' = fromCutPath cfg outPath
  txt <- withReadLock' ref inPath' $ readFile' $ fromCutPath cfg inPath
  -- let txt' = unhashIDs ids txt
  let txt' = unhashIDs cfg ids txt
  withWriteLock' ref outPath' $ liftIO $ writeFile outPath' txt'
  debugTrackWrite cfg [outPath']

-- TODO should this be IO or Action?
readHashedIDs :: CutConfig -> Locks -> CutPath -> Action HashedSeqIDs
readHashedIDs cfg ref path = do
  let path' = fromCutPath cfg path
  txt <- withReadLock' ref path' $ readFile' path'
  let ids = map (\(i, si) -> (i, tail si)) $ map (splitAt digestLength) $ lines txt
  -- liftIO $ putStrLn $ "loaded " ++ show (length ids) ++ " ids"
  return $ M.fromList ids
