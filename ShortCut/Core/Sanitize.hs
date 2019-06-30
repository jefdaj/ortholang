module ShortCut.Core.Sanitize
  ( hashIDsFile
  , writeHashedIDs
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
-- TODO would doing it in a separate script be better so it can run on other nodes?

-- import Debug.Trace
import qualified Data.DList as D
import qualified Data.Map   as M
-- import qualified Text.Regex as R

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Util    (digest, digestLength, headOrDie)
import ShortCut.Core.Locks   (withWriteLock')
import ShortCut.Core.Actions (debugTrackWrite, readFileStrict')
import ShortCut.Core.Paths   (fromCutPath)
import Data.Char             (isSpace)
import Data.Maybe            (catMaybes)
import Data.List             (isPrefixOf)
import Data.List.Utils       (subIndex)
import System.FilePath (takeFileName)

type HashedIDList = D.DList (String, String)

-- if the line is a fasta sequence id we hash it, otherwise leave alone
-- TODO use the map itself as an accumulator instead
hashIDsLine :: String -> (String, HashedIDList)
hashIDsLine ('>':seqID) = ('>':idHash, D.singleton (idHash, seqID)) -- TODO issue dropping newlines here?
  where
    idHash = "seqid_" ++ digest seqID -- TODO does storing the extra seqid_ prefix slow it down?
hashIDsLine txt = (txt, D.empty)

-- return the FASTA content with hashed IDs, along with a map of hashes -> original IDs
hashIDsTxt :: String -> (String, HashedIDs)
hashIDsTxt txt = joinFn $ foldl accFn (D.empty, D.empty) $ map hashIDsLine $ lines txt
  where
    -- hashIDsLine' s = let s' = hashIDsLine s in trace ("'" ++ s ++ "' -> " ++ show s') s'
    accFn  (oldLines, oldIDs) (newLine, newIDs) = (D.snoc oldLines newLine, D.append oldIDs newIDs)
    joinFn (hashedLines, ids) = (unlines $ D.toList hashedLines, M.fromList $ D.toList ids)

-- copy a fasta file to another path, replacing sequence ids with their hashes
hashIDsFile :: CutConfig -> Locks -> CutPath -> CutPath -> Action HashedIDs
hashIDsFile cfg ref inPath outPath = do
  let inPath'  = fromCutPath cfg inPath
      outPath' = fromCutPath cfg outPath
  -- txt <- withReadLock' ref inPath' $ readFile' $ fromCutPath cfg inPath
  txt <- readFileStrict' cfg ref inPath'
  let (fasta', ids) = hashIDsTxt txt
      (CutPath k) = outPath
      v = takeFileName inPath'
      -- ids' = trace ("k: '" ++ k ++ "' v: '" ++ v ++ "'") $ M.insert k v ids
      ids' = M.insert k v ids
  withWriteLock' ref outPath' $ liftIO $ writeFile outPath' fasta' -- TODO be strict?
  debugTrackWrite cfg [outPath']
  return ids'

writeHashedIDs :: CutConfig -> Locks -> CutPath -> HashedIDs -> Action ()
writeHashedIDs cfg ref path ids = do
  withWriteLock' ref path'
    $ liftIO
    $ writeFile path' -- TODO be strict?
    $ unlines
    $ map toLine
    $ M.toList ids
  debugTrackWrite cfg [path']
  where
    path' = fromCutPath cfg path
    toLine (h, i) = h ++ "\t" ++ i

-- see https://stackoverflow.com/q/48571481
-- unhashIDs :: HashedIDs -> String -> String
-- unhashIDs ids txt = foldl (\acc (k, v) -> R.subRegex (R.mkRegex k) acc v) txt $ M.toList ids

-- sorry in advance! this is what you get when you build it in ghci
-- its hackier than the regex, but also faster
-- TODO add a config setting for long or short IDs
-- TODO does storing a bunch of seqid_ strings in the map slow it down?
unhashIDs :: HashedIDs -> String -> String
unhashIDs ids t = case findNext t of
                   Nothing -> t
                   Just i  -> let (before, after) = splitAt i t
                                  after' = replaceNextFold after
                              in before ++ unhashIDs ids after'
  where
    replaceNextFold txt = foldr replacePattern txt patterns
    replacePattern (ptn, fn) txt = if ptn `isPrefixOf` txt then replaceLen txt fn else txt
    replaceLen txt lFn = let (sid, rest) = splitAt (lFn txt) txt
                         in case M.lookup sid ids of
                              Nothing -> error $ "sid not found: '" ++ sid ++ "'"
                              Just v  -> headOrDie "failed to look up sid in unhashIDs" (words v) ++ rest
    findNext txt = case catMaybes $ map (\p -> subIndex p txt) (map fst patterns) of
                     (x:xs) -> Just $ minimum (x:xs)
                     _ -> Nothing
    patterns =
      [ ("seqid_" , \_   -> length "seqid_" + digestLength)
      , ("$TMPDIR", \txt -> length $ takeWhile (not . isSpace) txt)
      ]

unhashIDsFile :: CutConfig -> Locks -> HashedIDs -> CutPath -> CutPath -> Action ()
unhashIDsFile cfg ref ids inPath outPath = do
  let inPath'  = fromCutPath cfg inPath
      outPath' = fromCutPath cfg outPath
  -- txt <- withReadLock' ref inPath' $ readFile' $ fromCutPath cfg inPath
  txt <- readFileStrict' cfg ref inPath'
  -- let txt' = unhashIDs ids txt
  let txt' = unhashIDs ids txt
  withWriteLock' ref outPath' $ liftIO $ writeFile outPath' txt' -- TODO be strict?
  debugTrackWrite cfg [outPath']

-- TODO should this be IO or Action?
readHashedIDs :: CutConfig -> Locks -> CutPath -> Action HashedIDs
readHashedIDs cfg ref path = do
  let path' = fromCutPath cfg path
  -- txt <- withReadLock' ref path' $ readFile' path'
  txt <- readFileStrict' cfg ref path'
  let ids = map (\(i, si) -> (i, tail si)) $ map (splitAt digestLength) $ lines txt
  -- liftIO $ putStrLn $ "loaded " ++ show (length ids) ++ " ids"
  return $ M.fromList ids
