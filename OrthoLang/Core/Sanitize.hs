module OrthoLang.Core.Sanitize
  ( hashIDsFile
  , writeHashedIDs
  , unhashIDs
  , unhashIDsFile
  , readHashedIDs
  , lookupID
  , lookupIDsFile
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

import Prelude hiding (readList)
import qualified Data.DList as D
import qualified Data.Map   as M
-- import qualified Text.Regex as R

import Development.Shake
import OrthoLang.Core.Types

import OrthoLang.Core.Util    (digest, digestLength, headOrDie)
import OrthoLang.Core.Locks   (withWriteLock')
import OrthoLang.Core.Actions (trackWrite', readFileStrict', readList, writeCachedLines)
import OrthoLang.Core.Paths   (fromOrthoLangPath)
import Data.Char             (isSpace)
import Data.Maybe            (catMaybes)
import Data.List             (isPrefixOf, intersperse)
import Data.List.Utils       (split, subIndex)
import System.FilePath (takeFileName)
import Data.IORef                  (readIORef)

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
hashIDsFile :: OrthoLangConfig -> Locks -> OrthoLangPath -> OrthoLangPath -> Action HashedIDs
hashIDsFile cfg ref inPath outPath = do
  let inPath'  = fromOrthoLangPath cfg inPath
      outPath' = fromOrthoLangPath cfg outPath
  -- txt <- withReadLock' ref inPath' $ readFile' $ fromOrthoLangPath cfg inPath
  txt <- readFileStrict' cfg ref inPath'
  let (fasta', ids) = hashIDsTxt txt
      (OrthoLangPath k) = outPath
      v = takeFileName inPath'
      -- ids' = trace ("k: '" ++ k ++ "' v: '" ++ v ++ "'") $ M.insert k v ids
      ids' = M.insert k v ids
  withWriteLock' ref outPath' $ liftIO $ writeFile outPath' fasta' -- TODO be strict?
  trackWrite' cfg [outPath']
  return ids'

writeHashedIDs :: OrthoLangConfig -> Locks -> OrthoLangPath -> HashedIDs -> Action ()
writeHashedIDs cfg ref path ids = do
  withWriteLock' ref path'
    $ liftIO
    $ writeFile path' -- TODO be strict?
    $ unlines
    $ map toLine
    $ M.toList ids
  trackWrite' cfg [path']
  where
    path' = fromOrthoLangPath cfg path
    toLine (h, i) = h ++ "\t" ++ i

-- see https://stackoverflow.com/q/48571481
-- unhashIDs :: HashedIDs -> String -> String
-- unhashIDs ids txt = foldl (\acc (k, v) -> R.subRegex (R.mkRegex k) acc v) txt $ M.toList ids

-- sorry in advance! this is what you get when you build it in ghci
-- its hackier than the regex, but also faster
-- TODO add a config setting for long or short IDs
-- TODO does storing a bunch of seqid_ strings in the map slow it down?
unhashIDs :: Bool -> HashedIDs -> String -> String
unhashIDs longIDs ids t = case findNext t of
                   Nothing -> t
                   Just i  -> let (before, after) = splitAt i t
                                  after' = replaceNextFold after
                              -- the take/drop thing is a hacky way to prevent endless cycles
                              in before ++ take 1 after' ++ unhashIDs longIDs ids (drop 1 after')
                              -- in before ++ unhashIDs longIDs ids after'
  where
    replaceNextFold txt = foldr replacePattern txt patterns
    replacePattern (ptn, fn) txt = if ptn `isPrefixOf` txt then replaceLen txt fn else txt
    replaceLen txt lFn = let (sid, rest) = splitAt (lFn txt) txt
                         in case M.lookup sid ids of
                              Nothing -> txt -- happens to other $TMPDIR paths
                              Just v  -> if longIDs
                                           then v ++ rest
                                           else headOrDie "failed to look up sid in unhashIDs" (words v) ++ rest
    findNext txt = case catMaybes $ map (\p -> subIndex p txt) (map fst patterns) of
                     (x:xs) -> Just $ minimum (x:xs)
                     _ -> Nothing
    patterns =
      [ ("seqid_" , \_   -> length "seqid_" + digestLength)
      , ("$TMPDIR", \txt -> length $ takeWhile (not . isSpace) txt)
      ]

-- This sometimes operates internally, but also writes the final result of the cut to a file.
-- In that case the outpath might not be able to be a cutpath.
unhashIDsFile :: OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangPath -> FilePath -> Action ()
unhashIDsFile cfg ref idref inPath outPath = do
  let inPath'  = fromOrthoLangPath cfg inPath
  -- txt <- withReadLock' ref inPath' $ readFile' $ fromOrthoLangPath cfg inPath
  txt <- readFileStrict' cfg ref inPath'
  -- let txt' = unhashIDs ids txt
  ids <- liftIO $ readIORef idref
  let txt' = unhashIDs False ids txt
  -- liftIO $ putStrLn $ "txt': '" ++ txt' ++ "'"
  withWriteLock' ref outPath $ liftIO $ writeFile outPath txt' -- TODO be strict?
  trackWrite' cfg [outPath]

-- TODO should this be IO or Action?
readHashedIDs :: OrthoLangConfig -> Locks -> OrthoLangPath -> Action HashedIDs
readHashedIDs cfg ref path = do
  let path' = fromOrthoLangPath cfg path
  -- txt <- withReadLock' ref path' $ readFile' path'
  txt <- readFileStrict' cfg ref path'
  let splitFn l = let ws = split "\t" l
                  in if length ws < 2
                       then error ("failed to split '" ++ l ++ "'")
                       else (head ws, concat $ intersperse "\t" $ tail ws)
      ids = map splitFn $ lines txt
  -- liftIO $ putStrLn $ "loaded " ++ show (length ids) ++ " ids"
  -- liftIO $ putStrLn $ "ids'' with    seqid_: " ++ show ((take 10 $ filter (\(k,_) ->     ("seqid_" `isPrefixOf` k)) ids) :: [(String, String)])
  -- liftIO $ putStrLn $ "ids'' without seqid_: " ++ show ((take 10 $ filter (\(k,_) -> not ("seqid_" `isPrefixOf` k)) ids) :: [(String, String)])
  return $ M.fromList ids

-- TODO display error to user in a cleaner way than error!
lookupID :: HashedIDs -> String -> [String]
lookupID ids s = map fst $ filter (\(k,v) -> s == k || s `isPrefixOf` v) (M.toList ids)

-- TODO move to Actions? causes an import cycle so far
lookupIDsFile :: OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangPath -> OrthoLangPath -> Action ()
lookupIDsFile cfg ref ids inPath outPath = do
  partialIDs <- readList cfg ref $ fromOrthoLangPath cfg inPath
  ids' <- liftIO $ readIORef ids
  let lookupFn v = case lookupID ids' v of
                     [] -> liftIO (putStrLn ("warning: no ID found for '" ++ v ++ "'. these are the ids: " ++ show ids')) >> return []
                     is -> return is
  idKeys <- fmap concat $ mapM lookupFn partialIDs
  writeCachedLines cfg ref (fromOrthoLangPath cfg outPath) idKeys
  where
