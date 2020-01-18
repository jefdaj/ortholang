{-# LANGUAGE ScopedTypeVariables #-}

{- Some Shake Actions wrapped with OrthoLang-specific additions,
 - and some other stuff. Eventually, it would be nice if all IO happened here.
 -
 - TODO rename to be shorter (no "safe", "wrapped", etc)
 -}

module OrthoLang.Core.Actions

  -- read files
  ( readLit
  , readLits
  , readPath
  , readPaths
  , absolutizePaths
  , readLitPaths
  , readString
  , readStrings
  , readList
  , readFileStrict
  , readFileStrict'

  -- write files
  , writeLit
  , writeLits
  , writePath
  , writePaths
  , writeString
  , writeStrings
  , writeCachedLines
  , writeCachedVersion
  , sanitizeFileInPlace
  , sanitizeFilesInPlace

  -- debugging
  -- TODO export different ones for the modules from Modules.hs?
  , need'
  , trackWrite' -- TODO should this not be needed?
  , traceA
  , debugA

  -- run system commands
  -- TODO remove all these except one, and have it take a record
  , CmdDesc(..)
  , runCmd

  -- misc
  -- , digestFile  -- TODO what's the difference with hashContent?
  , hashContent
  , withBinHash
  , symlink
  , cachedLinesPath

  )
  where

import Prelude hiding (readList)
import Development.Shake
import OrthoLang.Core.Types
-- import OrthoLang.Core.Config (debug)

import Control.Monad              (when)
import Data.List                  (sort, nub, isPrefixOf, isInfixOf, isSuffixOf)
import Data.List.Split            (splitOneOf)
import Development.Shake.FilePath ((</>), isAbsolute, pathSeparators, makeRelative)
-- import OrthoLang.Core.Debug        (debug)
import OrthoLang.Core.Paths        (OrthoLangPath, toOrthoLangPath, fromOrthoLangPath, checkLit,
                                   checkLits, cacheDir, cutPathString,
                                   stringOrthoLangPath, toGeneric, sharedPath)
import OrthoLang.Core.Util         (digest, digestLength, rmAll, readFileStrict, absolutize, resolveSymlinks,
                                   ignoreExistsError, digest, globFiles, isEmpty, headOrDie, debug, trace, traceShow)
import OrthoLang.Core.Locks        (withReadLock', withReadLocks',
                                   withWriteLock', withWriteLocks', withWriteOnce)
import System.Directory           (createDirectoryIfMissing, pathIsSymbolicLink, copyFile, renameFile)
import System.Exit                (ExitCode(..))
import System.FilePath            ((<.>), takeDirectory, takeExtension)
import System.FilePath.Glob       (compile, globDir1)
-- import System.IO                  (IOMode(..), withFile)
import System.Posix.Files         (readSymbolicLink, createSymbolicLink, setFileMode)
import System.Posix.Escape         (escape)
-- import System.IO.Temp (emptyTempFile)
-- import Control.Concurrent.Thread.Delay (delay)
import Data.Maybe (isJust)
-- import Network.HTTP (simpleHTTP, getRequest, Response(..))
import System.IO.Temp (writeTempFile)
import qualified Data.ByteString as BS
import Network.Download (openURIString)
-- import Control.Exception (try)
import Control.Exception.Safe (catchAny)

---------------
-- debugging --
---------------

-- TODO should this use one of Shake's put* functions instead and be in Action?
-- TODO oh, and shake has traced now too! consider using that, or rewriting with Logging
debugA :: String -> String -> Action ()
debugA name msg = liftIO $ debug name msg

-- debugA is for export; debugA' is specialized to this module
debugA' :: String -> String -> Action ()
debugA' name = debugA ("core.actions." ++ name)

-- TODO use this for all (or most) of the current debugS calls if possible
-- TODO use Shake's traced function for this? rewrite it to work with Logging?
traceA :: (Show a, Show b) => String -> a -> [b] -> a
traceA name out args = trace "core.actions" msg out
  where
    msg = name ++ " creating " ++ show out ++ " from " ++ show args

-- TODO is this .need thing the best convention?
need' :: OrthoLangConfig -> Locks -> String -> [FilePath] -> Action ()
need' cfg ref fnName paths = mapM_ (needShared cfg ref fnName . toOrthoLangPath cfg) paths

needDebug :: String -> [FilePath] -> Action ()
needDebug fnName paths = do
  debugA (fnName ++ ".need") (show paths)
  need paths

-- TODO how to force it to load the seqids when their downstream files aren't needed?
--      probably list all the load* dependencies of the expr and want/need them first
needShared :: OrthoLangConfig -> Locks -> String -> OrthoLangPath -> Action ()
needShared cfg ref name path@(OrthoLangPath p) = do
  let path' = fromOrthoLangPath cfg path
  done <- doesFileExist path'
  if done -- if done already, needing is cheap + more elegant than cache lookup
     || ("/load" `isInfixOf` p)
     || ("/glob" `isInfixOf` p)
     || ("exprs/str" `isInfixOf` p)
     || ("exprs/num" `isInfixOf` p)
     || ("/reps/" `isInfixOf` p)
     || ("/vars/" `isInfixOf` p)
     || (not $ "$TMPDIR" `isPrefixOf` p)
    then needDebug name [path']
    else do
      shared <- lookupShared cfg path
      case shared of
        Nothing -> needDebug name [path']
        Just sp -> do
          isLink <- liftIO $ pathIsSymbolicLink sp
          when isLink $ needLink cfg ref name sp
          when (isPathList sp) $ do
            paths <- readPaths cfg ref sp
            liftIO $ putStrLn $ "paths: " ++ show paths
            need' cfg ref name $ map (fromOrthoLangPath cfg) paths -- TODO rename?
          liftIO $ createDirectoryIfMissing True $ takeDirectory path'
          -- TODO figure out better criteria for this
          if "download" `isInfixOf` sp
            then liftIO $ renameFile sp path'
            else withWriteOnce ref path' $ liftIO $ copyFile sp path'
          trackWrite' cfg [path']

isPathList :: FilePath -> Bool
isPathList path
  =  (".list" `isSuffixOf` path)
  && not (".str.list" `isSuffixOf` path)
  && not (".num.list" `isSuffixOf` path)

needLink :: OrthoLangConfig -> Locks -> String -> FilePath -> Action ()
needLink cfg ref name link = do
  relPath <- liftIO $ readSymbolicLink link
  absPath <- liftIO $ absolutize $ takeDirectory link </> relPath
  need' cfg ref name [absPath]

lookupShared :: OrthoLangConfig -> OrthoLangPath -> Action (Maybe FilePath)
lookupShared cfg path = case sharedPath cfg path of
  Nothing -> return Nothing
  Just sp -> do
    if "http" `isPrefixOf` sp -- TODO come up with something more robust to detect urls!
      then liftIO $ download cfg sp
      else do
        exists <- doesFileExist sp
        return $ if exists
          then Just sp
          else Nothing

-- TODO what about timeouts and stuff?
-- TODO would downloading as binary be better?
-- TODO how to remove the files after downloading?
download :: OrthoLangConfig -> String -> IO (Maybe FilePath)
-- download _ url = return Nothing
download cfg url = do
  -- liftIO $ putStrLn $ "url: " ++ url

  -- works, but seems to have problems with timeouts or something
  -- response <- simpleHTTP $ getRequest url
  -- case fmap rspCode response of
  --   Left _ -> do
  --     -- liftIO $ putStrLn $ show err
  --     return Nothing
  --   Right (2,0,0) -> case fmap rspBody response of
  --     Left _ -> return Nothing
  --     Right txt -> do
  --       path <- writeTempFile (cfgTmpDir cfg) "download.txt" txt
  --       -- liftIO $ putStrLn $ "downloaded: " ++ path
  --       return $ Just path
  --   Right _ -> return Nothing

  -- use download instead of HTTP
  es <- openURIString url
  case es of
    Left _ -> return Nothing
    Right s -> fmap Just $ writeTempFile (cfgTmpDir cfg) "download.txt" s

----------------
-- read files --
----------------

-- Action version of readFileStrict. This is used for all reads during a cut;
-- the raw one is just for showing results, reading cmd files etc.
readFileStrict' :: OrthoLangConfig -> Locks -> FilePath -> Action String
readFileStrict' cfg ref path = do
  need' cfg ref "core.actions.readFileStrict'" [path]
  withReadLock' ref path $ liftIO (readFileStrict ref path)
-- {-# INLINE readFileStrict' #-}

{- OrthoLang requires empty strings to contain the text <<emptystr>> so we
 - can distinguish them from the empty files that might result from a cmd
 - failing, and from empty lists in case of an error in the typechecker.  This
 - also gives empty lists and strings distinct hashes.
 -}
readLit :: OrthoLangConfig -> Locks -> FilePath -> Action String
readLit cfg locks path = do
  debugA' "readLit" path
  -- TODO need' here?
  need [path] -- Note isEmpty also does this
  empty <- isEmpty locks path
  if empty
    then return ""
    else fmap (checkLit . init) -- TODO safe? already checked if empty
       $ readFileStrict' cfg locks path

readLits :: OrthoLangConfig -> Locks -> FilePath -> Action [String]
readLits cfg ref path = readList cfg ref path >>= return . checkLits

readPath :: OrthoLangConfig -> Locks -> FilePath -> Action OrthoLangPath
readPath cfg ref path = readPaths cfg ref path >>= return . headOrDie "readPath failed"

-- TODO should this have checkPaths?
readPaths :: OrthoLangConfig -> Locks -> FilePath -> Action [OrthoLangPath]
readPaths cfg ref path = (fmap . map) stringOrthoLangPath (readList cfg ref path)

-- makes a copy of a list of paths without ortholang funny business,
-- suitible for external scripts to read
-- TODO does this go here or somewhere else?
absolutizePaths :: OrthoLangConfig -> Locks -> FilePath -> FilePath -> Action ()
absolutizePaths cfg ref inPath outPath = do
  paths  <- readPaths cfg ref inPath
  paths' <- mapM (liftIO . absolutize. fromOrthoLangPath cfg) paths
  need' cfg ref "core.actions.absolutizePaths" paths' -- because they will be read by the script next
  -- liftIO $ putStrLn $ "paths': " ++ show paths'
  withWriteLock' ref outPath $ writeFile' outPath $ unlines paths'

-- read a file as lines, convert to absolute paths, then parse those as cutpaths
-- used by the load_* functions to convert user-friendly relative paths to absolute
readLitPaths :: OrthoLangConfig -> Locks -> FilePath -> Action [OrthoLangPath]
readLitPaths cfg ref path = do
  ls <- readList cfg ref path
  return $ map (toOrthoLangPath cfg . toAbs) ls
  where
    toAbs line = if isAbsolute line
                   then line
                   else cfgWorkDir cfg </> line

-- TODO how should this relate to readLit and readStr?
readString :: OrthoLangType -> OrthoLangConfig -> Locks -> FilePath -> Action String
readString etype cfg ref path = readStrings etype cfg ref path >>= return . headOrDie "readString failed"

{- Read a "list of whatever". Mostly for generic set operations. You include
 - the OrthoLangType (of each element, not the list!) so it knows how to convert from
 - String, and then within the function you treat them as Strings.
 -}
readStrings :: OrthoLangType -> OrthoLangConfig -> Locks -> FilePath -> Action [String]
readStrings etype cfg ref path = if etype' `elem` [str, num]
  then readLits cfg ref path
  else (fmap . map) (fromOrthoLangPath cfg) (readPaths cfg ref path)
  where
    etype' = trace "core.actions.readStrings" ("readStrings (each " ++ extOf etype ++ ") from " ++ path) etype

{- OrthoLang requires empty lists to contain the text <<emptylist>> so we can
 - distinguish them from the empty files that might result from a cmd
 - failing, and from empty strings in case of an error in the typechecker.
 - This also gives empty lists and strings distinct hashes.
 -
 - Note that strict reading is important to avoid "too many open files" on long lists.
 -}
readList :: OrthoLangConfig -> Locks -> FilePath -> Action [String]
readList cfg locks path = do
  debugA' "readList" $ show path
  need' cfg locks "core.actions.readList" [path] -- Note isEmpty also does this
  empty <- isEmpty locks path
  if empty
    then return []
    else fmap lines
       $ withReadLock' locks path
       $ liftIO
       $ readFileStrict locks path


-----------------
-- write files --
-----------------

cachedLinesPath :: OrthoLangConfig -> [String] -> FilePath
cachedLinesPath cfg content = cDir </> digest content <.> "txt"
  where
    cDir = fromOrthoLangPath cfg $ cacheDir cfg "lines"

-- TODO move to Util?
first50 :: Show a => a -> String
first50 thing = if length shown > 50 then take 50 shown ++ "..." else shown
  where
    shown = show thing

-- TODO move to Util?
last50 :: Show a => a -> String
last50 thing = if length shown > 50 then "..." ++ (reverse $ take 50 $ reverse shown) else shown
  where
    shown = show thing

{- This ensures that when two lists have the same content, their expression
 - paths will be links to the same cached path. That causes them to get
 - properly deduplicated when used in a set operation. It also makes the .tree
 - test files much stricter, since they'll change if any list element changes.
 - The Maybe is in case you only need to write lists as part of a larger list
 - and don't want individual outpaths.
 -
 - TODO switch to md5sum/hashContent?
 - TODO does it need to handle a race condition when writing to the cache?
 - TODO any reason to keep original extensions instead of all using .txt?
 -      oh, if we're testing extensions anywhere. lets not do that though
 -}
writeCachedLines :: OrthoLangConfig -> Locks -> FilePath -> [String] -> Action ()
writeCachedLines cfg ref outPath content = do
  let cache = cachedLinesPath cfg content
  -- TODO show as OrthoLangPath?
  debugA' "writeCachedLines" $ first50 content ++ " -> " ++ last50 cache
      -- lock  = cache <.> "lock"
  -- liftIO $ createDirectoryIfMissing True cDir
  withWriteOnce ref cache $ writeFile' cache $ unlines content -- TODO is this strict?
  -- unlessExists outPath $ -- TODO remove?
  symlink cfg ref (toOrthoLangPath cfg outPath) (toOrthoLangPath cfg cache)

-- like writeCachedLines but starts from a file written by a script
-- TODO remove in favor of sanitizeFileInPlace?
writeCachedVersion :: OrthoLangConfig -> Locks -> FilePath -> FilePath -> Action ()
writeCachedVersion cfg ref outPath inPath = do
  content <- fmap lines $ readFileStrict' cfg ref inPath
  let content' = map (toGeneric cfg) content
  writeCachedLines cfg ref outPath content'

-- TODO take a OrthoLangPath for the out file too
-- TODO take Path Abs File and convert them... or Path Rel File?
-- TODO explicit case for empty lists that isn't just an empty file!
writePaths :: OrthoLangConfig -> Locks -> FilePath -> [OrthoLangPath] -> Action ()
writePaths cfg ref out cpaths = writeCachedLines cfg ref out paths >> trackWrite paths -- TODO trackwrite'?
  where
    paths = if null cpaths then ["<<emptylist>>"] else map cutPathString cpaths

writePath :: OrthoLangConfig -> Locks -> FilePath -> OrthoLangPath -> Action ()
writePath cfg ref out path = do
  debugA' "writePath" (show path)
  writePaths cfg ref out [path]

writeLits :: OrthoLangConfig -> Locks -> FilePath -> [String] -> Action ()
writeLits cfg ref path lits = do
  debugA' "writeLits" $ show lits ++ " -> writeCachedLines " ++ show lits'
  writeCachedLines cfg ref path lits'
  where
    lits' = if null lits then ["<<emptylist>>"] else checkLits lits

-- TODO any need to prevent writing <<emptystr>> in a .num?
--      (seems almost certain to be caught on reading later)
writeLit :: OrthoLangConfig -> Locks -> FilePath -> String -> Action ()
writeLit cfg ref path lit = do
  -- debugS (pack "core.actions.writeLit") (pack $ "writeLit lit: '" ++ lit ++ "'")
  -- debugS (pack "core.actions.writeLit") (pack $ "writeLit lits: '" ++ lits ++ "'")
  debugA' "writeLit" $ show lit ++ " -> writeLits " ++ show lits
  writeLits cfg ref path lits
  where
    lits = [if null lit then "<<emptystr>>" else lit]

{- Write a "list of whatever". Mostly for generic set operations. You include
 - the OrthoLangType (of each element, not the list!) so it knows how to convert
 - to/from String, and then within the function you treat them as Strings.
 -}
writeStrings :: OrthoLangType -> OrthoLangConfig -> Locks
             -> FilePath -> [String] -> Action ()
writeStrings etype cfg ref out whatevers = do
  debugA' "writeStrings"
    $ first50 (take 3 whatevers) ++ " (each " ++ extOf etype ++ ") -> " ++ last50 out
  if etype `elem` [str, num]
    then writeLits  cfg ref out whatevers
    else writePaths cfg ref out $ map (toOrthoLangPath cfg) whatevers

writeString :: OrthoLangType -> OrthoLangConfig -> Locks
            -> FilePath -> String -> Action ()
writeString etype cfg ref out whatever = writeStrings etype cfg ref out [whatever]

{- Turns out there's a race condition during `repeat` calls, because the same
 - literals are being compiled in each thread at roughly the same time. The way
 - I solved it was 1) check if the file as written already, and 2) if there's a
 - conflict in the middle of the operation anyway, ignore the error. Whichever
 - thread got there first will be writing the same exact text anyway.
 -}

-- TODO rename like myReadFile, myReadLines?
-- TODO move to Util?
trackWrite' :: OrthoLangConfig -> [FilePath] -> Action ()
trackWrite' cfg fs = do
  -- mapM_ (assertNonEmptyFile cfg ref) fs
  -- also ensure it only gets written once:
  -- liftIO $ mapM_ (\f -> setFileMode f 444) fs -- TODO is 444 right? test it
  liftIO $ mapM_ ((\f -> catchAny f (\_ -> return ())) . setReadOnly cfg) fs -- TODO is 444 right? test it
  trackWrite $ traceShow "core.actions.trackWrite'" fs

setReadOnly :: OrthoLangConfig -> FilePath -> IO ()
setReadOnly cfg path = do
  path' <- resolveSymlinks (Just $ cfgTmpDir cfg) path
  setFileMode path' 444

-------------------------
-- run system commands --
-------------------------

{- OrthoLang requires explicit empty files with contents like "<<emptylist>>" to
 - distinguish them from runtime errors. This function replaces those with
 - actual empty files before passing them to a cmd, so logic for that
 - doesn't have to be duplicated over and over.
 -}
fixEmptyText :: OrthoLangConfig -> Locks -> FilePath -> Action FilePath
fixEmptyText cfg ref path = do
  need' cfg ref "core.actions.fixEmptyText" [path] -- Note isEmpty does this too
  empty <- isEmpty ref path
  return $ if empty then "/dev/null" else path -- TODO will /dev/null work?

-- TODO call this when exiting nonzero and/or exception thrown
-- TODO take a list of globs and resolve them to files
-- TODO delete the files, telling shake if possible
-- TODO print a message for the user
-- TODO raise/re-raise an exception

-- Shake's command_ adapted to work with wrapperScript and wrapperLimit if used.
-- ptns is a list of patterns for files to delete in case the cmd fails.
-- TODO gather shake stuff into a Shake.hs module?
--      could have config, debug, wrappedCmd, eval...
-- TODO separate wrappedReadCmd with a shared lock?

-- TODO multiple out patterns too?
data CmdDesc = CmdDesc
  { cmdBinary        :: String
  , cmdArguments     :: [String] -- TODO auto-include outpath before these?
  , cmdFixEmpties    :: Bool -- TODO version for after too?
  , cmdInPatterns    :: [String]
  , cmdExtraOutPaths :: [FilePath]
  , cmdSanitizePaths :: [FilePath]
  , cmdRmPatterns    :: [String]
  , cmdOptions       :: [CmdOption] -- TODO remove?
  , cmdOutPath       :: FilePath -- TODO Maybe?
  , cmdParallel      :: Bool
  , cmdExitCode      :: ExitCode -- expected exit code (others are errors)
  }

{- One wrappedCmd equivalent function to rule them all.
 - It's controlled by a CmdDesc record instead of a bunch of unnamed positional arguments,
 - and will help enforce consistency because all the patterns are enforced in one place.
 - TODO should it track the .out and .err files, or ignore them?
 - TODO take OrthoLangState instead of individual cfg + locks?
 - TODO if exit is wrong (usually non-zero), cat out stderr for user
 - TODO if stdout == outfile, put it there and skip the .out file altogether, or symlink it?
 -}
runCmd :: OrthoLangConfig -> Locks -> CmdDesc -> Action ()
runCmd cfg ref@(disk, _) desc = do
  let stdoutPath = cmdOutPath desc <.> "out"
      stderrPath = cmdOutPath desc <.> "err"
      dbg = debugA' "runCmd"
  -- liftIO $ delay 1000000

  inPaths  <- fmap concat $ liftIO $ mapM globFiles $ cmdInPatterns desc
  inPaths' <- if cmdFixEmpties desc
                then mapM (fixEmptyText cfg ref) inPaths
                else need' cfg ref "core.actions.runCmd" inPaths >> return inPaths
  -- liftIO $ createDirectoryIfMissing True $ takeDirectory stdoutPath
  dbg $ "wrappedCmd acquiring read locks on " ++ show inPaths'
  -- dbg $ pack $ "wrappedCmd cfg: " ++ show cfg
  let parLockFn = if cmdParallel desc
                    then \f -> withResource (cfgParLock cfg) 1 f
                    else id
      -- writeLockPaths = (cmdOutPath desc):(cmdExtraOutPaths desc)
      writeLockFn fn = do
        dbg $ "runCmd acquiring main write lock: " ++ show (cmdOutPath desc)
        withWriteOnce ref (cmdOutPath desc) $ do
          dbg $ "runCmd acquiring extra write locks: " ++ show (cmdExtraOutPaths desc)
          withWriteLocks' ref (cmdExtraOutPaths desc) $ parLockFn fn

  -- TODO is 5 a good number of times to retry? can there be increasing delay or something?
  writeLockFn $ withReadLocks' ref inPaths' $ do
    -- TODO remove opts?
    -- TODO always assume disk is 1?
    Exit code <- withResource disk (length inPaths + 1) $ case cfgWrapper cfg of
      Nothing -> command (cmdOptions desc) (cmdBinary desc) (cmdArguments desc)
      Just w  -> command (Shell:cmdOptions desc) w [escape $ unwords (cmdBinary desc:cmdArguments desc)]
    Exit _ <- command [] "sync" [] -- TODO is this needed?
    -- This is disabled because it can make the logs really big
    -- dbg $ "wrappedCmd: " ++ bin ++ " " ++ show args ++ " -> " ++ show (out, err, code')
    -- trackWrite' cfg [stdoutPath, stderrPath] -- TODO does this happen here?
    -- return ()

    -- TODO use exitWith here?
    when (code /= cmdExitCode desc) $
      handleCmdError cfg ref (cmdBinary desc) code stderrPath (cmdRmPatterns desc)

  let sPaths = stdoutPath:stderrPath:cmdSanitizePaths desc -- TODO main outpath too?
  -- sanitizeFilesInPlace cfg ref $ cmdSanitizePaths desc
  sanitizeFilesInPlace cfg ref sPaths

  return () -- TODO out, err, code here?

-- TODO does this do directories?
-- TODO does this work on absolute paths?
matchPattern :: OrthoLangConfig -> String -> Action [FilePath]
matchPattern cfg ptn = liftIO $ globDir1 (compile ptn) (cfgTmpDir cfg)

handleCmdError :: OrthoLangConfig -> Locks -> String -> ExitCode -> FilePath -> [String] -> Action a
handleCmdError cfg ref bin n stderrPath rmPatterns = do
  hasErr <- doesFileExist stderrPath
  errMsg2 <- if hasErr
               then do
                 errTxt <- readFileStrict' cfg ref stderrPath
                 return $ ["Stderr was:", errTxt]
               else return []
  -- let files' = sort $ nub rmPatterns
  files' <- fmap concat $ mapM (matchPattern cfg) rmPatterns
  let files'' = sort $ nub files'
  liftIO $ if isJust $ cfgDebug cfg
    then putStrLn $ "these files would be deleted, --debug is enabled: " ++ show files''
    else rmAll files'' -- TODO should these be patterns to match first?
  -- TODO does this get caught by recoverAll in eval? make sure it does!
  -- TODO also try adding a manual flush before each external command in case it's an io delay thing
  let errMsg = if isJust $ cfgDebug cfg
                 then []
                 else
                   [ bin ++ " failed with " ++ show n ++ "."
                   , "The files it was working on have been deleted:"
                   ] ++ files'
  error $ unlines $ errMsg ++ errMsg2

----------
-- misc --
----------

-- This is the only function that should access readFileStrict' directly;
-- all others go through readStr and readList. TODO no longer true?
-- TODO use a OrthoLangPath here?
-- digestFile :: OrthoLangConfig -> Locks -> FilePath -> Action String
-- digestFile cfg ref path = readFileStrict' cfg ref path >>= return . digest

-- TODO fixEmpties should be False here, but don't want to break existing tmpdir just yet
-- TODO take mod time into account to avoid re-hashing (see if Shake exports that code)
hashContent :: OrthoLangConfig -> Locks -> OrthoLangPath -> Action String
hashContent cfg ref@(disk, _) path = do
  -- alwaysRerun -- TODO does this help?
  need' cfg ref "hashContent" [path']
  -- Stdout out <- withReadLock' ref path' $ command [] "md5sum" [path']
  -- out <- wrappedCmdOut False True cfg ref [path'] [] [] "md5sum" [path'] -- TODO runCmd here
  Stdout out <- withReadLock' ref path' $ withResource disk 1 $ case cfgWrapper cfg of
    Nothing -> command [] "md5sum" [path']
    Just w  -> command [Shell] w ["md5sum", path']
  -- liftIO $ putStrLn $ "out: " ++ out
  let md5 = take digestLength out
  -- liftIO $ putStrLn $ "md5: " ++ md5
  return md5
  where
    path' = fromOrthoLangPath cfg path

{- Hashing doesn't save any space here, but it puts the hashes in
 - src/tests/plots/*.tree so we can test that the plots don't change.
 -
 - What it does:
 -   1. make a random temporary path
 -   2. pass that to actFn to make the actual output
 -   3. hash the output to determine cache path
 -   4. symlink hash path -> tmp path, actual outPath -> hash path
 -
 - TODO remove if ggplot turns out to be nondeterministic
 - TODO use hash of expr + original ext instead so it looks nicer?
 -}
withBinHash :: OrthoLangConfig -> Locks -> OrthoLangExpr -> OrthoLangPath
            -> (OrthoLangPath -> Action ()) -> Action ()
withBinHash cfg ref expr outPath actFn = do
  let binDir'  = fromOrthoLangPath cfg $ cacheDir cfg "bin"
      outPath' = fromOrthoLangPath cfg outPath
  liftIO $ createDirectoryIfMissing True binDir'
  let binTmp' = binDir' </> digest expr <.> takeExtension outPath'
      binTmp  = toOrthoLangPath cfg binTmp'
  _ <- actFn binTmp
  md5 <- hashContent cfg ref binTmp
  let binOut' = binDir' </> md5 <.> takeExtension outPath'
      binOut  = toOrthoLangPath cfg binOut'
  -- debugS $ "withBinHash binOut: "  ++ show binOut
  -- debugS $ "withBinHash binOut': " ++ show binOut'
  debugA' "withBinHash" $ show binTmp ++ " -> " ++ show binOut
  symlink cfg ref binOut  binTmp
  symlink cfg ref outPath binOut

{- Takes source and destination paths in the tmpdir and makes a path between
 - them with the right number of dots.
 - TODO check that the OrthoLangPath is in TMPDIR, not WORKDIR!
 -}
tmpLink :: OrthoLangConfig -> FilePath -> FilePath -> FilePath
tmpLink cfg src dst = dots </> tmpRel dst
  where
    tmpRel  = makeRelative $ cfgTmpDir cfg
    dots    = foldr1 (</>) $ take (nSeps - 1) $ repeat ".."
    nSeps   = length $ splitOneOf pathSeparators $ tmpRel src

{- Note that src here means what's sometimes called the destination. The first
 - arg should be the symlink path and the second the file it points to. (it
 - was going to be kind of confusing either way)
 -}
symlink :: OrthoLangConfig -> Locks -> OrthoLangPath -> OrthoLangPath -> Action ()
symlink cfg ref src dst = withWriteOnce ref src' $ do
  liftIO $ createDirectoryIfMissing True $ takeDirectory src'
  liftIO $ ignoreExistsError $ createSymbolicLink dstr src'
  trackWrite' cfg [src']
  where
    src' = fromOrthoLangPath cfg src
    dst' = fromOrthoLangPath cfg dst
    dstr = tmpLink cfg src' dst' -- TODO use cutpaths here too?

-- Apply toGeneric to sanitize the output(s) of a script
-- Should be done before trackWrite to avoid confusing Shake
sanitizeFileInPlace :: OrthoLangConfig -> Locks -> FilePath -> Action ()
sanitizeFileInPlace cfg ref path = do
  -- txt <- readFileStrict' cfg ref path
  exists <- doesFileExist path
  when exists $ do
    txt <- liftIO $ readFileStrict ref path -- can't use need here
    let txt' = toGeneric cfg txt
    -- liftIO $ putStrLn $ "txt': '" ++ txt' ++ "'"
    writeFile' path txt'
    trackWrite' cfg [path]
    -- writeFile' path $ toGeneric cfg txt
    -- writeCachedLines cfg ref path []

-- Apply toGeneric to sanitize the output(s) of a script
-- Should be done before trackWrite to avoid confusing Shake
sanitizeFilesInPlace :: OrthoLangConfig -> Locks -> [FilePath] -> Action ()
sanitizeFilesInPlace cfg ref = mapM_ (sanitizeFileInPlace cfg ref)
