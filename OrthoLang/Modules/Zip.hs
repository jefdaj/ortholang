module OrthoLang.Modules.Zip
  where

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter

import Prelude hiding (zip)
import qualified Prelude

import Data.Maybe (fromJust)
import OrthoLang.Modules.Plots (listVarNames)
import OrthoLang.Locks (withReadLock)
import System.Process          (readProcess)
import Data.List (intersperse)
import Data.List.Split (splitOn)
import System.Directory           (createDirectoryIfMissing, removeDirectoryRecursive, copyFile)
import System.FilePath        ((</>), (<.>), takeDirectory)
import Data.List (isSuffixOf)
import Control.Monad (forM_)

------------
-- module --
------------

olModule :: Module
olModule = Module
  { mName = "Zip"
  , mDesc = "Archive lists of variables"
  , mTypes = [zip]
  , mGroups = []
  , mEncodings = []
  , mFunctions = [zipArchive, zipArchiveExplicit]
  }

zip :: Type
zip = Type
  { tExt  = "zip"
  , tDesc = "zip archives"
  , tShow = \cfg ref path -> do
      path' <- resolveSymlinks Nothing path
      out <- fmap lines $ withReadLock ref path' $ readProcess "zipinfo" [path'] []
      let header = fmap (toGeneric cfg) $ take 2 out
          files  = map (drop 53) $ drop 2 $ init out
          footer = last out
          out' = unlines $ header ++ files ++ [footer]
      return out'
  }

--------------------------
-- zip_archive_explicit --
--------------------------

-- | Hidden version of `zipArchive` that takes an explicit pre-loaded script and varnames file.
zipArchiveExplicit :: Function
zipArchiveExplicit = newFnA2
  "zip_archive_explicit"
  (ListSigs (Exactly str), ListSigs (Exactly Untyped))
  (Exactly zip)
  aZipArchiveExplicit
  [Hidden]

aZipArchiveExplicit :: NewAction2
aZipArchiveExplicit (ExprPath out') inNames inList = do
  cfg <- fmap fromJust $ getShakeExtra
  let loc = "modules.zip.aZipArchiveExplicit"
      out = toPath loc cfg out'

  -- create a temporary, unique hash-named dir for exported files
  let inputDir = takeDirectory out' </> "ortholang_" ++ digest loc out
  liftIO $ createDirectoryIfMissing True inputDir

  -- pair names with their paths
  -- note that this is an unrelated meaning of zip
  names <- readLits loc inNames
  paths <- fmap lines $ readFileStrict' inList -- TODO warning! these are probably not lits

  -- liftIO $ putStrLn $ "names: " ++ show names
  -- liftIO $ putStrLn $ "paths: " ++ show paths

  if null names
    then do
      -- case 1: entire list is empty (no args given)
      -- liftIO $ putStrLn $ "chose case 1"
      liftIO $ writeFile (inputDir </> "empty.list") "<<emptylist>>"

    else
      let n1 = head names
          p1 = fromPath loc cfg $ Path (head paths)
          dst = inputDir </> n1
      in if (".str.list" `isSuffixOf` n1 || ".num.list" `isSuffixOf` n1)
        then do
          -- case 2: entire list is a list of lits, and the first path points to it
          -- liftIO $ putStrLn $ "chose case 2"
          liftIO $ copyFile p1 dst

        else do
          -- case 3: handle each arg individually
          -- at this point we still don't know whether each path is really a path or a lit
          -- TODO rename var to reflect that
          -- liftIO $ putStrLn $ "chose case 3"
          let pairs = Prelude.zip paths names
          forM_ pairs $ \(path, name) -> writeOrtholangArg inputDir path name

  -- last step is to zip up the input dir and remove it
  -- this should probably depend on inputDir, but that causes a shake error
  aNewRulesS1 "deterministic_zip.py" (\d -> d {cmdNoNeedDirs = [inputDir]}) (ExprPath out') inputDir
  liftIO $ removeDirectoryRecursive inputDir

-- TODO should just be Path right?
writeOrtholangArg :: FilePath -> String -> String -> Action ()
writeOrtholangArg inputDir path name = do
  cfg <- fmap fromJust $ getShakeExtra
  let loc  = "modules.zip.writeOrtholangArg"
      -- path = fromPath loc cfg (Path path') -- TODO error here
      dst  = inputDir </> name
      exts = tail $ splitOn "." name

  if exts `elem` [["str"], ["num"]]
    then do
      -- case 3a: "path" was actually a single lit which should be written to file
      -- TODO rename var to reflect ambiguity
      -- liftIO $ putStrLn $ "chose case 3a"
      writeLit loc dst path

    else
      -- at this point we know the path is an actual Path, so treat it that way
      let path' = Path path
      in if (exts `elem` [["str", "list"], ["num", "list"]] || last exts /= "list")
        then do
          -- case 3b: path is a lit.list or a single non-lit type, and should be copied over
          -- (same action as case 2, but this one may be called recursively)
          -- liftIO $ putStrLn $ "chose case 3b"
          liftIO $ copyFile (fromPath loc cfg path') dst

        else do
          -- case 4: path is to a non-lit list type, so we should make a dir + copy elements into it
          -- this one may recursively call writeOrtholangArg again
          -- liftIO $ putStrLn $ "chose case 3c"
          writeOrtholangList inputDir path' name

writeOrtholangList :: FilePath -> Path -> String -> Action ()
writeOrtholangList inputDir path name = do
  cfg <- fmap fromJust $ getShakeExtra
  let loc   = "modules.zip.writeOrtholangList"
      name' = takeWhile (/= '.') name
      ext'  = concat $ intersperse "." $ tail $ init $ splitOn "." name
      iDir  = inputDir </> name'
  -- liftIO $ putStrLn $ "writeOrtholangList path: " ++ show path
  -- liftIO $ putStrLn $ "writeOrtholangList name': " ++ name'
  -- liftIO $ putStrLn $ "writeOrtholangList ext': " ++ ext'
  -- liftIO $ putStrLn $ "writeOrtholangList iDir: " ++ iDir
  liftIO $ createDirectoryIfMissing True iDir
  -- TODO no need to chdir right?
  paths <- readPaths loc $ fromPath loc cfg path -- now we know these are actual Paths
  -- liftIO $ putStrLn $ "writeOrtholangList paths: " ++ show paths
  let defNames = map (\i -> "item" ++ show i <.> ext') [(1 :: Int)..]
      named = Prelude.zip paths defNames -- note: unrelated meaning of zip
  forM_ named $ \(p, n) -> do
    -- liftIO $ putStrLn $ "recursing with p: " ++ show p
    -- liftIO $ putStrLn $ "recursing with n: " ++ show n
    writeOrtholangArg iDir (fromPath loc cfg p) n

-----------------
-- zip_archive --
-----------------

-- | User-facing version that auto-loads the script and captures any varnames in the untyped list.
zipArchive :: Function
zipArchive = newExprExpansion
  "zip_archive"
  [ListSigs (Exactly Untyped)]
  (Exactly zip)
  mZipArchive
  []

-- TODO rewrite Plots.hs functions to use expr expansions with varNames, like this
mZipArchive :: ExprExpansion
mZipArchive _ scr (Fun r ms ds _ [e@(Lst _ _ _ es)]) =
  let ns = listVarNames "input" scr es -- TODO pick up overall list name here?
  in Fun r ms ds "zip_archive_explicit" [ns, e]
mZipArchive _ _ e = error "modules.zip.mZipArchive" $ "bad argument: " ++ show e
