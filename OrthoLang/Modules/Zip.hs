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
import Data.List.Split (splitOn)
import System.Directory           (createDirectoryIfMissing, removeDirectoryRecursive, copyFile)
import System.FilePath        ((</>), takeDirectory)
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
      out <- withReadLock ref path' $ readProcess "zipinfo" [path'] []
      return $ toGeneric cfg out
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

  liftIO $ putStrLn $ "names: " ++ show names
  liftIO $ putStrLn $ "paths: " ++ show paths

  if null names
    then liftIO $ writeFile (inputDir </> "empty.list") "<<emptylist>>"

    else
      let n1 = head names
          p1 = head paths
          dst = inputDir </> n1
      in if (".str.list" `isSuffixOf` n1 || ".num.list" `isSuffixOf` n1)
        then do
          liftIO $ putStrLn $ "copyFile case 1: " ++ show p1 ++ " -> " ++ show dst
          liftIO $ copyFile p1 dst

        else do
          let pairs = Prelude.zip paths names
          forM_ pairs $ \(path, name) -> writeOrtholangArg inputDir path name

  -- last step is to zip up the input dir and remove it
  -- this should probably depend on inputDir, but that causes a shake error
  aNewRulesS1 "deterministic_zip.py" (\d -> d {cmdNoNeedDirs = [inputDir]}) (ExprPath out') inputDir
  liftIO $ removeDirectoryRecursive inputDir

-- TODO should just be Path right?
writeOrtholangArg :: FilePath -> String -> String -> Action ()
writeOrtholangArg inputDir path' name = do
  cfg <- fmap fromJust $ getShakeExtra
  let loc  = "modules.zip.writeOrtholangArg"
      path = toPath loc cfg path'
      dst  = inputDir </> name
      exts = tail $ splitOn "." name
  if exts `elem` [["str"], ["num"]]
    -- case 1: "path'" was actually a single lit which should be written to file
    then writeLit loc dst path'

    -- case 2/3: path is a lit.list or a single non-lit type, and should be copied over
    else if (exts `elem` [["str", "list"], ["num", "list"]] || last exts /= "list")
      then do
        liftIO $ putStrLn $ "copyFile case 2: " ++ show path ++ " -> " ++ show dst
        liftIO $ copyFile (fromPath loc cfg path) dst

      -- case 4: path is to a non-lit list type, so we should make a dir + copy elements into it
      else writeOrtholangList inputDir path' name

writeOrtholangList :: FilePath -> FilePath -> String -> Action ()
writeOrtholangList inputDir path name = do
  cfg <- fmap fromJust $ getShakeExtra
  let loc   = "modules.zip.writeOrtholangList"
      name' = takeWhile (/= '.') name
      ext'  = dropWhile (/= '.') name
      iDir  = inputDir </> name'
  liftIO $ createDirectoryIfMissing True iDir
  -- TODO no need to chdir right?
  paths <- readPaths loc path -- now we know these are actual Paths
  let defNames = map (\i -> "item" ++ show i ++ ext') [(1 :: Int)..]
      named = Prelude.zip paths defNames -- note: unrelated meaning of zip
  forM_ named $ \(p, n) -> writeOrtholangArg iDir (fromPath loc cfg p) n

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
