{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ShortCut (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,7,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/global/home/users/jefdaj/shortcut/src/.stack-work/install/x86_64-linux-nix/lts-8.6/8.0.2/bin"
libdir     = "/global/home/users/jefdaj/shortcut/src/.stack-work/install/x86_64-linux-nix/lts-8.6/8.0.2/lib/x86_64-linux-ghc-8.0.2/ShortCut-0.7.0.0"
dynlibdir  = "/global/home/users/jefdaj/shortcut/src/.stack-work/install/x86_64-linux-nix/lts-8.6/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/global/home/users/jefdaj/shortcut/src/.stack-work/install/x86_64-linux-nix/lts-8.6/8.0.2/share/x86_64-linux-ghc-8.0.2/ShortCut-0.7.0.0"
libexecdir = "/global/home/users/jefdaj/shortcut/src/.stack-work/install/x86_64-linux-nix/lts-8.6/8.0.2/libexec"
sysconfdir = "/global/home/users/jefdaj/shortcut/src/.stack-work/install/x86_64-linux-nix/lts-8.6/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ShortCut_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ShortCut_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ShortCut_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ShortCut_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ShortCut_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ShortCut_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
