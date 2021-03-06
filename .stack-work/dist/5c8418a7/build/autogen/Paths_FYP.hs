{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_FYP (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\richi\\FYP\\.stack-work\\install\\b7328101\\bin"
libdir     = "C:\\Users\\richi\\FYP\\.stack-work\\install\\b7328101\\lib\\x86_64-windows-ghc-8.2.2\\FYP-0.1.0.0-DB97z4OHGdmISridARSpDG"
dynlibdir  = "C:\\Users\\richi\\FYP\\.stack-work\\install\\b7328101\\lib\\x86_64-windows-ghc-8.2.2"
datadir    = "C:\\Users\\richi\\FYP\\.stack-work\\install\\b7328101\\share\\x86_64-windows-ghc-8.2.2\\FYP-0.1.0.0"
libexecdir = "C:\\Users\\richi\\FYP\\.stack-work\\install\\b7328101\\libexec\\x86_64-windows-ghc-8.2.2\\FYP-0.1.0.0"
sysconfdir = "C:\\Users\\richi\\FYP\\.stack-work\\install\\b7328101\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "FYP_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "FYP_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "FYP_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "FYP_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "FYP_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "FYP_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
