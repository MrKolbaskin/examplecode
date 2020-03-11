{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_project_template (
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
version = Version [0,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/vladimir/Desktop/msu/Haskel/Saper/.stack-work/install/x86_64-osx/cd1ef797d37a91f2225d2b62785257aa160c6fe084b36dadfb0a318ab4c2c676/8.6.5/bin"
libdir     = "/Users/vladimir/Desktop/msu/Haskel/Saper/.stack-work/install/x86_64-osx/cd1ef797d37a91f2225d2b62785257aa160c6fe084b36dadfb0a318ab4c2c676/8.6.5/lib/x86_64-osx-ghc-8.6.5/project-template-0.0.0.0-EA2QFcsYN3A5Mqmci25fGt"
dynlibdir  = "/Users/vladimir/Desktop/msu/Haskel/Saper/.stack-work/install/x86_64-osx/cd1ef797d37a91f2225d2b62785257aa160c6fe084b36dadfb0a318ab4c2c676/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/vladimir/Desktop/msu/Haskel/Saper/.stack-work/install/x86_64-osx/cd1ef797d37a91f2225d2b62785257aa160c6fe084b36dadfb0a318ab4c2c676/8.6.5/share/x86_64-osx-ghc-8.6.5/project-template-0.0.0.0"
libexecdir = "/Users/vladimir/Desktop/msu/Haskel/Saper/.stack-work/install/x86_64-osx/cd1ef797d37a91f2225d2b62785257aa160c6fe084b36dadfb0a318ab4c2c676/8.6.5/libexec/x86_64-osx-ghc-8.6.5/project-template-0.0.0.0"
sysconfdir = "/Users/vladimir/Desktop/msu/Haskel/Saper/.stack-work/install/x86_64-osx/cd1ef797d37a91f2225d2b62785257aa160c6fe084b36dadfb0a318ab4c2c676/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "project_template_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "project_template_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "project_template_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "project_template_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "project_template_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "project_template_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
