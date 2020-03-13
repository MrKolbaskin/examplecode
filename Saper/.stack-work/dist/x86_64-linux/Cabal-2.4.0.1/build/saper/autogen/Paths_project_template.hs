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

bindir     = "/home/alex/PRAK/Haskell/examplecode/Saper/.stack-work/install/x86_64-linux/1e379af94ec8a18a78bfb404a58b5ea155b44a35cd882b2e5c846e243602c63f/8.6.5/bin"
libdir     = "/home/alex/PRAK/Haskell/examplecode/Saper/.stack-work/install/x86_64-linux/1e379af94ec8a18a78bfb404a58b5ea155b44a35cd882b2e5c846e243602c63f/8.6.5/lib/x86_64-linux-ghc-8.6.5/project-template-0.0.0.0-EAwOkPl9OBWFIUvDQaD8dZ-saper"
dynlibdir  = "/home/alex/PRAK/Haskell/examplecode/Saper/.stack-work/install/x86_64-linux/1e379af94ec8a18a78bfb404a58b5ea155b44a35cd882b2e5c846e243602c63f/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/alex/PRAK/Haskell/examplecode/Saper/.stack-work/install/x86_64-linux/1e379af94ec8a18a78bfb404a58b5ea155b44a35cd882b2e5c846e243602c63f/8.6.5/share/x86_64-linux-ghc-8.6.5/project-template-0.0.0.0"
libexecdir = "/home/alex/PRAK/Haskell/examplecode/Saper/.stack-work/install/x86_64-linux/1e379af94ec8a18a78bfb404a58b5ea155b44a35cd882b2e5c846e243602c63f/8.6.5/libexec/x86_64-linux-ghc-8.6.5/project-template-0.0.0.0"
sysconfdir = "/home/alex/PRAK/Haskell/examplecode/Saper/.stack-work/install/x86_64-linux/1e379af94ec8a18a78bfb404a58b5ea155b44a35cd882b2e5c846e243602c63f/8.6.5/etc"

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
