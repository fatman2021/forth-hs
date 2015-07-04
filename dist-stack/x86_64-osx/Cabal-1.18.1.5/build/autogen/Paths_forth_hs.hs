module Paths_forth_hs (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/owainlewis/Code/forth-hs/.stack-work/install/x86_64-osx/lts-2.13/7.8.4/bin"
libdir     = "/Users/owainlewis/Code/forth-hs/.stack-work/install/x86_64-osx/lts-2.13/7.8.4/lib/x86_64-osx-ghc-7.8.4/forth-hs-0.1.0.0"
datadir    = "/Users/owainlewis/Code/forth-hs/.stack-work/install/x86_64-osx/lts-2.13/7.8.4/share/x86_64-osx-ghc-7.8.4/forth-hs-0.1.0.0"
libexecdir = "/Users/owainlewis/.cabal/libexec"
sysconfdir = "/Users/owainlewis/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "forth_hs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "forth_hs_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "forth_hs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "forth_hs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "forth_hs_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
