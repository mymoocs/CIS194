module Paths_cis194_eisenberg (
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

bindir     = "D:\\==OnlineLearning\\CIS194\\reisenberg\\homework\\haskell\\.cabal-sandbox\\bin"
libdir     = "D:\\==OnlineLearning\\CIS194\\reisenberg\\homework\\haskell\\.cabal-sandbox\\i386-windows-ghc-7.6.3\\cis194-eisenberg-0.1.0.0"
datadir    = "D:\\==OnlineLearning\\CIS194\\reisenberg\\homework\\haskell\\.cabal-sandbox\\i386-windows-ghc-7.6.3\\cis194-eisenberg-0.1.0.0"
libexecdir = "D:\\==OnlineLearning\\CIS194\\reisenberg\\homework\\haskell\\.cabal-sandbox\\cis194-eisenberg-0.1.0.0"
sysconfdir = "D:\\==OnlineLearning\\CIS194\\reisenberg\\homework\\haskell\\.cabal-sandbox\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cis194_eisenberg_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cis194_eisenberg_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cis194_eisenberg_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cis194_eisenberg_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cis194_eisenberg_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
