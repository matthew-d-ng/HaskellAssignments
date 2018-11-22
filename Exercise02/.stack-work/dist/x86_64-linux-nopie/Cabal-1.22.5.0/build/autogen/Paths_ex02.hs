module Paths_ex02 (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/users/ugrad/ngm1/code/cs3016/assignments/CS3016-1819/Exercise02/.stack-work/install/x86_64-linux-nopie/lts-6.19/7.10.3/bin"
libdir     = "/users/ugrad/ngm1/code/cs3016/assignments/CS3016-1819/Exercise02/.stack-work/install/x86_64-linux-nopie/lts-6.19/7.10.3/lib/x86_64-linux-ghc-7.10.3/ex02-0.1.0.0-FNe9vPG3iyEKTyBf8I5Adp"
datadir    = "/users/ugrad/ngm1/code/cs3016/assignments/CS3016-1819/Exercise02/.stack-work/install/x86_64-linux-nopie/lts-6.19/7.10.3/share/x86_64-linux-ghc-7.10.3/ex02-0.1.0.0"
libexecdir = "/users/ugrad/ngm1/code/cs3016/assignments/CS3016-1819/Exercise02/.stack-work/install/x86_64-linux-nopie/lts-6.19/7.10.3/libexec"
sysconfdir = "/users/ugrad/ngm1/code/cs3016/assignments/CS3016-1819/Exercise02/.stack-work/install/x86_64-linux-nopie/lts-6.19/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ex02_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ex02_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ex02_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ex02_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ex02_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
