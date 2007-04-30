{-# OPTIONS_GHC -fglasgow-exts #-}

module System.FilePath.Manip (
      Modifiable(..)
    , renameWith
    , modifyWith
    , modifyWithBackup
    , modifyInPlace
    ) where

import Control.Exception (bracket, bracket_, handle, throwIO)
import Control.Monad (liftM)
import Data.Bits ((.&.))
import System.Directory (removeFile)
import System.IO (Handle, IOMode(..), hClose, hGetContents, hPutStr, openFile)
import System.Posix.Files (fileMode, getFileStatus, rename, setFileMode)
import System.Posix.Temp (mkstemp)
import Data.ByteString.Base (ByteString, LazyByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

renameWith :: (FilePath -> FilePath) -> FilePath -> IO ()

renameWith f path = rename path (f path)

class Modifiable a where
    pipeline :: (a -> a) -> Handle -> Handle -> IO ()

instance Modifiable ByteString where
    pipeline f ih oh = B.hGetContents ih >>= return . f >>= B.hPut oh

instance Modifiable LazyByteString where
    pipeline f ih oh = L.hGetContents ih >>= return . f >>= L.hPut oh

instance Modifiable String where
    pipeline f ih oh = hGetContents ih >>= return . f >>= hPutStr oh

modifyInPlace :: Modifiable a => (a -> a) -> FilePath -> IO ()

modifyInPlace = modifyWith (flip rename)

modifyWithBackup :: Modifiable a => (FilePath -> FilePath)
                 -> (a -> a)
                 -> FilePath
                 -> IO ()

modifyWithBackup f = modifyWith backup
    where backup path tmpPath = renameWith f path >> rename tmpPath path

modifyWith :: Modifiable a => (FilePath -> FilePath -> IO ())
             -> (a -> a)
             -> FilePath
             -> IO ()

modifyWith after transform path =
    bracket (openFile path ReadMode) hClose $ \ih -> do
        (tmpPath, oh) <- mkstemp (path ++ "XXXXXX")
        let ignore = return ()
            nukeTmp = handle (const ignore) (removeFile tmpPath)
        handle (\e -> nukeTmp >> throwIO e) $ do
            bracket_ ignore (hClose oh) $
                pipeline transform ih oh
            handle (const nukeTmp) $ do
                mode <- fileMode `liftM` getFileStatus path
                setFileMode tmpPath (mode .&. 0777)
                after path tmpPath
