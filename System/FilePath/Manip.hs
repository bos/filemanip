module System.FilePath.Manip (
      renameWith
    , modifyWith
    , modifyWithBackup
    , modifyInPlace
    ) where

import Control.Exception (bracket, bracket_, handle, throwIO)
import Control.Monad (liftM)
import Data.Bits ((.&.))
import System.Directory (removeFile)
import System.IO (IOMode(..), hClose, openFile)
import System.Posix.Files (fileMode, getFileStatus, rename, setFileMode)
import System.Posix.Temp (mkstemp)
import qualified Data.ByteString.Lazy.Char8 as L

renameWith :: (FilePath -> FilePath) -> FilePath -> IO ()

renameWith f path = rename path (f path)

modifyWith :: (FilePath -> FilePath -> IO ())
           -> (L.ByteString -> L.ByteString)
           -> FilePath
           -> IO ()

modifyWith after transform path =
    bracket (openFile path ReadMode) hClose $ \ih -> do
        (tmpPath, oh) <- mkstemp (path ++ "XXXXXX")
        let ignore = return ()
            nukeTmp = handle (const ignore) (removeFile tmpPath)
        handle (\e -> nukeTmp >> throwIO e) $ do
            bracket_ ignore (hClose oh) $
                transform `liftM` L.hGetContents ih >>= L.hPut oh
            handle (const nukeTmp) $ do
                mode <- fileMode `liftM` getFileStatus path
                setFileMode tmpPath (mode .&. 0777)
                after path tmpPath

modifyInPlace :: (L.ByteString -> L.ByteString) -> FilePath -> IO ()

modifyInPlace = modifyWith (flip rename)

modifyWithBackup :: (FilePath -> FilePath)
                 -> (L.ByteString -> L.ByteString)
                 -> FilePath
                 -> IO ()

modifyWithBackup f = modifyWith backup
    where backup path tmpPath = renameWith f path >> rename tmpPath path
