{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances #-}

-- |
-- Module:      System.FilePath.Manip
-- Copyright:   Bryan O'Sullivan
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   unstable
-- Portability: Unix-like systems (requires flexible instances)

module System.FilePath.Manip (
      Streamable(..)
    , renameWith
    , modifyWith
    , modifyWithBackup
    , modifyInPlace
    ) where

import Control.Exception
import Control.Monad (liftM)
import Data.Bits ((.&.))
import System.Directory (removeFile)
import System.IO (Handle, IOMode(..), hClose, openFile)
import System.PosixCompat.Files (fileMode, getFileStatus, rename, setFileMode)
import System.PosixCompat.Temp (mkstemp)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified System.IO as I

-- | Use a renaming function to generate a new name for a file, then
-- rename it.
renameWith :: (FilePath -> FilePath) -- ^ function to rename with
           -> FilePath -- ^ file to rename
           -> IO ()

renameWith f path = rename path (f path)

-- | Type class for string manipulation over files.
class Streamable a where
    -- | Read the entire contents of a 'Handle'.
    readAll :: Handle -> IO a
    -- | Write an entire string to a 'Handle'.
    writeAll :: Handle -> a -> IO ()

instance Streamable B.ByteString where
    readAll = B.hGetContents
    writeAll = B.hPut

instance Streamable L.ByteString where
    readAll = L.hGetContents
    writeAll = L.hPut

instance Streamable String where
    readAll = I.hGetContents
    writeAll = I.hPutStr

-- | Modify a file in place using the given function.  This is
-- performed by writing to a temporary file, then renaming it on top of
-- the existing file when done.
modifyInPlace :: Streamable a => (a -> a) -- ^ transformation function
              -> FilePath -- ^ name of file to modify
              -> IO ()

modifyInPlace = modifyWith (flip rename)

-- | Modify a file in place using the given function.  The original
-- copy of the file is saved under a new name.  This is performed by
-- writing to a temporary file; renaming the original file to its new
-- name; then renaming the temporary file to the original name.
--
-- Example:
--
-- @
--     -- save original file with a \".bak\" extension
--     'modifyWithBackup' (\<.\> \"bak\")
-- @ 
modifyWithBackup :: Streamable a =>
                    (FilePath -> FilePath) -- ^ chooses new name for original file
                 -> (a -> a) -- ^ transformation function
                 -> FilePath -- ^ name of file to modify
                 -> IO ()

modifyWithBackup f = modifyWith backup
    where backup path tmpPath = renameWith f path >> rename tmpPath path

-- | Modify a file in place using the given function.  The new content
-- is written to a temporary file.  Once this is complete, the file
-- manipulation action is called.  Its arguments are the names of the
-- original and temporary files.
--
-- Example:
--
-- @
--     'modifyInPlace' = 'modifyWith' (flip rename)
-- @ 
modifyWith :: Streamable a =>
                (FilePath -> FilePath -> IO ()) -- ^ file manipulation action
             -> (a -> a) -- ^ transformation function
             -> FilePath
             -> IO ()

modifyWith after transform path =
    bracket (openFile path ReadMode) hClose $ \ih -> do
        (tmpPath, oh) <- mkstemp (path ++ "XXXXXX")
        let ignore = return ()
            nukeTmp = handle (\(_::IOException) -> ignore) (removeFile tmpPath)
        handle (\(e::IOException) -> nukeTmp >> throw e) $ do
            bracket_ ignore (hClose oh) $
                readAll ih >>= return . transform >>= writeAll oh
            handle (\(_::IOException) -> nukeTmp) $ do
                mode <- fileMode `liftM` getFileStatus path
                setFileMode tmpPath (mode .&. 0777)
                after path tmpPath
