{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module:      System.FilePath.Find
-- Copyright:   Bryan O'Sullivan
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   unstable
-- Portability: Unix-like systems (requires newtype deriving)
--
-- This module provides functions for traversing a filesystem
-- hierarchy.  The 'find' function generates a lazy list of matching
-- files, while 'fold' performs a left fold.
--
-- Both 'find' and 'fold' allow fine control over recursion, using the
-- 'FindClause' type.  This type is also used to pre-filter the results
-- returned by 'find'.
--
-- The 'FindClause' type lets you write filtering and recursion
-- control expressions clearly and easily.
--
-- For example, this clause matches C source files.
--
-- @
-- 'extension' '==?' \".c\" '||?' 'extension' '==?' \".h\"
-- @
--
-- Because 'FindClause' is a monad, you can use the usual monad
-- machinery to, for example, lift pure functions into it.
--
-- Here's a clause that will return 'True' for any file whose
-- directory name contains the word @\"temp\"@.
--
-- @
-- (isInfixOf \"temp\") \`liftM\` 'directory'
-- @

module System.FilePath.Find (
      FileInfo(..)
    , FileType(..)
    , FindClause
    , FilterPredicate
    , RecursionPredicate

    -- * Simple entry points
    , find
    , fold

    -- * More expressive entry points
    , findWithHandler
    , foldWithHandler

    -- * Helper functions
    , evalClause
    , statusType
    , liftOp

    -- * Combinators for controlling recursion and filtering behaviour
    , filePath
    , fileStatus
    , depth
    , fileInfo

    , always
    , extension
    , directory
    , fileName

    , fileType

    , contains

    -- ** Combinator versions of 'F.FileStatus' functions from "System.Posix.Files"
    -- $statusFunctions

    , deviceID
    , fileID
    , fileOwner
    , fileGroup
    , fileSize
    , linkCount
    , specialDeviceID
    , fileMode
    , accessTime
    , modificationTime
    , statusChangeTime

    -- *** Convenience combinators for file status
    , filePerms
    , anyPerms

    -- ** Combinators for canonical path and name
    , canonicalPath
    , canonicalName

    -- ** Combinators that operate on symbolic links
    , readLink
    , followStatus

    -- ** Common binary operators, lifted as combinators
    -- $binaryOperators
    , (~~?)
    , (/~?)
    , (==?)
    , (/=?)
    , (>?)
    , (<?)
    , (>=?)
    , (<=?)
    , (.&.?)

    -- ** Combinators for gluing clauses together
    , (&&?)
    , (||?)
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative)
#endif
import qualified Control.Exception as E
import Control.Exception (IOException, handle)
import Control.Monad (foldM, forM, liftM, liftM2)
import Control.Monad.State (State, evalState, get)
import Data.Bits (Bits, (.&.))
import Data.List (sort)
import System.Directory (getDirectoryContents, canonicalizePath)
import System.FilePath ((</>), takeDirectory, takeExtension, takeFileName)
import System.FilePath.GlobPattern (GlobPattern, (~~), (/~))
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import qualified System.PosixCompat.Files as F
import qualified System.PosixCompat.Types as T

-- | Information collected during the traversal of a directory.
data FileInfo = FileInfo
    {
      infoPath :: FilePath -- ^ file path
    , infoDepth :: Int -- ^ current recursion depth
    , infoStatus :: F.FileStatus -- ^ status of file
    } deriving (Eq)

instance Eq F.FileStatus where
    a == b = F.deviceID a == F.deviceID b &&
             F.fileID a == F.fileID b

-- | Construct a 'FileInfo' value.

mkFI :: FilePath -> Int -> F.FileStatus -> FileInfo

mkFI = FileInfo

-- | Monadic container for file information, allowing for clean
-- construction of combinators.  Wraps the 'State' monad, but doesn't
-- allow 'get' or 'put'.
newtype FindClause a = FC { runFC :: State FileInfo a }
    deriving (Functor, Applicative, Monad)

-- | Run the given 'FindClause' on the given 'FileInfo' and return its
-- result.  This can be useful if you are writing a function to pass
-- to 'fold'.
--
-- Example:
--
-- @
-- myFoldFunc :: a -> 'FileInfo' -> a
-- myFoldFunc a i = let useThisFile = 'evalClause' ('fileName' '==?' \"foo\") i
--                  in if useThisFile
--                     then fiddleWith a
--                     else a
-- @
evalClause :: FindClause a -> FileInfo -> a
evalClause = evalState . runFC

evalFI :: FindClause a
       -> FilePath
       -> Int
       -> F.FileStatus
       -> a
evalFI m p d s = evalClause m (mkFI p d s)

-- | Return the current 'FileInfo'.
fileInfo :: FindClause FileInfo

fileInfo = FC $ get

-- | Return the name of the file being visited.
filePath :: FindClause FilePath

filePath = infoPath `liftM` fileInfo

-- | Return the current recursion depth.
depth :: FindClause Int

depth = infoDepth `liftM` fileInfo

-- | Return the 'F.FileStatus' for the current file.
fileStatus :: FindClause F.FileStatus

fileStatus = infoStatus `liftM` fileInfo

type FilterPredicate = FindClause Bool
type RecursionPredicate = FindClause Bool

-- | List the files in the given directory, sorted, and without \".\"
-- or \"..\".
getDirContents :: FilePath -> IO [FilePath]

getDirContents dir = (sort . filter goodName) `liftM` getDirectoryContents dir
    where goodName "." = False
          goodName ".." = False
          goodName _ = True

-- | Search a directory recursively, with recursion controlled by a
-- 'RecursionPredicate'.  Lazily return a sorted list of all files
-- matching the given 'FilterPredicate'.  Any errors that occur are
-- dealt with by the given handler.
findWithHandler ::
    (FilePath -> IOException -> IO [FilePath]) -- ^ error handler
    -> RecursionPredicate -- ^ control recursion into subdirectories
    -> FilterPredicate -- ^ decide whether a file appears in the result
    -> FilePath -- ^ directory to start searching
    -> IO [FilePath] -- ^ files that matched the 'FilterPredicate'

findWithHandler errHandler recurse filt path0 =
    handle (errHandler path0) $ F.getSymbolicLinkStatus path0 >>= visit path0 0
  where visit path depth st =
            if F.isDirectory st && evalFI recurse path depth st
              then unsafeInterleaveIO (traverse path (succ depth) st)
              else filterPath path depth st []
        traverse dir depth dirSt = do
            names <- E.catch (getDirContents dir) (errHandler dir)
            filteredPaths <- forM names $ \name -> do
                let path = dir </> name
                unsafeInterleaveIO $ handle (errHandler path)
                    (F.getSymbolicLinkStatus path >>= visit path depth)
            filterPath dir depth dirSt (concat filteredPaths)
        filterPath path depth st result =
            return $ if evalFI filt path depth st
                then path:result
                else result

-- | Search a directory recursively, with recursion controlled by a
-- 'RecursionPredicate'.  Lazily return a sorted list of all files
-- matching the given 'FilterPredicate'.  Any errors that occur are
-- ignored, with warnings printed to 'stderr'.
find :: RecursionPredicate -- ^ control recursion into subdirectories
     -> FilterPredicate -- ^ decide whether a file appears in the result
     -> FilePath -- ^ directory to start searching
     -> IO [FilePath] -- ^ files that matched the 'FilterPredicate'

find = findWithHandler warnOnError
    where warnOnError path err =
              hPutStrLn stderr (path ++ ": " ++ show err) >> return []

-- | Search a directory recursively, with recursion controlled by a
-- 'RecursionPredicate'.  Fold over all files found.  Any errors that
-- occur are dealt with by the given handler.  The fold is strict, and
-- run from \"left\" to \"right\", so the folded function should be
-- strict in its left argument to avoid space leaks.  If you need a
-- right-to-left fold, use 'foldr' on the result of 'findWithHandler'
-- instead.
foldWithHandler
    :: (FilePath -> a -> IOException -> IO a) -- ^ error handler
    -> RecursionPredicate -- ^ control recursion into subdirectories
    -> (a -> FileInfo -> a) -- ^ function to fold with
    -> a -- ^ seed value for fold
    -> FilePath -- ^ directory to start searching
    -> IO a -- ^ final value after folding

foldWithHandler errHandler recurse f state path =
    handle (errHandler path state) $
        F.getSymbolicLinkStatus path >>= visit state path 0
  where visit state path depth st =
            if F.isDirectory st && evalFI recurse path depth st
            then traverse state path (succ depth) st
            else let state' = f state (mkFI path depth st)
                 in state' `seq` return state'
        traverse state dir depth dirSt = handle (errHandler dir state) $
            getDirContents dir >>=
                let state' = f state (mkFI dir depth dirSt)
                in state' `seq` flip foldM state' (\state name ->
                    handle (errHandler dir state) $
                    let path = dir </> name
                    in F.getSymbolicLinkStatus path >>= visit state path depth)

-- | Search a directory recursively, with recursion controlled by a
-- 'RecursionPredicate'.  Fold over all files found.  Any errors that
-- occur are ignored, with warnings printed to 'stderr'.  The fold
-- function is run from \"left\" to \"right\", so it should be strict
-- in its left argument to avoid space leaks.  If you need a
-- right-to-left fold, use 'foldr' on the result of 'findWithHandler'
-- instead.
fold :: RecursionPredicate
     -> (a -> FileInfo -> a)
     -> a
     -> FilePath
     -> IO a

fold = foldWithHandler warnOnError
    where warnOnError path a err =
              hPutStrLn stderr (path ++ ": " ++ show err) >> return a

-- | Unconditionally return 'True'.
always :: FindClause Bool
always = return True

-- | Return the file name extension.
--
-- Example:
--
-- @
-- 'extension' \"foo\/bar.txt\" => \".txt\"
-- @
extension :: FindClause FilePath
extension = takeExtension `liftM` filePath

-- | Return the file name, without the directory name.
--
-- What this means in practice:
--
-- @
-- 'fileName' \"foo\/bar.txt\" => \"bar.txt\"
-- @
--
-- Example:
--
-- @
-- 'fileName' '==?' \"init.c\"
-- @
fileName :: FindClause FilePath
fileName = takeFileName `liftM` filePath

-- | Return the directory name, without the file name.
--
-- What this means in practice:
--
-- @
-- 'directory' \"foo\/bar.txt\" => \"foo\"
-- @
--
-- Example in a clause:
--
-- @
-- let hasSuffix = 'liftOp' 'isSuffixOf'
-- in directory \`hasSuffix\` \"tests\"
-- @
directory :: FindClause FilePath
directory = takeDirectory `liftM` filePath

-- | Return the canonical path of the file being visited.
--
-- See `canonicalizePath` for details of what canonical path means.
canonicalPath :: FindClause FilePath
canonicalPath = (unsafePerformIO . canonicalizePath) `liftM` filePath

-- | Return the canonical name of the file (canonical path with the
-- directory part removed).
canonicalName :: FindClause FilePath
canonicalName = takeFileName `liftM` canonicalPath

-- | Run the given action in the 'IO' monad (using 'unsafePerformIO')
-- if the current file is a symlink.  Hide errors by wrapping results
-- in the 'Maybe' monad.
withLink :: (FilePath -> IO a) -> FindClause (Maybe a)

withLink f = do
    path <- filePath
    st <- fileStatus
    return $ if F.isSymbolicLink st
        then unsafePerformIO $ handle (\(_::IOException) -> return Nothing) $
             Just `liftM` f path
        else Nothing

-- | If the current file is a symbolic link, return 'Just' the target
-- of the link, otherwise 'Nothing'.
readLink :: FindClause (Maybe FilePath)

readLink = withLink F.readSymbolicLink

-- | If the current file is a symbolic link, return 'Just' the status
-- of the ultimate endpoint of the link.  Otherwise (including in the
-- case of an error), return 'Nothing'.
--
-- Example:
--
-- @
-- 'statusType' \`liftM\` 'followStatus' '==?' 'RegularFile'
-- @
followStatus :: FindClause (Maybe F.FileStatus)

followStatus = withLink F.getFileStatus

data FileType = BlockDevice
              | CharacterDevice
              | NamedPipe
              | RegularFile
              | Directory
              | SymbolicLink
              | Socket
              | Unknown
                deriving (Eq, Ord, Show)

-- | Return the type of file currently being visited.
--
-- Example:
--
-- @
-- 'fileType' '==?' 'RegularFile'
-- @
fileType :: FindClause FileType

fileType = statusType `liftM` fileStatus

-- | Return the type of a file.  This is much more useful for case
-- analysis than the usual functions on 'F.FileStatus' values.
statusType :: F.FileStatus -> FileType

statusType st | F.isBlockDevice st = BlockDevice
statusType st | F.isCharacterDevice st = CharacterDevice
statusType st | F.isNamedPipe st = NamedPipe
statusType st | F.isRegularFile st = RegularFile
statusType st | F.isDirectory st = Directory
statusType st | F.isSymbolicLink st = SymbolicLink
statusType st | F.isSocket st = Socket
statusType _ = Unknown

-- $statusFunctions
--
-- These are simply lifted versions of the 'F.FileStatus' accessor
-- functions in the "System.Posix.Files" module.  The definitions all
-- have the following form:
--
-- @
-- 'deviceID' :: 'FindClause' "System.Posix.Types".DeviceID
-- 'deviceID' = "System.Posix.Files".deviceID \`liftM\` 'fileStatus'
-- @

deviceID :: FindClause T.DeviceID
deviceID = F.deviceID `liftM` fileStatus

fileID :: FindClause T.FileID
fileID = F.fileID `liftM` fileStatus

fileOwner :: FindClause T.UserID
fileOwner = F.fileOwner `liftM` fileStatus

fileGroup :: FindClause T.GroupID
fileGroup = F.fileGroup `liftM` fileStatus

fileSize :: FindClause T.FileOffset
fileSize = F.fileSize `liftM` fileStatus

linkCount :: FindClause T.LinkCount
linkCount = F.linkCount `liftM` fileStatus

specialDeviceID :: FindClause T.DeviceID
specialDeviceID = F.specialDeviceID `liftM` fileStatus

fileMode :: FindClause T.FileMode
fileMode = F.fileMode `liftM` fileStatus

-- | Return the permission bits of the 'T.FileMode'.
filePerms :: FindClause T.FileMode
filePerms = (.&. 0777) `liftM` fileMode

-- | Return 'True' if any of the given permission bits is set.
--
-- Example:
--
-- @
-- 'anyPerms' 0444
-- @
anyPerms :: T.FileMode
         -> FindClause Bool
anyPerms m = filePerms >>= \p -> return (p .&. m /= 0)

accessTime :: FindClause T.EpochTime
accessTime = F.accessTime `liftM` fileStatus

modificationTime :: FindClause T.EpochTime
modificationTime = F.modificationTime `liftM` fileStatus

statusChangeTime :: FindClause T.EpochTime
statusChangeTime = F.statusChangeTime `liftM` fileStatus

-- | Return 'True' if the given path exists, relative to the current
-- file.  For example, if @\"foo\"@ is being visited, and you call
-- contains @\"bar\"@, this combinator will return 'True' if
-- @\"foo\/bar\"@ exists.
contains :: FilePath -> FindClause Bool
contains p = do
    d <- filePath
    return $ unsafePerformIO $
        handle (\(_::IOException) -> return False) $
            F.getFileStatus (d </> p) >> return True

-- | Lift a binary operator into the 'FindClause' monad, so that it
-- becomes a combinator.  The left hand side of the combinator should
-- be a @'FindClause' a@, while the right remains a normal value of
-- type @a@.
liftOp :: Monad m => (a -> b -> c) -> m a -> b -> m c

liftOp f a b = a >>= \a' -> return (f a' b)

-- $binaryOperators
--
-- These are lifted versions of the most commonly used binary
-- operators.  They have the same fixities and associativities as
-- their unlifted counterparts.  They are lifted using 'liftOp', like
-- so:
--
-- @('==?') = 'liftOp' (==)@

-- | Return 'True' if the current file's name matches the given
-- 'GlobPattern'.
(~~?) :: FindClause FilePath -> GlobPattern -> FindClause Bool
(~~?) = liftOp (~~)
infix 4 ~~?

-- | Return 'True' if the current file's name does not match the given
-- 'GlobPattern'.
(/~?) :: FindClause FilePath -> GlobPattern -> FindClause Bool
(/~?) = liftOp (/~)
infix 4 /~?

(==?) :: Eq a => FindClause a -> a -> FindClause Bool
(==?) = liftOp (==)
infix 4 ==?

(/=?) :: Eq a => FindClause a -> a -> FindClause Bool
(/=?) = liftOp (/=)
infix 4 /=?

(>?) :: Ord a => FindClause a -> a -> FindClause Bool
(>?) = liftOp (>)
infix 4 >?

(<?) :: Ord a => FindClause a -> a -> FindClause Bool
(<?) = liftOp (<)
infix 4 <?

(>=?) :: Ord a => FindClause a -> a -> FindClause Bool
(>=?) = liftOp (>=)
infix 4 >=?

(<=?) :: Ord a => FindClause a -> a -> FindClause Bool
(<=?) = liftOp (<=)
infix 4 <=?

-- | This operator is useful to check if bits are set in a
-- 'T.FileMode'.
(.&.?) :: Bits a => FindClause a -> a -> FindClause a
(.&.?) = liftOp (.&.)
infixl 7 .&.?

(&&?) :: FindClause Bool -> FindClause Bool -> FindClause Bool
(&&?) = liftM2 (&&)
infixr 3 &&?

(||?) :: FindClause Bool -> FindClause Bool -> FindClause Bool
(||?) = liftM2 (||)
infixr 2 ||?
