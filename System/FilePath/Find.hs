{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module:      System.FilePath.Find
-- Copyright:   Bryan O'Sullivan
-- License:     LGPL
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   unstable
-- Portability: Unix-like systems (requires newtype deriving)

module System.FilePath.Find (
      FileInfo(..)
    , FileType(..)
    , FindClause
    , FilterPredicate
    , RecursionPredicate

    , find
    , findWithHandler

    , fold
    , foldWithHandler

    , filePath
    , fileStatus
    , depth
    , fileInfo

    , always
    , extension
    , directory
    , fileName

    , fileType
    , deviceID
    , fileID
    , fileOwner
    , fileGroup
    , fileSize
    , linkCount
    , specialDeviceID
    , fileMode
    , filePerms
    , anyPerms
    , accessTime
    , modificationTime
    , statusChangeTime

    , readLink
    , followStatus

    , (~~?)
    , (/~?)
    , (==?)
    , (/=?)
    , (>?)
    , (<?)
    , (>=?)
    , (<=?)
    , (&&?)
    , (||?)

    , (.&.?)
    ) where

import Control.Monad (foldM, forM, liftM, liftM2)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State (State(..), evalState)
import Data.Bits (Bits, (.&.))
import Data.List (sort)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), replaceFileName, takeDirectory, takeExtension,
                        takeFileName)
import System.FilePath.GlobPattern (GlobPattern, (~~), (/~))
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import qualified Control.Exception as E
import qualified System.Posix.Files as F
import qualified System.Posix.Types as T

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
    deriving (Functor, Monad)

-- | Run the given find clause and return a pure value.
evalFI :: FindClause a
       -> FilePath
       -> Int
       -> F.FileStatus
       -> a
evalFI m p d s = evalState (runFC m) (mkFI p d s)

mkFindClause :: (FileInfo -> (a, FileInfo)) -> FindClause a

mkFindClause = FC . State

-- | Return the current 'FileInfo'.
fileInfo :: FindClause FileInfo

fileInfo = mkFindClause $ \st -> (st, st)

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
findWithHandler :: (FilePath -> E.Exception -> IO [FilePath]) -- ^ error handler
                -> RecursionPredicate -- ^ control recursion into subdirectories
                -> FilterPredicate -- ^ decide whether a file appears in the result
                -> FilePath -- ^ directory to start searching
                -> IO [FilePath] -- ^ files that matched the 'FilterPredicate'

findWithHandler errHandler recurse filter path =
    E.handle (errHandler path) $ F.getSymbolicLinkStatus path >>= visit path 0
  where visit path depth st =
            if F.isDirectory st && evalFI recurse path depth st
              then unsafeInterleaveIO (traverse path (succ depth) st)
              else filterPath path depth st []
        traverse dir depth dirSt = do
            names <- E.catch (getDirContents dir) (errHandler dir)
            filteredPaths <- forM names $ \name -> do
                let path = dir </> name
                unsafeInterleaveIO $ E.handle (errHandler path)
                    (F.getSymbolicLinkStatus path >>= visit path depth)
            filterPath dir depth dirSt (concat filteredPaths)
        filterPath path depth st result =
            return $ if evalFI filter path depth st
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
-- occur are dealt with by the given handler.  The fold function is
-- run from \"left\" to \"right\", so it should be strict in its left
-- argument to avoid space leaks.  If you need a right-to-left fold,
-- use 'foldr' on the result of 'findWithHandler' instead.
foldWithHandler :: (FilePath -> a -> E.Exception -> IO a) -- ^ error handler
                -> RecursionPredicate -- ^ control recursion into subdirectories
                -> (a -> FileInfo -> a) -- ^ function to fold with
                -> a -- ^ seed value for fold
                -> FilePath -- ^ directory to start searching
                -> IO a -- ^ final value after folding

foldWithHandler errHandler recurse f state path =
    E.handle (errHandler path state) $
        F.getSymbolicLinkStatus path >>= visit state path 0
  where visit state path depth st =
            if F.isDirectory st && evalFI recurse path depth st
            then traverse state path (succ depth) st
            else return (f state (mkFI path depth st))
        traverse state dir depth dirSt = E.handle (errHandler dir state) $
            getDirContents dir >>=
                flip foldM (f state (mkFI dir depth dirSt)) (\state name ->
                    E.handle (errHandler dir state) $
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

always :: FindClause Bool
always = return True

extension :: FindClause FilePath
extension = takeExtension `liftM` filePath

fileName :: FindClause FilePath
fileName = takeFileName `liftM` filePath

directory :: FindClause FilePath
directory = takeDirectory `liftM` filePath

withLink :: (FilePath -> IO a) -> FindClause (Maybe a)

withLink f = do
    path <- filePath
    st <- fileStatus
    return $ if F.isSymbolicLink st
        then unsafePerformIO $ E.handle (const (return Nothing)) $
             Just `liftM` f path
        else Nothing

readLink :: FindClause (Maybe FilePath)

readLink = withLink F.readSymbolicLink

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

fileType :: FindClause FileType

fileType = fType `liftM` fileStatus
    where fType st | F.isBlockDevice st = BlockDevice
          fType st | F.isCharacterDevice st = CharacterDevice
          fType st | F.isNamedPipe st = NamedPipe
          fType st | F.isRegularFile st = RegularFile
          fType st | F.isDirectory st = Directory
          fType st | F.isSymbolicLink st = SymbolicLink
          fType st | F.isSocket st = Socket
          fType _ = Unknown

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

filePerms :: FindClause T.FileMode
filePerms = (.&. 0777) `liftM` fileMode

anyPerms :: T.FileMode
         -> FindClause Bool
anyPerms m = filePerms >>= \p -> return (p .&. m /= 0)

accessTime :: FindClause T.EpochTime
accessTime = F.accessTime `liftM` fileStatus

modificationTime :: FindClause T.EpochTime
modificationTime = F.modificationTime `liftM` fileStatus

statusChangeTime :: FindClause T.EpochTime
statusChangeTime = F.statusChangeTime `liftM` fileStatus

contains :: FilePath -> FindClause Bool
contains p = do
    d <- filePath
    return $ unsafePerformIO $
        E.handle (const (return False)) $
            F.getFileStatus (d </> p) >> return True

liftOp :: Monad m => (a -> b -> c) -> m a -> b -> m c

liftOp f a b = a >>= \a' -> return (f a' b)

(~~?) :: FindClause FilePath -> GlobPattern -> FindClause Bool
(~~?) = liftOp (~~)
infix 4 ~~?

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

(.&.?) :: Bits a => FindClause a -> a -> FindClause a
(.&.?) = liftOp (.&.)
infixl 7 .&.?

(>=?) :: Ord a => FindClause a -> a -> FindClause Bool
(>=?) = liftOp (>=)
infix 4 >=?

(<=?) :: Ord a => FindClause a -> a -> FindClause Bool
(<=?) = liftOp (<=)
infix 4 <=?

(&&?) :: FindClause Bool -> FindClause Bool -> FindClause Bool
(&&?) = liftM2 (&&)
infixr 3 &&?

(||?) :: FindClause Bool -> FindClause Bool -> FindClause Bool
(||?) = liftM2 (||)
infixr 2 ||?
