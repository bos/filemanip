{-# OPTIONS_GHC -fglasgow-exts #-}

module System.FilePath.Find (
      FileType(..)
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

    , linkTarget

    , (~?)
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

import qualified Control.Exception as E
import Control.Monad (foldM, forM, liftM, liftM2)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State (State(..), evalState)
import Data.Bits (Bits, (.&.))
import Data.List (sort)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), takeDirectory, takeExtension, takeFileName)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import System.IO (hPutStrLn, stderr)
import qualified System.Posix.Files as F
import qualified System.Posix.Types as T
import qualified System.FilePath.Glob as G

type Info = (FilePath, Int, F.FileStatus)

newtype FindClause a = FI { runFI :: State Info a }
    deriving (Functor, Monad, MonadFix)

evalFI :: FindClause a
       -> FilePath
       -> Int
       -> F.FileStatus
       -> a

evalFI m p d s = evalState (runFI m) (p, d, s)

mkFI = FI . State

filePath :: FindClause FilePath

filePath = mkFI $ \st@(p, _, _) -> (p, st)

fileStatus :: FindClause F.FileStatus

fileStatus = mkFI $ \st@(_, _, s) -> (s, st)

depth :: FindClause Int

depth = mkFI $ \st@(_, d, _) -> (d, st)

type FilterPredicate = FindClause Bool
type RecursionPredicate = FindClause Bool

getDirContents :: FilePath -> IO [FilePath]

getDirContents dir = (sort . filter goodName) `liftM` getDirectoryContents dir
    where goodName "." = False
          goodName ".." = False
          goodName _ = True

findWithHandler :: (FilePath -> E.Exception -> IO [FilePath])
                -> RecursionPredicate
                -> FilterPredicate
                -> FilePath
                -> IO [FilePath]

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

find :: RecursionPredicate
     -> FilterPredicate
     -> FilePath
     -> IO [FilePath]

find = findWithHandler warnOnError
    where warnOnError path err =
              hPutStrLn stderr (path ++ ": " ++ show err) >> return []

foldWithHandler :: (FilePath -> a -> E.Exception -> IO a)
                -> RecursionPredicate
                -> (a -> FindClause a)
                -> a
                -> FilePath
                -> IO a

foldWithHandler errHandler recurse f state path =
    E.handle (errHandler path state) $
        F.getSymbolicLinkStatus path >>= visit state path 0
  where visit state path depth st =
            if F.isDirectory st && evalFI recurse path depth st
            then traverse state path (succ depth) st
            else return (evalFI (f state) path depth st)
        traverse state dir depth dirSt = E.handle (errHandler dir state) $
            getDirContents dir >>=
                flip foldM (evalFI (f state) dir depth dirSt) (\state name ->
                    E.handle (errHandler dir state) $
                    let path = dir </> name
                    in F.getSymbolicLinkStatus path >>= visit state path depth)

fold :: RecursionPredicate
     -> (a -> FindClause a)
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

linkTarget :: FindClause (Maybe FilePath)
linkTarget = do
    path <- filePath
    st <- fileStatus
    return $ if F.isSymbolicLink st
      then unsafePerformIO $ E.handle (const (return Nothing))
             (Just `liftM` F.readSymbolicLink path)
      else Nothing

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

liftOp :: Monad m => (a -> b -> c) -> m a -> b -> m c

liftOp f a b = a >>= \a' -> return (f a' b)

(~?) :: FindClause FilePath
     -> G.GlobPattern
     -> FindClause Bool

(~?) = liftOp G.match

infix 4 ~?

(/~?) :: FindClause FilePath
      -> G.GlobPattern
      -> FindClause Bool
(/~?) = liftOp (\s p -> not (s `G.match` p))
      
infix 4 /~?

(==?) :: Eq a => FindClause a
      -> a
      -> FindClause Bool

(==?) = liftOp (==)

infix 4 ==?

(/=?) :: Eq a => FindClause a
      -> a
      -> FindClause Bool

(/=?) = liftOp (/=)

infix 4 /=?

(>?) :: Ord a => FindClause a
     -> a
     -> FindClause Bool

(>?) = liftOp (>)

infix 4 >?

(<?) :: Ord a => FindClause a
     -> a
     -> FindClause Bool

(<?) = liftOp (<)

infix 4 <?

(.&.?) :: Bits a => FindClause a
       -> a
       -> FindClause a

(.&.?) = liftOp (.&.)

infixl 7 .&.?

(>=?) :: Ord a => FindClause a
      -> a
      -> FindClause Bool

(>=?) = liftOp (>=)

infix 4 >=?

(<=?) :: Ord a => FindClause a
      -> a
      -> FindClause Bool

(<=?) = liftOp (<=)

infix 4 <=?

(&&?) :: FindClause Bool
      -> FindClause Bool
      -> FindClause Bool

(&&?) = liftM2 (&&)

infixr 3 &&?

(||?) :: FindClause Bool
      -> FindClause Bool
      -> FindClause Bool

(||?) = liftM2 (||)

infixr 2 ||?
