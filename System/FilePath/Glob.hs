{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module:      System.FilePath.Glob
-- Copyright:   Bryan O'Sullivan
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   unstable
-- Portability: everywhere

module System.FilePath.Glob (
      namesMatching
    , unsafeInterleaveIO'
    ) where

import Control.Exception
import Control.Monad (forM)
import System.FilePath.GlobPattern ((~~))
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents, setCurrentDirectory)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import System.IO.Unsafe (unsafeInterleaveIO)

-- | A version of unsafeInterleaveIO that doesn't forget about changes
-- to the current directory.  dsf: I don't think this is a bug in
-- unsafeInterleaveIO, the doc says the IO operation performed by
-- unsafeInterleaveIO "should be free of side effects and independent
-- of its environment."  That doesn't quite square with getting the
-- "wrong" value from getCurrentDirectory, but this code certainly
-- makes things explicit.  Surprisingly, unsafePerformIO seems to see
-- the correct getCurrentDirectory.
unsafeInterleaveIO' :: IO a -> IO a
unsafeInterleaveIO' action =
    getCurrentDirectory >>= \here -> unsafeInterleaveIO (withCurrentDirectory here action)
    where
      withCurrentDirectory dir action =
          bracket getCurrentDirectory
                  setCurrentDirectory
                  (\_ -> setCurrentDirectory dir >> action)

-- | Return a list of names matching a glob pattern.  The list is
-- generated lazily.
namesMatching :: String -> IO [FilePath]
namesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return (if exists then [pat] else [])
  | otherwise = do
    case splitFileName pat of
      ("", baseName) -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName
      (dirName, baseName) -> do
          dirs <- if isPattern dirName
                  then namesMatching (dropTrailingPathSeparator dirName)
                  else return [dirName]
          let listDir = if isPattern baseName
                        then listMatches
                        else listPlain
          pathNames <- forM dirs $ \dir -> do
                           baseNames <- listDir dir baseName
                           return (map (dir </>) baseNames)
          return (concat pathNames)
  where isPattern = any (`elem` "[*?")

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    names <- unsafeInterleaveIO' (handle (\(_::IOException) -> return []) $
                                         getDirectoryContents dirName')
    let names' = if isHidden pat
                 then filter isHidden names
                 else filter (not . isHidden) names
    return (filter (~~ pat) names')
  where isHidden ('.':_) = True
        isHidden _ = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <- if null baseName
              then doesDirectoryExist dirName
              else doesNameExist (dirName </> baseName)
    return (if exists then [baseName] else [])

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
      then return True
      else doesDirectoryExist name
