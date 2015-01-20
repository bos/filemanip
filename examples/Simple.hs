import Control.Monad
import Codec.Compression.GZip
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import System.FilePath
import System.FilePath.Find
import System.FilePath.Glob
import System.FilePath.Manip
import Text.Regex.Posix ((=~))



-- Get a list of all symlinks.

getDanglingLinks :: FilePath -> IO [FilePath]

getDanglingLinks = find always (fileType ==? SymbolicLink &&?
                                followStatus ==? Nothing)



-- Rename all ".cpp" files to ".C".

renameCppToC :: FilePath -> IO ()

renameCppToC path = find always (extension ==? ".cpp") path >>=
                    mapM_ (renameWith (replaceExtension ".C"))



-- A recursion control predicate that will avoid recursing into
-- directories commonly used by revision control tools.

noRCS :: RecursionPredicate

noRCS = (`notElem` ["_darcs","SCCS","CVS",".svn",".hg",".git"]) `liftM` fileName

cSources :: FilePath -> IO [FilePath]

cSources = find noRCS (extension ==? ".c" ||? extension ==? ".h")



-- Replace all uses of "monkey" with "simian", saving the original copy
-- of the file with a ".bak" extension:

monkeyAround :: FilePath -> IO ()

monkeyAround = modifyWithBackup (<.> "bak") (unwords . map reMonkey . words)
    where reMonkey x = if x == "monkey" then "simian" else x



-- Given a simple grep, it's easy to construct a recursive grep.

grep :: (Int -> S.ByteString -> a) -> String -> S.ByteString -> [a]

grep f pat s = consider 0 (S.lines s)
    where consider _ [] = []
          consider n (l:ls) | S.null l = consider (n+1) ls
          consider n (l:ls) | l =~ pat = (f n l):ls'
                            | otherwise = ls'
              where ls' = consider (n+1) ls

grepFile :: (Int -> S.ByteString -> a) -> String -> FilePath -> IO [a]

grepFile f pat name = grep f pat `liftM` S.readFile name

recGrep :: String -> FilePath -> IO [(FilePath, Int, S.ByteString)]

recGrep pat top = find always (fileType ==? RegularFile) top >>=
                  mapM ((,,) >>= flip grepFile pat) >>=
                  return . concat


-- Decompress all gzip files matching a fixed glob pattern, and return
-- the results as a single huge lazy ByteString.

decomp :: IO L.ByteString

decomp = namesMatching "*/*.gz" >>=
         fmap L.concat . mapM (fmap decompress . L.readFile)
