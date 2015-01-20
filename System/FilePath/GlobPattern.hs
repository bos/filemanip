-- |
-- Module:      System.FilePath.GlobPattern
-- Copyright:   Bryan O'Sullivan
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   unstable
-- Portability: everywhere
module System.FilePath.GlobPattern (
    -- * Glob patterns
    -- $syntax
      GlobPattern
    -- * Matching functions
    , (~~)
    , (/~)
    ) where

import Control.Arrow (second)
import Control.Monad (msum)
import Data.Ix (Ix, inRange)
import Data.List (nub)
import Data.Maybe (isJust)
import System.FilePath (pathSeparator)

-- $syntax
--
-- Basic glob pattern syntax is the same as for the Unix shell
-- environment.
-- 
-- * @*@ matches everything up to a directory separator or end of
-- string.
--
-- * @[/range/]@ matches any character in /range/.
-- 
-- * @[!/range/]@ matches any character /not/ in /range/.
-- 
-- There are three extensions to the traditional glob syntax, taken
-- from modern Unix shells.
--
-- * @\\@ escapes a character that might otherwise have special
-- meaning.  For a literal @\"\\\"@ character, use @\"\\\\\"@.
-- 
-- * @**@ matches everything, including a directory separator.
-- 
-- * @(/s1/|/s2/|/.../)@ matches any of the strings /s1/, /s2/, etc.

-- | Glob pattern type.
type GlobPattern = String

spanClass :: Char -> String -> (String, String)

spanClass c = gs []
    where gs _ [] = error "unterminated character class"
          gs acc (d:ds) | d == c = (reverse acc, ds)
                        | d == '\\' = case ds of
                                     (e:es) -> gs (e:'\\':acc) es
                                     _ -> error "unterminated escape"
                        | otherwise = gs (d:acc) ds

data Ix a => SRange a = SRange [a] [(a, a)]
                      deriving (Show)

inSRange :: Ix a => a -> SRange a -> Bool

inSRange c (SRange d s) = c `elem` d || any (flip inRange c) s

type CharClass = SRange Char

makeClass :: String -> CharClass

makeClass = makeClass' [] []
    where makeClass' :: [(Char, Char)] -> [Char] -> String -> CharClass
          makeClass' dense sparse [] = SRange sparse dense
          makeClass' dense sparse (a:'-':b:cs) =
              makeClass' ((a,b):dense) sparse cs
          makeClass' dense sparse (c:cs) = makeClass' dense (c:sparse) cs

data MatchTerm = MatchLiteral String
               | MatchAny
               | MatchDir
               | MatchChar
               | MatchClass Bool CharClass
               | MatchGroup [String]
                 deriving (Show)

parseGlob :: GlobPattern -> [MatchTerm]
             
parseGlob [] = []
parseGlob ('*':'*':cs) = MatchAny : parseGlob cs
parseGlob ('*':cs) = MatchDir : parseGlob cs
parseGlob ('?':cs) = MatchChar : parseGlob cs
parseGlob ('[':cs) = let (cc, ccs) = spanClass ']' cs
                         cls = case cc of
                               ('!':ccs') -> MatchClass False $ makeClass ccs'
                               _ -> MatchClass True $ makeClass cc
                     in cls : parseGlob ccs
parseGlob ('(':cs) = let (gg, ggs) = spanClass ')' cs
                     in MatchGroup (breakGroup [] gg) : parseGlob ggs
    where breakGroup :: String -> String -> [String]
          breakGroup acc [] = [reverse acc]
          breakGroup _ ['\\'] = error "group: unterminated escape"
          breakGroup acc ('\\':c:cs') = breakGroup (c:acc) cs'
          breakGroup acc ('|':cs') = reverse acc : breakGroup [] cs'
          breakGroup acc (c:cs') = breakGroup (c:acc) cs'
parseGlob ['\\'] = error "glob: unterminated escape"
parseGlob ('\\':c:cs) = MatchLiteral [c] : parseGlob cs
parseGlob (c:cs) = MatchLiteral [c] : parseGlob cs

simplifyTerms :: [MatchTerm] -> [MatchTerm]
simplifyTerms [] = []
simplifyTerms (MatchLiteral []:as) = simplifyTerms as
simplifyTerms (m@(MatchLiteral a):as) =
    case simplifyTerms as of
    (MatchLiteral b:bs) -> MatchLiteral (a ++ b) : bs
    bs -> m : bs
simplifyTerms (MatchClass True (SRange [] []):as) = simplifyTerms as
simplifyTerms (MatchClass True (SRange a@[_] []):as) =
    simplifyTerms $ MatchLiteral a : as
simplifyTerms (MatchGroup []:as) = simplifyTerms as
simplifyTerms (MatchGroup gs:as) =
    case commonPrefix gs of
    (p ,[]) -> simplifyTerms (MatchLiteral p : as)
    ("",ss) -> MatchGroup ss : simplifyTerms as
    (p ,ss) -> simplifyTerms (MatchLiteral p : MatchGroup ss : as)
simplifyTerms (a:as) = a:simplifyTerms as

commonPrefix :: [String] -> (String, [String])
commonPrefix = second nub . pfx ""
    where pfx _ [] = ("", [])
          pfx acc ss | any null ss = (reverse acc, ss)
                     | otherwise = let hs = map head ss
                                       h = head hs
                                   in if all (h==) $ tail hs
                                      then pfx (h:acc) $ map tail ss
                                      else (reverse acc, ss)

matchTerms :: [MatchTerm] -> String -> Maybe ()

matchTerms [] [] = return ()
matchTerms [] _ = fail "residual string"
matchTerms (MatchLiteral m:ts) cs = matchLiteral m cs >>= matchTerms ts
    where matchLiteral (a:as) (b:bs) | a == b = matchLiteral as bs
          matchLiteral [] as = return as
          matchLiteral _ _ = fail "not a prefix"
matchTerms (MatchClass k c:ts) cs = matchClass cs >>= matchTerms ts
    where matchClass (b:bs) | (inClass && k) || not (inClass || k) = return bs
                            where inClass = b `inSRange` c
          matchClass _ = fail "no match"
matchTerms (MatchGroup g:ts) cs = msum (map matchGroup g)
    where matchGroup g = matchTerms (MatchLiteral g : ts) cs
matchTerms [MatchAny] _ = return ()
matchTerms (MatchAny:ts) cs = matchAny cs >>= matchTerms ts
    where matchAny [] = fail "no match"
          matchAny cs' = case matchTerms ts cs' of
                          Nothing -> matchAny (tail cs')
                          _ -> return cs'
matchTerms [MatchDir] cs | pathSeparator `elem` cs = fail "path separator"
                         | otherwise = return ()
matchTerms (MatchDir:ts) cs = matchDir cs >>= matchTerms ts
    where matchDir [] = fail "no match"
          matchDir (c:_) | c == pathSeparator = fail "path separator"
          matchDir cs' = case matchTerms ts cs' of
                         Nothing -> matchDir $ tail cs'
                         _ -> return cs'
matchTerms (MatchChar:_) [] = fail "end of input"
matchTerms (MatchChar:ts) (_:cs) = matchTerms ts cs

-- | Match a file name against a glob pattern.
(~~) :: FilePath -> GlobPattern -> Bool

name ~~ pat = let terms = simplifyTerms (parseGlob pat)
              in (isJust . matchTerms terms) name

-- | Match a file name against a glob pattern, but return 'True' if
-- the match /fail/s.
(/~) :: FilePath -> GlobPattern -> Bool

(/~) = (not . ) . (~~)
