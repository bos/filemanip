{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

-- |
-- Module:      System.FilePath.Manip
-- Copyright:   Sergei Trofimovich
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   unstable
-- Portability: Unix-like systems (requires flexible instances)

module System.FilePath.Error
    (
      bracket
    , bracket_
    , catch
    , handle
    , throwIO
    , Exception
    ) where

import qualified Control.Exception.Extensible as EE
import Prelude hiding (catch)

-- we can catch any exceptions if we need to
-- type Exception = SomeException
type Exception = EE.IOException

-- we just pin down 'EE.Exception e' to local Exception
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket  = EE.bracket

bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ = EE.bracket_

catch :: IO a -> (Exception -> IO a) -> IO a
catch  = EE.catch

handle :: (Exception -> IO a) -> IO a -> IO a
handle = EE.handle

throwIO :: (EE.Exception e) => e -> IO a
throwIO  = EE.throwIO
