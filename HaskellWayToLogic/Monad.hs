#! /usr/bin/env runhugs +l
--
-- Monad.hs
-- Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Monad where

import Prelude hiding ((>>=), pure, Monad, map, join)

class Monad m where
  pure  :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  map   :: (a -> b) -> m a -> m b
  join  :: m (m a) -> m a
  map f m = m >>= (pure . f)
  join z = z >>= id


class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

class (Monad m) => StateMonad s m where
  update :: (s -> s) -> m s

class (Monad m) => EnvMonad env m where
  inEnv :: env -> m a -> m a
  rdEnv :: m env

class (Monad m) => ErrMonad m where
  err :: String -> m a

class (Monad m) => ContMonad m where
  callcc :: ((a -> m b) -> m a) -> m a

class (Monad m) => ListMonad m where
  merge :: [m a] -> m a
