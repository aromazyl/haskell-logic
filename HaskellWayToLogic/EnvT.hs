#! /usr/bin/env runhugs +l
--
-- EnvT.hs
-- Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module EnvT where

import Prelude hiding ((>>=), Monad, pure)
import Monad

newtype EnvT s m a = EnvT { runEnv :: s -> m a}

instance (Monad m) => Monad (EnvT s m) where
  pure a = EnvT $ \r -> pure a
  (>>=) m f = EnvT $ \r -> (runEnv m) r >>= \x -> (runEnv (f x)) r

instance MonadTrans (EnvT s) where
  lift :: (Monad m) => m a -> EnvT s m a
  lift m = EnvT $ \r -> m

instance (Monad m) => EnvMonad r (EnvT r m) where
  inEnv :: r -> EnvT r m a -> EnvT r m a
  inEnv r env = EnvT $ \_ -> (runEnv env) r
  rdEnv ::  EnvT r m r
  rdEnv = EnvT $ \r -> pure r
