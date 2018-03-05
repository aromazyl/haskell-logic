#! /usr/bin/env runhugs +l
--
-- StateT.hs
-- Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--
--
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module StateT where

import Prelude hiding ((>>=), Monad, pure)
import Monad

newtype StateT s m a = StateT { runState :: s -> m (a, s) }

instance (Monad m) => Monad (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (>>=) m f = StateT $ \s -> (runState m) s >>= \(a, s') -> (runState (f a)) s'

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> m >>= \x -> pure (x, s)


instance (Monad m) => StateMonad s (StateT s m) where
  update f = StateT $ \s -> pure (f s, s)
