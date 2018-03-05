#! /usr/bin/env runhugs +l
--
-- ContT.hs
-- Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module ContT where


import Prelude hiding ((>>=), Monad, pure)
import Monad

newtype ContT ans m a = ContT { runContT :: (a -> m ans) -> m ans }

instance (Monad m) => Monad (ContT ans m) where
  pure a = ContT $ \f -> f a
  (>>=) :: ContT ans m a -> (a -> ContT ans m b) -> ContT ans m b
  (>>=) m f = ContT $ \g -> (runContT m) (\a -> (runContT $ f a) g)

instance MonadTrans (ContT ans) where
  lift :: (Monad m) => m a -> ContT ans m a
  lift m = ContT $ \g -> m >>= g

instance (Monad m) => ContMonad (ContT ans m) where
  callcc :: ((a -> ContT ans m b) -> ContT ans m a) -> ContT ans m a
  callcc f = ContT $ \k -> (runContT $ f (\a -> ContT $ \_ -> k a)) k

