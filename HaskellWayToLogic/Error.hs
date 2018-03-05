#! /usr/bin/env runhugs +l
--
-- Error.hs
-- Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Error where

import Prelude hiding ((>>=), pure, Monad, Error, map)
import Monad

data Error a = Ok a | Error String
newtype ErrorT m a = ErrorT { runErr :: m (Error a) }

instance (Monad m) => Monad (ErrorT m) where
  pure a = ErrorT $ pure $ Ok a
  m >>= f = ErrorT $ (runErr m) >>= \x ->
    case x of
      (Ok a) -> runErr (f a)
      (Error s) -> pure (Error s)

instance MonadTrans ErrorT where
  lift :: (Monad m) => m a -> ErrorT m a
  lift m = ErrorT $ m >>= \x -> (runErr $ pure x)

instance (Monad m) => ErrMonad (ErrorT m) where
  err s = ErrorT $ pure $ Error s
