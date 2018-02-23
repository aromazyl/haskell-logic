#! /usr/bin/env runhugs +l
--
-- MonadPlus.hs
-- Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--

module MonadPlus where

import Monad.Monad as Monad

class Monad m => Monad0Plus where
  zero :: m a
  (++) :: m a -> m a -> m a
