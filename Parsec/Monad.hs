#! /usr/bin/env runhugs +l
--
-- Monad.hs
-- Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--

module Monad where


class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
