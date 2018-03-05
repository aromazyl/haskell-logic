#! /usr/bin/env runhugs +l
--
-- List.hs
-- Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

import Prelude hiding ((>>=), pure, Monad, map, join)
import Monad
import Data.List

instance Monad List where
  pure a = [a]
  (>>=) [] f = []
  (>>=) (x:xs) f = (f x) ++ (xs >>= f)

instance ListMonad List where
  merge :: [List a] -> List a
  merge = concat
