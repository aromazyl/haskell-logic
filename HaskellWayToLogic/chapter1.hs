#! /usr/bin/env runhugs +l
--
-- chapter1.hs
-- Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--

divides :: Integer -> Integer -> Bool
divides d n = rem d n == 0

maximums :: [Integer] -> Integer
maximums = foldr1 max
