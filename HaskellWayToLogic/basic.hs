#! /usr/bin/env runhugs +l
--
-- basic.hs
-- Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--

import Prelude hiding (Bool, True, False, not, (&&), (==), (||), (\=))

data Bool = True | False deriving (Show)

not :: Bool -> Bool
not True = False
not _ = True

(&&) :: Bool -> Bool -> Bool
(&&) True True = True
(&&) _ _ = False

(||) :: Bool -> Bool -> Bool
(||) a b = not ((not a) && (not b))

(==) :: Bool -> Bool -> Bool
(==) a b = not (a \= b)

(==>) :: Bool -> Bool -> Bool
True ==> x = x
False ==> x = True
--
(\=) :: Bool -> Bool -> Bool
True \= True = False
False \= False = False
_ \= _ = True

(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

(<+>) :: Bool -> Bool -> Bool
x <+> y = x \= y

valid1 :: (Bool -> Bool) -> Bool
valid1 bs = (bs True) && (bs False)

exclude_middle :: Bool -> Bool
exclude_middle p = p || (not p)
