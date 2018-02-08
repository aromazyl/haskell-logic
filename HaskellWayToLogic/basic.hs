#! /usr/bin/env runhugs +l
--
-- basic.hs
-- Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--

import Prelude hiding (Bool, True, False, not, (&&), (==), (||), (\=), and, or, every, any, all)

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

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

valid1 :: (Bool -> Bool) -> Bool
valid1 bs = (bs True) && (bs False)

exclude_middle :: Bool -> Bool
exclude_middle p = p || (not p)

valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bs = (valid1 (bs True)) && (valid1 (bs False))

logicEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logicEquiv1 a b = and ([True, False] >>= \x -> [(a x) <=> (b x)])

makeLogic logicEqv = \a b -> and ([True, False] >>= \x -> [logicEqv (a x) (b x)])

logicEquiv2 = makeLogic logicEquiv1
logicEquiv3 = makeLogic logicEquiv2

all p = and . map p
any p = or . map p
every = flip all
some = flip any
