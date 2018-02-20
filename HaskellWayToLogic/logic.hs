#! /usr/bin/env runhugs +l
--
-- logic.hs
-- Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--

module logic where


data Atom = Atom Char | Atom String deriving (Eq, Show)

data Expr = Expr Atom
          | Contradiction Expr
          | Conjunction Expr Expr
          | Disjunction Expr Expr
          | Deduction Expr Expr

instance Show Atom where
  show Atom a = show a

newtype 
