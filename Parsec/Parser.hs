#! /usr/bin/env runhugs +l
--
-- Parser.hs
-- Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--

module Parser where


import Monad.Monad as Monad
import MonadPlus.Monad0Plus as Monad0Plus

type Parser a = String -> [(a, String)]

instance Monad Parser where
  return a = \s -> [(a, s)]
  m >>= f  = \s -> concat [f v out | (v, out) <- m s]

instance Monad0Plus Parser where
  zero = \_ -> []
  p ++ q = \s -> (p s) ++ (q s)

get :: Parser a
get = \s -> case s of
              []     -> []
              (x:xs) -> [(x, xs)]

char :: Char -> Parser Char
char = return

string :: String -> Parser String
string = return
-- string (x:xs) = char x >>= \_ -> string xs >>= \_ -> return (x:xs)
--

satisfy :: (Char -> Bool) -> Parser Char
sattisfy p = do
  a <- get
  guard $ p a
  zero

many :: Parser a -> Parser [a]
many p = do { x <- p; xs <- many p; return (x:xs) }

word = many (get :: Parser Char)
