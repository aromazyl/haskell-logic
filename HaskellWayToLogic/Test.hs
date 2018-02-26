#! /usr/bin/env runhugs +l
--
-- Test.hs
-- Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--

module Test where
import Logic

p = Literal "P"
q = Literal "Q"

premise = Contradiction (p `Deduction` (Contradiction (q `Conjunction` (Deduction $ (Contradiction p) q))))

main :: IO ()
main = do
  printLn $ display premise
  return ()
