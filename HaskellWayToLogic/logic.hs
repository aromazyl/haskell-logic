#! /usr/bin/env runhugs +l
--
-- logic.hs
-- Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--

-- module logic where

import Control.Monad.Error

-- data Atom = Atom Char | Atom String deriving (Eq, Show)

data Expr = Literal String
          | Contradiction Expr
          | Conjunction Expr Expr
          | Disjunction Expr Expr
          | Deduction Expr Expr
          deriving (Show)

display :: Expr -> String
display (Literal a) = a
display (Contradiction exp) = "not (" ++ display exp ++ ")"
display (Disjunction exp1 exp2) = "(" ++ display exp1 ++ ") or (" ++ display exp2 ++ ")"
display (Conjunction exp1 exp2) = "(" ++ display exp1 ++ ") and (" ++ display exp2 ++ ")"
display (Deduction exp1 exp2) = display exp1 ++ " -> " ++ display exp2

implTree :: Expr -> Expr
implTree expr = case expr of
                  Deduction exp1 exp2 -> Conjunction (Contradiction exp1) exp2
                  x -> x

distr :: Expr -> Expr -> Expr
distr exp1 exp2 = case (exp1, exp2) of
                    (Conjunction eta1 eta2, _) -> Conjunction (distr eta1 exp2) (distr eta2 exp2)
                    (_, Conjunction eta1 eta2) -> Conjunction (distr exp1 eta1) (distr exp1 eta2)
                    (_,_)                      -> Disjunction exp1 exp2


makeNNF :: Expr -> Expr
makeNNF expr = case expr of
                 Contradiction (Contradiction x) -> x
                 Conjunction exp1 exp2 -> Conjunction (makeNNF exp1) (makeNNF exp2)
                 Disjunction exp1 exp2 -> Disjunction (makeNNF exp1) (makeNNF exp2)
                 Contradiction (Conjunction exp1 exp2) ->
                   Disjunction (makeNNF $ Contradiction exp1) (makeNNF $ Contradiction exp2)
                 Contradiction (Disjunction exp1 exp2) ->
                   Conjunction (makeNNF $ Contradiction exp1) (makeNNF $ Contradiction exp2)
                 otherwise -> otherwise


makeCNF :: Expr -> Expr
makeCNF expr = case expr of
                 Conjunction exp1 exp2 -> Conjunction (makeCNF exp1) (makeCNF exp2)
                 Disjunction exp1 exp2 -> distr (makeCNF exp1) (makeCNF exp2)
                 otherwise -> otherwise
