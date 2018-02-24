#! /usr/bin/env runhugs +l
--
-- logic.hs
-- Copyright (C) 2018 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--

module Logic where

import Control.Monad.Except

-- data Atom = Atom Char | Atom String deriving (Eq, Show)

data Atom = Atom String | T0 | T1 deriving (Eq)

instance Show Atom where
  show (Atom a) = show a
  show T0 = "False"
  show T1 = "True"

data Expr = Literal Atom
          | Contradiction Expr
          | Conjunction Expr Expr
          | Disjunction Expr Expr
          | Deduction Expr Expr

instance Show Expr where
  show = display

display :: Expr -> String
display (Literal a) = show a
display (Contradiction exp) = "not (" ++ display exp ++ ")"
display (Disjunction exp1 exp2) = "(" ++ display exp1 ++ ") v (" ++ display exp2 ++ ")"
display (Conjunction exp1 exp2) = "(" ++ display exp1 ++ ") ^ (" ++ display exp2 ++ ")"
display (Deduction exp1 exp2) = "(" ++ display exp1 ++ ") -> (" ++ display exp2 ++ ")"

implTree :: Expr -> Expr
implTree expr = case expr of
                  Literal a -> Literal a
                  Contradiction a -> Contradiction (implTree a)
                  Conjunction a b -> Conjunction (implTree a) (implTree b)
                  Disjunction a b -> Disjunction (implTree a) (implTree b)
                  Deduction exp1 exp2 -> Disjunction (Contradiction $ implTree exp1) (Contradiction $ implTree exp2)


distr :: Expr -> Expr -> Expr
distr exp1 exp2 = case (exp1, exp2) of
                    (Conjunction eta1 eta2, _) -> Conjunction (distr eta1 exp2) (distr eta2 exp2)
                    (_, Conjunction eta1 eta2) -> Conjunction (distr exp1 eta1) (distr exp1 eta2)
                    (_,_)                      -> Disjunction exp1 exp2


makeNNF :: Expr -> Expr
makeNNF expr = case expr of
                 Contradiction (Contradiction x) -> makeNNF x
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
