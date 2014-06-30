{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Expr(
  toRPN,
  add, sub, mul, Expr.div, num) where

import RPN

data Expr
     = BinOp String Expr Expr
     | Num Float
       deriving (Eq, Ord, Show)
       
add = BinOp "+"
sub = BinOp "-"
mul = BinOp "*"
div = BinOp "/"
num = Num

toRPN :: Expr -> [RPN]
toRPN (Num f) = [push f]
toRPN (BinOp n l r) = (toRPN l) ++ (toRPN r) ++ [op n]