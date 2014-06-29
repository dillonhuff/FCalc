module RPN(
  RPN, push, op) where

data RPN
     = Op String
     | Push Float
       deriving (Eq, Ord, Show)
       
push = Push
op = Op

