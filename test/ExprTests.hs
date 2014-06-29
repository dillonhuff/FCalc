module ExprTests() where

import Expr
import RPN
import TestUtils

allExprTests = do
  toRPNTests
  
toRPNTests =
  testFunction toRPN toRPNCases
  
toRPNCases =
  [(num 12, [push 12]),
   (add (num 3) (Expr.div (num (-12)) (num 4)), [push 3, push (-12), push 4, op "/", op "+"])]