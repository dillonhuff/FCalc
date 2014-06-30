module EmitLLVM(
  withContextAST) where

import RPN

import Control.Monad
import Control.Monad.Trans.Error

import LLVM.General.Module
import LLVM.General.Context
--import LLVM.General.AST as AST

withContextAST mod = withContext (testErrE mod)

testErrE mod con = Control.Monad.Trans.Error.runErrorT $ withContextAndMod con mod

withContextAndMod con mod = withModuleFromAST con mod moduleLLVMAssembly






