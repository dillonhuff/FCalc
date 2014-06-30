module RPN(
  RPN, push, op) where

import Control.Monad.State

import Data.Word

import LLVM.General.AST as AST
import LLVM.General.AST.Constant
import LLVM.General.AST.Float
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST

import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.FloatingPointPredicate as FP

data RPN
     = Op String
     | Push Double
       deriving (Eq, Ord, Show)
       
push = Push
op = Op

-- LLVM Code generation
double :: Type
double = FloatingPointType 64 IEEE

data OpStack
     = OpStack {
       nextIdx      :: Word,
       operands     :: [Operand],
       instructions :: [Named Instruction],
       terminator   :: Maybe (Named Terminator)
       }
       deriving (Show)
       
initialStack :: OpStack
initialStack = OpStack 0 [] [] Nothing

genModule :: String -> [Definition] -> AST.Module
genModule name defs = Module name Nothing Nothing defs

genFunc :: String -> [RPN] -> Definition
genFunc n rpnCode =
  GlobalDefinition $ functionDefaults {
    name         = Name n,
    parameters   = ([], False),
    returnType   = double,
    basicBlocks  = [genBasicBlock (Name "entry") rpnCode]
    }

genBasicBlock :: Name -> [RPN] -> AST.BasicBlock
genBasicBlock name rpnCode = case term of
  Just t -> BasicBlock name (instructions opStk) t
  Nothing -> error $ "No terminator for RPN code " ++ show rpnCode
  where
    opStk = genInstrs initialStack rpnCode
    term = terminator opStk
    
genInstrs :: OpStack -> [RPN] -> OpStack
genInstrs opStk [] = setTerminator opStk
genInstrs opStk (rpnCode:rest) = genInstrs newOpStk rest
  where
    newOpStk = nextInstr rpnCode opStk

nextInstr :: RPN -> OpStack -> OpStack
nextInstr (Push f) opStk = OpStack (idx+1) nextOps instrs term
  where
    idx = nextIdx opStk
    ops = operands opStk
    instrs = instructions opStk
    term = terminator opStk
    nextOp = floatOp f
    nextOps = (nextOp:ops)
nextInstr (Op name) opStk = OpStack (idx+1) nextOps nextInstrs term
  where
    idx = nextIdx opStk
    ops = operands opStk
    instr = instructions opStk
    term = terminator opStk
    opName = UnName idx
    nextOp = localOp opName
    nextOps = (nextOp:ops)
    nextInstr = [opName := (binInstr name (head ops) (head (tail ops)))]
    nextInstrs = instr ++ nextInstr
    
setTerminator :: OpStack -> OpStack
setTerminator opStk = OpStack (nextIdx opStk) (operands opStk) (instructions opStk) newTerm
  where
    newTerm = Just $ Do $ returnOp $ head $ operands opStk
  
-- Operation Constructors
    
binInstr :: String -> Operand -> Operand -> Instruction
binInstr name op1 op2 = case name of
  "+" -> AST.FAdd NoFastMathFlags op1 op2 []
  "*" -> AST.FMul NoFastMathFlags op1 op2 []
  "-" -> AST.FMul NoFastMathFlags op1 op2 []
  "/" -> AST.FDiv NoFastMathFlags op1 op2 []
       
floatOp :: Double -> Operand
floatOp f = ConstantOperand $ Float $ Double f

-- Only type is double
localOp :: Name -> Operand
localOp n = LocalReference double n

returnOp :: Operand -> Terminator
returnOp op = Ret (Just op) []

--genLLVM :: RPN -> String
--genLLVM (Op "+") = 