{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module LLVMConverter (makeASTModule) where

import AST
import Data.Text.Lazy.IO as T

import LLVM.Pretty  -- from the llvm-hs-pretty package
import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Operand as O
import qualified LLVM.AST.IntegerPredicate as I

import Data.String
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

import qualified LLVM.IRBuilder.Constant as CB
import qualified LLVM.Context as Ctx
import qualified LLVM.Module as Module
import qualified LLVM.Target as Target

convertVariable :: MonadModuleBuilder m => Value -> Expression -> IRBuilderT m Operand
convertVariable (Var n _) expr = do
    res <- convertExpression expr
    case res of
        (ConstantOperand cons) -> global (fromString n) (FloatingPointType DoubleFP) cons
        _ -> global (fromString n) (FloatingPointType DoubleFP) $ (C.Float $ F.Double 0.0)
convertVariable _ expr = global (fromString n) (FloatingPointType DoubleFP) $ (C.Float $ F.Double 0.0)

convertValue :: MonadModuleBuilder m => Value -> IRBuilderT m Operand
convertValue (Nbr n) = CB.double $ fromIntegral n
convertValue (RealNbr n) = CB.double n
convertValue (Var n FloatingVar) = return $ ConstantOperand $ C.GlobalReference (FloatingPointType DoubleFP) (fromString n)
convertValue (Var n IntegerVar) = return $ ConstantOperand $ C.GlobalReference (FloatingPointType DoubleFP) (fromString n)
--convertValue (GlobVar n) = do
--    return $ LocalReference (FloatingPointType DoubleFP) $ fromString n
--convertValue (Var n IntegerVar) = LocalReference (IntegerType 32) $ fromString n


convertExpression :: MonadModuleBuilder m => Expression -> IRBuilderT m Operand
convertExpression (Un (Unary [] val)) = convertValue val
convertExpression (Expr (Unary [] val) AST.Add expr) = do
    leftOp <- convertValue val
    rightOp <- convertExpression expr
    fadd leftOp rightOp
convertExpression (Expr (Unary [] val) AST.Sub expr) = do
    leftOp <- convertValue val
    rightOp <- convertExpression expr
    fsub leftOp rightOp
convertExpression (Expr (Unary [] val) AST.Mul expr) = do
    leftOp <- convertValue val
    rightOp <- convertExpression expr
    fmul leftOp rightOp
convertExpression (Expr (Unary [] val) AST.Div expr) = do
    leftOp <- convertValue val
    rightOp <- convertExpression expr
    fdiv leftOp rightOp
convertExpression (Expr (Unary [] val) AST.Asg expr) = do
    convertVariable val expr

makeASTModule :: String -> [Expression] -> Module
makeASTModule name [] = buildModule (fromString name) $ do
  function "main" [] i32 $ \_ -> do
    value <- CB.int32 $ 0
    ret $ value
makeASTModule name exprs = buildModule (fromString name) $ do
  function "main" [] i32 $ \_ -> do
    operands <- traverse convertExpression exprs
    ret $ last operands