{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module LLVMConverter (makeASTModule) where

import AST
import Data.Text.Lazy.IO as T

import LLVM.Pretty  -- from the llvm-hs-pretty package
import LLVM.AST hiding (function)
import LLVM.AST.Type as ASTL
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Operand as O
import qualified LLVM.AST.IntegerPredicate as I
import qualified LLVM.AST.ParameterAttribute as PA

import Data.String
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

import qualified LLVM.IRBuilder.Constant as CB
import qualified LLVM.Context as Ctx
import qualified LLVM.Module as Module
import qualified LLVM.Target as Target

floatType = FloatingPointType DoubleFP

getParamsValueInLLVM :: MonadModuleBuilder m => [Expression] -> IRBuilderT m [(Operand, [PA.ParameterAttribute])]
getParamsValueInLLVM list = sequence $ fmap (\s -> fmap (\f -> (f, [])) s) $ fmap convertExpression list
--getParamsValueInLLVM [] = return []
--getParamsValueInLLVM (c:cs) = sequence ((convertValue c), []) ++ getParamsValueInLLVM cs

getFunctionParameters :: [(String, AST.Type)] -> [(ASTL.Type, String)]
getFunctionParameters [] = []
getFunctionParameters ((n, typ):cs) = [(getASTLType typ, n)] ++ getFunctionParameters cs

getASTLType :: AST.Type -> ASTL.Type
getASTLType FloatingVar = floatType
getASTLType IntegerVar = floatType
getASTLType _ = floatType

convertFunction :: MonadModuleBuilder m => FunctionPrototype -> [Expression] -> IRBuilderT m Operand
convertFunction (Proto name args retType) exprs = do
    function (fromString name) (fmap (fmap fromString) $ getFunctionParameters args) (getASTLType retType) $ \ops -> do
         operands <- traverse convertExpression exprs
         ret $ last operands

convertVariable :: MonadModuleBuilder m => Value -> Expression -> IRBuilderT m Operand
convertVariable (Var Global n _) expr = do
    res <- convertExpression expr
    case res of
        (ConstantOperand cons) -> global (fromString n) floatType cons
        _ -> global (fromString n) floatType $ (C.Float $ F.Double 0.0)
convertVariable (Var Local n _) expr = do
    res <- convertExpression expr
    case res of
        (ConstantOperand cons) -> global (fromString n) floatType cons
        _ -> global (fromString n) floatType $ (C.Float $ F.Double 0.0)

convertValue :: MonadModuleBuilder m => Value -> IRBuilderT m Operand
convertValue (Nbr n) = CB.double $ fromIntegral n
convertValue (RealNbr n) = CB.double n
convertValue (Var _ n FloatingVar) = return $ ConstantOperand $ C.GlobalReference floatType (fromString n)
convertValue (Var _ n IntegerVar) = return $ ConstantOperand $ C.GlobalReference floatType (fromString n)
convertValue (AST.Call (Proto name params retType) args) = do
    parameters <- getParamsValueInLLVM args
    call (ConstantOperand $ C.GlobalReference (FunctionType floatType (fst $ unzip $ getFunctionParameters params) False) $ fromString name) parameters
--convertValue (GlobVar n) = do
--    return $ LocalReference (FloatingPointType DoubleFP) $ fromString n
--convertValue (Var n IntegerVar) = LocalReference (IntegerType 32) $ fromString n


convertExpression :: MonadModuleBuilder m => Expression -> IRBuilderT m Operand
convertExpression (Un (Unary [] val)) = convertValue val
convertExpression (Fct (Decl proto expr)) = convertFunction proto expr
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
  function "main" [] double $ \_ -> do
    value <- CB.int32 $ 0
    ret $ value
makeASTModule name exprs = buildModule (fromString name) $ do
  function "main" [] double $ \_ -> do
    operands <- traverse convertExpression exprs
    ret $ last operands