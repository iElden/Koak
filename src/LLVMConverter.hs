{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module LLVMConverter (makeASTModule) where

import AST
import Data.Tuple
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

-- local variables

type VarName = String
type LocalVariables = [(ASTL.Type, VarName, Maybe Operand)]

lookupParameter :: String -> LocalVariables -> Maybe (ASTL.Type, Maybe Operand)
lookupParameter _ [] = Nothing
lookupParameter n ((t, ln, op):cs) | n == ln = Just (t, op)
    | otherwise = lookupParameter n cs

--utils

concatZipNoOperand :: ([ASTL.Type], [VarName]) -> [(ASTL.Type, VarName, Maybe Operand)]
concatZipNoOperand lists = zip3 (fst lists) (snd lists) (replicate (length $ fst lists) Nothing)
--

getParamsValueInLLVM :: MonadModuleBuilder m => [Value] -> LocalVariables -> IRBuilderT m [(Operand, [PA.ParameterAttribute])]
getParamsValueInLLVM list vars = sequence $ fmap (\s -> fmap (\f -> (f, [])) s) $ fmap (\s -> convertValue s vars) list
--getParamsValueInLLVM [] = return []
--getParamsValueInLLVM (c:cs) = sequence ((convertValue c), []) ++ getParamsValueInLLVM cs

getFunctionParameters :: [(String, AST.Type)] -> [(ASTL.Type, String)]
getFunctionParameters [] = []
getFunctionParameters ((n, typ):cs) = [(getASTLType typ, n)] ++ getFunctionParameters cs

getASTLType :: AST.Type -> ASTL.Type
getASTLType FloatingVar = floatType
getASTLType IntegerVar = floatType
getASTLType _ = floatType

convertFunction :: MonadModuleBuilder m => FunctionPrototype -> [Expression] -> LocalVariables -> IRBuilderT m Operand
convertFunction (Proto name args retType) exprs vars = do
    function (fromString name) (fmap (fmap fromString) $ getFunctionParameters args) (getASTLType retType) $ \_ -> do
        let newVars = concatZipNoOperand $ unzip $ getFunctionParameters args
        operands <- traverse (\s -> convertExpression s newVars) exprs
        ret $ fst $ last operands

convertVariable :: MonadModuleBuilder m => Value -> Expression -> LocalVariables -> IRBuilderT m (Operand, LocalVariables)
convertVariable (Var n t) expr loc = do
    newOperand <- convertExpression expr loc
    return (fst newOperand, loc ++ [(getASTLType t, fromString n, Just $ fst newOperand)])

convertValue :: MonadModuleBuilder m => Value -> LocalVariables -> IRBuilderT m Operand
convertValue (Nbr n) _ = CB.double $ fromIntegral n
convertValue (RealNbr n) _ = CB.double n
convertValue (Var n FloatingVar) loc = case lookupParameter n loc of
    Just (t, Nothing) -> return $ LocalReference t (fromString n)
    Just (t, Just op) -> return op
    Nothing -> return $ ConstantOperand $ C.GlobalReference floatType (fromString n)
convertValue (Var n IntegerVar) loc = case lookupParameter n loc of
    (Just (t, Nothing)) -> return $ LocalReference t (fromString n)
    (Just (t, Just op)) -> return op
    Nothing -> return $ ConstantOperand $ C.GlobalReference floatType (fromString n)
convertValue (AST.Call (Proto name params retType) args) loc = do
    parameters <- getParamsValueInLLVM args loc
    call (ConstantOperand $ C.GlobalReference (FunctionType floatType (fst $ unzip $ getFunctionParameters params) False) $ fromString name) parameters
--convertValue (GlobVar n) = do
--    return $ LocalReference (FloatingPointType DoubleFP) $ fromString n
--convertValue (Var n IntegerVar) = LocalReference (IntegerType 32) $ fromString n


convertExpression :: MonadModuleBuilder m => Expression -> LocalVariables -> IRBuilderT m (Operand, LocalVariables)
convertExpression (Un (Unary [] val)) loc = do
    let newOp = convertValue val loc
    fmap (\s -> (s, loc)) newOp
convertExpression (Fct (Decl proto expr)) loc = do
    let newOp = convertFunction proto expr loc
    fmap (\s -> (s, loc)) newOp
convertExpression (Expr (Unary [] val) AST.Add expr) loc = do
    leftOp <- convertValue val loc
    rightOp <- convertExpression expr loc
    fmap (\s -> (s, loc)) (fadd leftOp $ fst rightOp)
convertExpression (Expr (Unary [] val) AST.Sub expr) loc = do
    leftOp <- convertValue val loc
    rightOp <- convertExpression expr loc
    fmap (\s -> (s, loc)) (fsub leftOp $ fst rightOp)
convertExpression (Expr (Unary [] val) AST.Mul expr) loc = do
    leftOp <- convertValue val loc
    rightOp <- convertExpression expr loc
    fmap (\s -> (s, loc)) (fmul leftOp $ fst rightOp)
convertExpression (Expr (Unary [] val) AST.Div expr) loc = do
    leftOp <- convertValue val loc
    rightOp <- convertExpression expr loc
    fmap (\s -> (s, loc)) (fdiv leftOp $ fst rightOp)
convertExpression (Expr (Unary [] val) AST.Asg expr) loc = do
    convertVariable val expr loc

makeASTModule :: String -> [Expression] -> Module
makeASTModule name [] = buildModule (fromString name) $ do
  function "main" [] double $ \_ -> do
    value <- CB.int32 $ 0
    ret $ value
makeASTModule name exprs = buildModule (fromString name) $ do
  function "main" [] double $ \_ -> do
    operands <- traverse (\s -> convertExpression s []) exprs
    ret $ fst $ last operands