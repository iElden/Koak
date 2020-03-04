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

-- lookups in variable
type VarName = String
type LocalVariables = [(ASTL.Type, VarName, Maybe Operand)]

lookupVariable :: MonadModuleBuilder m => String -> LocalVariables -> IRBuilderT m Operand
lookupVariable n [] = return $ ConstantOperand $ C.GlobalReference floatType (fromString n)
lookupVariable n ((t, name, op):xs)
    | n == name = case op of
        Just operand -> return operand
        Nothing -> return $ LocalReference t (fromString n)
    | otherwise = lookupVariable n xs


getParamsValueInLLVM :: MonadModuleBuilder m => [Expression] -> LocalVariables -> IRBuilderT m [(Operand, [PA.ParameterAttribute])]
getParamsValueInLLVM list vars = sequence $ fmap (\s -> fmap (\f -> (fst f, [])) s) $ fmap (\t -> convertExpression t vars) list
--getParamsValueInLLVM [] = return []
--getParamsValueInLLVM (c:cs) = sequence ((convertValue c), []) ++ getParamsValueInLLVM cs

putFPinLocalVariables :: LocalVariables -> ([ASTL.Type], [String]) -> LocalVariables
putFPinLocalVariables vars (types, names) = zip3 types names $ replicate (length names) Nothing

getFunctionParameters :: [(String, AST.Type)] -> [(ASTL.Type, String)]
getFunctionParameters [] = []
getFunctionParameters ((n, typ):cs) = [(getASTLType typ, n)] ++ getFunctionParameters cs

getASTLType :: AST.Type -> ASTL.Type
getASTLType FloatingVar = floatType
getASTLType IntegerVar = floatType
getASTLType _ = floatType

--convertNewExpression :: MonadModuleBuilder m => [Expression] -> LocalVariables -> IRBuilderT m (Operand, LocalVariables)
--convertNewExpression (x:[]) vars = convertExpression x vars
--convertNewExpression (x:xs) vars = do
--    ops <- convertExpression x vars
--    convertNewExpression xs $ snd ops

convertFunction :: MonadModuleBuilder m => FunctionPrototype -> [Expression] -> IRBuilderT m Operand
convertFunction (Proto name args retType) exprs = do
    function (fromString name) (fmap (fmap fromString) $ getFunctionParameters args) (getASTLType retType) $ \_ -> do
        operand <- convert (putFPinLocalVariables [] $ unzip $ getFunctionParameters args) exprs -- Update le tableau de vars pour qu'il soit envoyé dans convert Expression
        ret operand
            where
                convert :: MonadModuleBuilder m => LocalVariables -> [Expression] -> IRBuilderT m Operand
                convert _ [] = CB.int32 0
                convert scope (x:[]) = do
                    (op, _) <- convertExpression x scope
                    return op
                convert scope (x:xs) = do
                    (op, vars) <- convertExpression x scope
                    convert vars xs



convertVariable :: MonadModuleBuilder m => Value -> Expression -> LocalVariables -> IRBuilderT m (Operand, LocalVariables)
convertVariable (Var Global n t) expr vars = do
    (res, _) <- convertExpression expr vars
    case res of
        (ConstantOperand cons) -> fmap (\s -> (s, vars)) $ global (fromString n) floatType cons
        _ -> fmap (\s -> (s, vars)) $ global (fromString n) floatType $ (C.Float $ F.Double 0.0)
convertVariable (Var Local n t) expr vars = do
    (res, _) <- convertExpression expr vars
    case res of
        op -> return (op, [(getASTLType t, n, Just op)] ++ vars)
        _ -> fmap (\s -> (s, vars)) $ global (fromString n) floatType $ (C.Float $ F.Double 0.0)


convertValue :: MonadModuleBuilder m => Value -> LocalVariables -> IRBuilderT m Operand
convertValue (Nbr n) _ = CB.double $ fromIntegral n
convertValue (RealNbr n) _ = CB.double n
convertValue (Var _ n _) vars = lookupVariable n vars
convertValue (AST.Call (Proto name params retType) args) vars = do
    parameters <- getParamsValueInLLVM args vars
    call (ConstantOperand $ C.GlobalReference (FunctionType floatType (fst $ unzip $ getFunctionParameters params) False) $ fromString name) parameters
--convertValue (GlobVar n) = do
--    return $ LocalReference (FloatingPointType DoubleFP) $ fromString n
--convertValue (Var n IntegerVar) = LocalReference (IntegerType 32) $ fromString n


convertExpression :: MonadModuleBuilder m => Expression -> LocalVariables -> IRBuilderT m (Operand, LocalVariables)
convertExpression (Un (Unary [] val)) vars = do
    op <- convertValue val vars
    return $ (op, vars)
convertExpression (Fct (Decl proto expr)) vars = do
    op <- convertFunction proto expr
    return $ (op, vars)
convertExpression (Expr (Unary [] val) AST.Add expr) vars = do
    leftOp <- convertValue val vars
    (rightOp, locs) <- convertExpression expr vars
    fmap (\s -> (s, locs)) $ fadd leftOp rightOp
convertExpression (Expr (Unary [] val) AST.Sub expr) vars = do
    leftOp <- convertValue val vars
    (rightOp, locs) <- convertExpression expr vars
    fmap (\s -> (s, locs)) $ fsub leftOp rightOp
convertExpression (Expr (Unary [] val) AST.Mul expr) vars = do
    leftOp <- convertValue val vars
    (rightOp, locs) <- convertExpression expr vars
    fmap (\s -> (s, locs)) $ fmul leftOp rightOp
convertExpression (Expr (Unary [] val) AST.Div expr) vars = do
    leftOp <- convertValue val vars
    (rightOp, locs) <- convertExpression expr vars
    fmap (\s -> (s, locs)) $ fdiv leftOp rightOp
convertExpression (Expr (Unary [] val) AST.Asg expr) vars = convertVariable val expr vars

makeASTModule :: String -> [Expression] -> Module
makeASTModule name [] = buildModule (fromString name) $ do
    function "main" [] double $ \_ -> do
        value <- CB.int32 $ 0
        ret $ value
makeASTModule name exprs = buildModule (fromString name) $ do
    function "main" [] double $ \_ -> do
        operand <- convert [] exprs
        ret operand
            where
                convert :: MonadModuleBuilder m => LocalVariables -> [Expression] -> IRBuilderT m Operand
                convert _ [] = CB.int32 0
                convert scope (x:[]) = do
                    (op, _) <- convertExpression x scope
                    return op
                convert scope (x:xs) = do
                    (op, vars) <- convertExpression x scope
                    convert vars xs