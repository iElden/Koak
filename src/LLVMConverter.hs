{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module LLVMConverter (makeASTModule) where

import AST
import Data.Text.Lazy.IO as T
import Data.Tuple

import LLVM.Pretty  -- from the llvm-hs-pretty package
import LLVM.AST hiding (function)
import LLVM.AST.Type as ASTL
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Operand as O
import qualified LLVM.AST.IntegerPredicate as I
import qualified LLVM.AST.ParameterAttribute as PA
import qualified LLVM.AST.Instruction as Ins
import qualified LLVM.AST.FloatingPointPredicate as FPP
import qualified LLVM.AST.AddrSpace as Addr

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
type GlobalVariables = [(ASTL.Type, VarName)]
type CurrentVariables = (GlobalVariables, LocalVariables)

lookupVariable :: MonadModuleBuilder m => String -> CurrentVariables -> IRBuilderT m Operand
lookupVariable n (_, []) = do
    load (ConstantOperand $ C.GlobalReference (PointerType floatType $ Addr.AddrSpace 0) (fromString n)) 0
lookupVariable n (gv ,((t, name, op):xs))
    | n == name = case op of
        Just operand -> return operand
        Nothing -> return $ LocalReference t (fromString n)
    | otherwise = lookupVariable n (gv, xs)


getParamsValueInLLVM :: MonadModuleBuilder m => [Expression] -> CurrentVariables -> IRBuilderT m [(Operand, [PA.ParameterAttribute])]
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

executeExpressionsConversion :: MonadModuleBuilder m => CurrentVariables -> [Expression] -> IRBuilderT m (Operand, GlobalVariables)
executeExpressionsConversion (gv, _) [] = do
    returned <- CB.int32 0
    return (returned, gv)
executeExpressionsConversion scope (x:[]) = do
    (op, (gv, _)) <- convertExpression x scope
    return (op, gv)
executeExpressionsConversion scope (x:xs) = do
    (op, vars) <- convertExpression x scope
    executeExpressionsConversion vars xs

convertIfExpr :: MonadModuleBuilder m => Expression -> CurrentVariables -> Name -> IRBuilderT m (Operand, GlobalVariables)
convertIfExpr (IfExpr expr thenExpr Nothing) vars end = do
    (equ, newVars) <- convertExpression expr vars
    thenF <- freshName $ fromString "then"
--    let tN = fromString $ "if" ++ (show thenF)
--    let eN = fromString $ "end" ++ (show endF)
    condBr equ thenF end
    emitBlockStart thenF
    (thenOp, gv) <- executeExpressionsConversion newVars thenExpr
    emitTerm $ Br end []
    return (thenOp, gv)
--convertIfExpr (IfExpr expr thenExpr (Just elseExpr)) vars = do
--    (equ, locs) <- convertExpression expr vars
--    thenN <- fresh
--    elseN <- fresh
--    endN <- fresh
 --   condBr equ thenN elseN
--    emitBlockStart thenN
--    thenOp <- executeExpressionsConversion vars thenExpr
--    emitTerm $ Ins.Br endN []
--    emitBlockStart elseN
--    elseOp <- executeExpressionsConversion vars elseExpr
--    emitTerm $ Ins.Resume elseOp []
--    emitBlockStart endN
 --   return thenOp

createFunctionBody :: MonadModuleBuilder m => [(String, AST.Type)] -> [Expression] -> GlobalVariables -> IRBuilderT m ([Operand] -> IRBuilderT m (), GlobalVariables)
createFunctionBody args exprs gv = do
        (operand, newGv) <- executeExpressionsConversion (gv, putFPinLocalVariables [] $ unzip $ getFunctionParameters args) exprs
        return (\_ -> do
            ret operand, newGv)


convertFunction :: MonadModuleBuilder m => FunctionPrototype -> [Expression] -> GlobalVariables -> IRBuilderT m (Operand, GlobalVariables)
convertFunction (Proto name args retType) exprs gv = do
    (func, glob) <- makeBody
    swap <$> (,) glob <$> makeOperand name args retType func
    where
        makeBody :: MonadModuleBuilder m => IRBuilderT m ([Operand] -> IRBuilderT m (), GlobalVariables)
        makeBody = createFunctionBody args exprs gv
        makeOperand :: MonadModuleBuilder m => String -> [(String, AST.Type)] -> AST.Type -> ([Operand] -> IRBuilderT m ()) -> IRBuilderT m Operand
        makeOperand name args retType fct = do
            function (fromString name) (fmap (fmap fromString) $ getFunctionParameters args) (getASTLType retType) fct



convertVariable :: MonadModuleBuilder m => Value -> Expression -> CurrentVariables -> IRBuilderT m (Operand, CurrentVariables)
convertVariable (Var Global n t) expr vars = do
    (res, _) <- convertExpression expr vars
    case res of
        (ConstantOperand cons) -> do
            op <- global (fromString n) floatType cons
            store res 0 op
            return (op, vars)
        _ -> fmap (\s -> (s, vars)) $ global (fromString n) floatType $ (C.Float $ F.Double 0.0)
convertVariable (Var Local n t) expr vars = do
    (res, (gv, lv)) <- convertExpression expr vars
    case res of
        op -> return (op, (gv, [(getASTLType t, n, Just op)] ++ lv))


convertValue :: MonadModuleBuilder m => Value -> CurrentVariables -> IRBuilderT m Operand
convertValue (Nbr n) _ = CB.double $ fromIntegral n
convertValue (RealNbr n) _ = CB.double n
convertValue (Var _ n _) vars = lookupVariable n vars
convertValue (AST.Call (Proto name params retType) args) vars = do
    parameters <- getParamsValueInLLVM args vars
    call (ConstantOperand $ C.GlobalReference (FunctionType floatType (fst $ unzip $ getFunctionParameters params) False) $ fromString name) parameters
--convertValue (GlobVar n) = do
--    return $ LocalReference (FloatingPointType DoubleFP) $ fromString n
--convertValue (Var n IntegerVar) = LocalReference (IntegerType 32) $ fromString n


convertExpression :: MonadModuleBuilder m => Expression -> CurrentVariables -> IRBuilderT m (Operand, CurrentVariables)
convertExpression (Unary [] val) vars = do
    op <- convertValue val vars
    return $ (op, vars)
convertExpression (Expr (Unary [] val) AST.Asg expr) vars = convertVariable val expr vars
convertExpression (Fct (Decl proto expr)) (gv, lv) = do
    (op, newGv) <- convertFunction proto expr gv
    return $ (op, (newGv, lv))
convertExpression (Expr firstExpr AST.Add secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    fmap (\s -> (s, nVars)) $ fadd leftOp rightOp
convertExpression (Expr firstExpr AST.Sub secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    fmap (\s -> (s, nVars)) $ fsub leftOp rightOp
convertExpression (Expr firstExpr AST.Mul secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    fmap (\s -> (s, nVars)) $ fmul leftOp rightOp
convertExpression (Expr firstExpr AST.Div secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    fmap (\s -> (s, nVars)) $ fdiv leftOp rightOp
convertExpression (Expr firstExpr AST.Equ secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    fmap (\s -> (s, nVars)) $ fcmp FPP.UEQ leftOp rightOp
convertExpression ex@(IfExpr expr thenExpr elseExpr) vars@(_, lv) = do
    endF <- freshName $ fromString "end"
    (op, gv) <- convertIfExpr ex vars endF
    emitBlockStart endF
    return (op, (gv, lv))

makeASTModule :: String -> [Expression] -> Module
makeASTModule name [] = buildModule (fromString name) $ do
    function "main" [] double $ \_ -> do
        value <- CB.int32 $ 0
        ret $ value
makeASTModule name exprs = buildModule (fromString name) $ do
    function "main" [] double $ \_ -> do
        entry <- freshName $ fromString "entry"
        emitBlockStart entry
        (operand, gv) <- executeExpressionsConversion ([], []) exprs
        ret operand