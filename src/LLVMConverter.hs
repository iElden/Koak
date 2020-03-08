{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module LLVMConverter (makeASTModule) where

import AST
import TypeInfer
import Data.Text.Lazy.IO as T
import Data.Tuple
import Data.Functor.Identity


import LLVM.Pretty  -- from the llvm-hs-pretty package
import LLVM.AST hiding (function)
import LLVM.AST.Type as ASTL
import LLVM.AST.Name
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Operand as O
import qualified LLVM.AST.IntegerPredicate as I
import qualified LLVM.AST.ParameterAttribute as PA
import qualified LLVM.AST.Instruction as Ins
import qualified LLVM.AST.FloatingPointPredicate as FPP
import qualified LLVM.AST.IntegerPredicate as IP
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
type ModuleName = String
type LocalVariables = [(ASTL.Type, VarName, Maybe Operand)]
type GlobalVariables = [(ASTL.Type, VarName, Operand)]
type CurrentVariables = (GlobalVariables, LocalVariables)

lookupVariable :: MonadModuleBuilder m => VarName -> CurrentVariables -> IRBuilderT m Operand
lookupVariable n (((t, name, op):xs), [])
    | n == name = do
        load op 0
    | otherwise = lookupVariable n (xs, [])
lookupVariable n (gv, ((t, name, op):xs))
    | n == name = case op of
        Just operand -> do
            load operand 0
        Nothing -> return $ LocalReference t (fromString n)
    | otherwise = lookupVariable n (gv, xs)

isGlobalExisting :: MonadModuleBuilder m => VarName -> GlobalVariables -> IRBuilderT m (Maybe (Operand, ASTL.Type))
isGlobalExisting n [] = return Nothing
isGlobalExisting n ((t, name, op):xs)
    | n == name = return (Just (op, t))
    | otherwise = isGlobalExisting n xs

isLocalExisting :: MonadModuleBuilder m => VarName -> LocalVariables -> IRBuilderT m (Maybe (Operand, ASTL.Type))
isLocalExisting n [] = return Nothing
isLocalExisting n ((t, name, op):xs)
    | n == name = case op of
        Just operand -> return (Just (operand, t))
        Nothing -> return $ Just (LocalReference t (fromString n), t)
    | otherwise = isLocalExisting n xs



getParamsValueInLLVM :: MonadModuleBuilder m => [Expression] -> CurrentVariables -> IRBuilderT m [(Operand, [PA.ParameterAttribute])]
getParamsValueInLLVM list vars = sequence $ fmap (\s -> fmap (\f -> (fst f, [])) s) $ fmap (\t -> convertExpression t vars) list
--getParamsValueInLLVM [] = return []
--getParamsValueInLLVM (c:cs) = sequence ((convertValue c), []) ++ getParamsValueInLLVM cs

getFunctionParameters :: [(VarName, AST.Type)] -> [(ASTL.Type, VarName)]
getFunctionParameters [] = []
getFunctionParameters ((n, typ):cs) = [(getASTLType typ, n)] ++ getFunctionParameters cs

getASTLType :: AST.Type -> ASTL.Type
getASTLType Void = VoidType
getASTLType IntegerVar = IntegerType 32
getASTLType BooleanVar = IntegerType 1
getASTLType FloatingVar = floatType
getASTLType (UnknownType name) = NamedTypeReference (fromString name)
getASTLType (AST.Function (Proto name params retType)) = FunctionType (getASTLType retType) (fst $ unzip $ getFunctionParameters params) False

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
    condBr equ thenF end
    emitBlockStart thenF
    (thenOp, gv) <- executeExpressionsConversion newVars thenExpr
    emitTerm $ Br end []
    return (thenOp, gv)
convertIfExpr (IfExpr expr thenExpr (Just elseExpr)) vars end = do
    (equ, newVars@(_, lv)) <- convertExpression expr vars
    thenF <- freshName $ fromString "then"
    elseF <- freshName $ fromString "else"
    condBr equ thenF elseF
    emitBlockStart thenF
    (thenOp, gv) <- executeExpressionsConversion newVars thenExpr
    emitTerm $ Br end []
    emitBlockStart elseF
    (elseOp, newGv) <- executeExpressionsConversion (gv, lv) elseExpr
    emitTerm $ Br end []
    return (elseOp, newGv)

convertWhileExpr :: MonadModuleBuilder m => Expression -> CurrentVariables -> Name -> IRBuilderT m (Operand, GlobalVariables)
convertWhileExpr (WhileExpr expr body) vars end = do
    (equ, (g, l)) <- convertExpression expr vars
    loopF <- freshName $ fromString "loop"
    condBr equ loopF end
    emitBlockStart loopF
    (loopOp, gv) <- executeExpressionsConversion (g, l) body
    (newEqu, nVars) <- convertExpression expr (gv, l)
    condBr newEqu loopF end
    return (loopOp, gv)

initLocalVariables :: MonadModuleBuilder m => LocalVariables -> [(ASTL.Type, VarName)] -> IRBuilderT m (Operand, LocalVariables)
initLocalVariables lv ((t, n):[]) = do
    op <- alloca t Nothing 0
    store op 0 (LocalReference t $ fromString n)
    return (op, ([(t, n, Just op)] ++ lv))
initLocalVariables lv ((t, n):xs) = do
    op <- alloca t Nothing 0
    store op 0 (LocalReference t $ fromString n)
    initLocalVariables ([(t, n, Just op)] ++ lv) xs


convertFunction :: MonadModuleBuilder m => FunctionPrototype -> [Expression] -> GlobalVariables -> IRBuilderT m Operand
convertFunction (Proto name args retType) exprs gv = do
    function (fromString name) (fmap (fmap fromString) $ getFunctionParameters args) (getASTLType retType) $ \_ -> do
        entry <- freshName $ fromString "entry"
        emitBlockStart entry
        (op, lv) <- initLocalVariables [] $ getFunctionParameters args
        (operand, newGv) <- executeExpressionsConversion (gv, lv) exprs
        ret operand

--guessType :: Operand -> Maybe ASTL.Type
--guessType (LocalReference t n) = Just t
--guessType (ConstantOperand cons) = case cons of
--    (C.Int n _) -> IntegerType n
--    (C.Float _) -> floatType
--    (C.AggregateZero t) -> VoidType
--    (C.FAdd cons1 cons2) -> case (guessType cons1) of
--        (C.Int 32 _) -> case (guessType cons2) of
--            (C.Int 32 _) -> IntegerType 32
--            _ -> floatType
--        _ -> floatType

castValues :: MonadModuleBuilder m => Operand -> ASTL.Type -> ASTL.Type -> IRBuilderT m Operand
castValues op VoidType VoidType = return op
castValues op (IntegerType 1) (IntegerType 1) = return op
castValues op (IntegerType 32) (IntegerType 32) = return op
castValues op (FloatingPointType DoubleFP) (FloatingPointType DoubleFP) = return op
castValues op (NamedTypeReference _) (NamedTypeReference _) = return op
castValues op (FunctionType _ _ _) (FunctionType _ _ _) = return op
castValues op (IntegerType n) (IntegerType 1) = icmp IP.NE op $ ConstantOperand $ C.Int n 0
castValues op (IntegerType 1) (IntegerType n) = sext op (IntegerType n)
castValues op (IntegerType n) (FloatingPointType DoubleFP) = sitofp op floatType
castValues op (FloatingPointType DoubleFP) (IntegerType n) = fptosi op (IntegerType n)


createGlobal :: MonadModuleBuilder m => Name -> ASTL.Type -> IRBuilderT m Operand
createGlobal n VoidType = global n VoidType $ C.AggregateZero VoidType
createGlobal n (IntegerType i) = global n (IntegerType i) $ C.Int i 0
createGlobal n (FloatingPointType DoubleFP) = global n floatType $ C.Float $ F.Double 0
createGlobal n (NamedTypeReference _) = global n VoidType $ C.AggregateZero VoidType
createGlobal n funcType@(FunctionType retType aT vA) = global n funcType $ C.Undef funcType


convertVariable :: MonadModuleBuilder m => Value -> Expression -> CurrentVariables -> IRBuilderT m (Operand, CurrentVariables)
convertVariable (Var Global n t) expr vars = do
    (res, (gv, lv)) <- convertExpression expr vars
    sol <- isGlobalExisting n gv
    case sol of
        Just (op, exT) -> do
            casted <- castValues res exT $ getASTLType t
            store op 0 casted
            return (op, (gv, lv))
        Nothing -> do
            op <- createGlobal (fromString n) (getASTLType t)
            store op 0 res
            return (op, ([(getASTLType t, n, op)] ++ gv, lv))
convertVariable (Var Local n t) expr vars = do
    (res, (gv, lv)) <- convertExpression expr vars
    sol <- isLocalExisting n lv
    case sol of
        Just (op, exT) -> do
            casted <- castValues res exT $ getASTLType t
            store op 0 casted
            return (op, (gv, lv))
        Nothing -> do
            op <- alloca (getASTLType t) Nothing 0
            store op 0 res
            return (op, (gv, [(getASTLType t, n, Just op)] ++ lv))

-- (Just (ConstantOperand $ C.Float $ F.Double 0))
convertUnaryOpCons :: MonadModuleBuilder m => [UnaryOp] -> Operand -> IRBuilderT m Operand
convertUnaryOpCons [] op = do
    return op
convertUnaryOpCons (Minus:xs) val = do
    op <- fmul (ConstantOperand $ C.Float $ F.Double (-1)) val
    convertUnaryOpCons xs op
convertUnaryOpCons (BinNot:xs) val = do
    op <- LLVM.IRBuilder.Instruction.xor (ConstantOperand $ C.Int 32 0xFFFFFFFF) val
    convertUnaryOpCons xs op
convertUnaryOpCons (BoolNot:xs) val = do
    op <- LLVM.IRBuilder.Instruction.xor (ConstantOperand $ C.Int 1 1) val
    convertUnaryOpCons xs op
convertUnaryOpCons (_:xs) val = convertUnaryOpCons xs val

convertValue :: MonadModuleBuilder m => Value -> CurrentVariables -> IRBuilderT m Operand
convertValue (Nbr n) _ = CB.int32 $ fromIntegral n
convertValue (Boolean True) _ = CB.bit 1
convertValue (Boolean False) _ = CB.bit 0
convertValue (RealNbr n) _ = CB.double n
convertValue (Var _ n _) vars = lookupVariable n vars
convertValue (AST.Call (Proto name params retType) args) vars = do
    parameters <- getParamsValueInLLVM args vars
    call (ConstantOperand $ C.GlobalReference (FunctionType (getASTLType retType) (fst $ unzip $ getFunctionParameters params) False) $ fromString name) parameters
--convertValue (GlobVar n) = do
--    return $ LocalReference (FloatingPointType DoubleFP) $ fromString n
--convertValue (Var n IntegerVar) = LocalReference (IntegerType 32) $ fromString n


convertExpression :: MonadModuleBuilder m => Expression -> CurrentVariables -> IRBuilderT m (Operand, CurrentVariables)
convertExpression (Unary opt val) vars = do
    op <- convertValue val vars
    newOp <- convertUnaryOpCons (reverse opt) op
    return $ (newOp, vars)
convertExpression (Cast tb ta expr) vars = do
    (op, newVars) <- convertExpression expr vars
    newOp <- castValues op (getASTLType tb) (getASTLType ta)
    return (newOp, newVars)
convertExpression (Expr (Unary [] val) AST.Asg expr) vars = convertVariable val expr vars
convertExpression (Fct (Decl proto expr)) (gv, lv) = do
    op <- convertFunction proto expr gv
    return $ (op, (gv, lv))
convertExpression (Extern n (AST.Function (Proto name args retType))) vars = do
    let types = fst $ unzip $ getFunctionParameters args
    fmap (\s -> (s, vars)) $ extern (fromString n) types $ getASTLType retType

-- CLASSIC CALCULATIONS --
convertExpression (Expr firstExpr AST.Add secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    let typ = getExpressionType firstExpr
    case typ of
        (_, Just IntegerVar) -> fmap (\s -> (s, nVars)) $ add leftOp rightOp
        _ -> fmap (\s -> (s, nVars)) $ fadd leftOp rightOp
convertExpression (Expr firstExpr AST.Sub secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    let typ = getExpressionType firstExpr
    case typ of
        (_, Just IntegerVar) -> fmap (\s -> (s, nVars)) $ sub leftOp rightOp
        _ -> fmap (\s -> (s, nVars)) $ fsub leftOp rightOp
convertExpression (Expr firstExpr AST.Mul secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    let typ = getExpressionType firstExpr
    case typ of
        (_, Just IntegerVar) -> fmap (\s -> (s, nVars)) $ mul leftOp rightOp
        _ -> fmap (\s -> (s, nVars)) $ fmul leftOp rightOp
convertExpression (Expr firstExpr AST.Div secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    let typ = getExpressionType firstExpr
    case typ of
        (_, Just IntegerVar) -> fmap (\s -> (s, nVars)) $ sdiv leftOp rightOp
        _ -> fmap (\s -> (s, nVars)) $ fdiv leftOp rightOp
convertExpression (Expr firstExpr AST.Mod secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    let typ = getExpressionType firstExpr
    case typ of
        (_, Just IntegerVar) -> fmap (\s -> (s, nVars)) $ srem leftOp rightOp
        _ -> fmap (\s -> (s, nVars)) $ frem leftOp rightOp
convertExpression (Expr firstExpr AST.Pow secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    let fctOp = (ConstantOperand $ C.GlobalReference (FunctionType floatType ([floatType, floatType]) False) $ fromString "pow")
    let typ = getExpressionType firstExpr
    case typ of
        (_, Just IntegerVar) -> do
            newLeftOp <- castValues leftOp (IntegerType 32) (floatType)
            newRightOp <- castValues rightOp (IntegerType 32) (floatType)
            let params = [(newLeftOp, []), (newRightOp, [])]
            newOp <- call fctOp params
            fmap (\s -> (s, nVars)) $ castValues newOp (floatType) (IntegerType 32)
        _ -> do
            let params = [(leftOp, []), (rightOp, [])]
            fmap (\s -> (s, nVars)) $ call fctOp params

-- BINARY CALCULATIONS --
convertExpression (Expr firstExpr AST.And secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    fmap (\s -> (s, nVars)) $ LLVM.IRBuilder.Instruction.and leftOp rightOp
convertExpression (Expr firstExpr AST.Or secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    fmap (\s -> (s, nVars)) $ LLVM.IRBuilder.Instruction.or leftOp rightOp
convertExpression (Expr firstExpr AST.Xor secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    fmap (\s -> (s, nVars)) $ LLVM.IRBuilder.Instruction.xor leftOp rightOp
convertExpression (Expr firstExpr AST.RSh secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    fmap (\s -> (s, nVars)) $ lshr leftOp rightOp
convertExpression (Expr firstExpr AST.LSh secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    fmap (\s -> (s, nVars)) $ shl leftOp rightOp

-- COMPARISONS --
convertExpression (Expr firstExpr AST.Equ secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    let typ = getExpressionType firstExpr
    case typ of
        (_, Just IntegerVar) -> fmap (\s -> (s, nVars)) $ icmp IP.EQ leftOp rightOp
        _ -> fmap (\s -> (s, nVars)) $ fcmp FPP.UEQ leftOp rightOp
convertExpression (Expr firstExpr AST.Neq secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    let typ = getExpressionType firstExpr
    case typ of
        (_, Just IntegerVar) -> fmap (\s -> (s, nVars)) $ icmp IP.NE leftOp rightOp
        _ -> fmap (\s -> (s, nVars)) $ fcmp FPP.UNE leftOp rightOp
convertExpression (Expr firstExpr AST.Gt secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    let typ = getExpressionType firstExpr
    case typ of
        (_, Just IntegerVar) -> fmap (\s -> (s, nVars)) $ icmp IP.UGT leftOp rightOp
        _ -> fmap (\s -> (s, nVars)) $ fcmp FPP.UGT leftOp rightOp
convertExpression (Expr firstExpr AST.Gte secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    let typ = getExpressionType firstExpr
    case typ of
        (_, Just IntegerVar) -> fmap (\s -> (s, nVars)) $ icmp IP.UGE leftOp rightOp
        _ -> fmap (\s -> (s, nVars)) $ fcmp FPP.UGE leftOp rightOp
convertExpression (Expr firstExpr AST.Lt secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    let typ = getExpressionType firstExpr
    case typ of
        (_, Just IntegerVar) -> fmap (\s -> (s, nVars)) $ icmp IP.ULT leftOp rightOp
        _ -> fmap (\s -> (s, nVars)) $ fcmp FPP.ULT leftOp rightOp
convertExpression (Expr firstExpr AST.Lte secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    let typ = getExpressionType firstExpr
    case typ of
        (_, Just IntegerVar) -> fmap (\s -> (s, nVars)) $ icmp IP.ULE leftOp rightOp
        _ -> fmap (\s -> (s, nVars)) $ fcmp FPP.ULE leftOp rightOp
convertExpression (Expr firstExpr AST.BAnd secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    fmap (\s -> (s, nVars)) $ LLVM.IRBuilder.Instruction.and leftOp rightOp
convertExpression (Expr firstExpr AST.BOr secExpr) vars = do
    (leftOp, newVars) <- convertExpression firstExpr vars
    (rightOp, nVars) <- convertExpression secExpr newVars
    fmap (\s -> (s, nVars)) $ LLVM.IRBuilder.Instruction.or leftOp rightOp

-- Conditional Branching EXPR --
convertExpression ex@(IfExpr expr thenExpr elseExpr) vars@(_, lv) = do
    endF <- freshName $ fromString "end"
    (op, gv) <- convertIfExpr ex vars endF
    emitBlockStart endF
    return (op, (gv, lv))
convertExpression ex@(WhileExpr expr body) vars@(_, lv) = do
    endF <- freshName $ fromString "end"
    (op, gv) <- convertWhileExpr ex vars endF
    emitBlockStart endF
    return (op, (gv, lv))

-- CRASH --
convertExpression expr _ = error $ "Unimplemented expression '" ++ show expr ++ "'"

createFunctionsWithoutMain :: Monad m => [Expression] -> ModuleBuilderT m Operand
createFunctionsWithoutMain (x:[]) = case x of
    (Fct (Decl (Proto name args retType) exprs)) -> function (fromString name) (fmap (fmap fromString) $ getFunctionParameters args) (getASTLType retType) $ \_ -> do
        entry <- freshName $ fromString "entry"
        emitBlockStart entry
        (op, lv) <- initLocalVariables [] $ getFunctionParameters args
        (operand, gv) <- executeExpressionsConversion ([], lv) exprs
        ret operand
    (Extern name (AST.Function (Proto _ args retType))) -> do
        let types = fst $ unzip $ getFunctionParameters args
        extern (fromString name) types $ getASTLType retType
createFunctionsWithoutMain (x:xs) = case x of
    (Fct (Decl (Proto name args retType) exprs)) -> do
        function (fromString name) (fmap (fmap fromString) $ getFunctionParameters args) (getASTLType retType) $ \_ -> do
            entry <- freshName $ fromString "entry"
            emitBlockStart entry
            (op, lv) <- initLocalVariables [] $ getFunctionParameters args
            (operand, gv) <- executeExpressionsConversion ([], lv) exprs
            ret operand
        createFunctionsWithoutMain xs
    (Extern name (AST.Function (Proto _ args retType))) -> do
         let types = fst $ unzip $ getFunctionParameters args
         extern (fromString name) types $ getASTLType retType
         createFunctionsWithoutMain xs

makeASTModule :: ModuleName -> [Expression] -> Module
makeASTModule name [] = buildModule (fromString name) $ do
    function "main" [] i32 $ \_ -> do
        value <- CB.int32 $ 0
        ret $ value
makeASTModule name exprs = buildModule (fromString name) $ do
    extern (fromString "pow") [floatType, floatType] floatType
    case filter isReleventExpression exprs of
        [] -> createFunctionsWithoutMain exprs
        _ -> function "main" [] i32 $ \_ -> do
            entry <- freshName $ fromString "entry"
            emitBlockStart entry
            (operand, gv) <- executeExpressionsConversion ([], []) exprs
            ret operand