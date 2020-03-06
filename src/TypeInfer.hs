module TypeInfer (
    inferTypes,
    Message (..),
    notImplemented,
    getExpr,
    castError,
    varNotFound,
    noEffect,
    isCastValid,
    findVarType,
    checkExpression,
    checkExpressionsType
    ) where

import AST

data Message =
    Info String |
    Warning String |
    Error String
    deriving Eq

type Scope = [(String, Value)]

instance Show Message where
    show (Info s) = "info: " ++ s
    show (Warning s) = "warning: " ++ s
    show (Error s) = "fatal error: " ++ s

notImplemented :: Scope -> (([Message], Maybe Expression), Scope)
notImplemented scope = (([Error "Not implemented"], Nothing), scope)

getExpr :: Expression -> Message
getExpr expr = Info $ "In expression \'" ++ show expr ++ "\'\n"

castError :: String -> Type -> Type -> Expression -> [Message]
castError varName typeFrom typeTo expr = [Error $ "Cannot cast variable " ++ varName ++ " from " ++ show typeFrom ++ " to " ++ show typeTo, getExpr expr]

varNotFound :: String -> Expression -> [Message]
varNotFound varName expr = [Error $ "Use of undeclared identifier " ++ varName, getExpr expr]

noEffect :: Expression -> [Message]
noEffect e = [Warning "This statement has no effect", getExpr e]

isCastValid :: Type -> Type -> Bool
isCastValid IntegerVar BooleanVar = True
isCastValid IntegerVar FloatingVar = True
isCastValid FloatingVar IntegerVar = True
isCastValid _ Void = True
isCastValid a b = a == b

isOpValid :: BinaryOp -> Type -> Type -> Bool
isOpValid And IntegerVar IntegerVar = True
isOpValid Or  IntegerVar IntegerVar = True
isOpValid Xor IntegerVar IntegerVar = True
isOpValid RSh IntegerVar IntegerVar = True
isOpValid LSh IntegerVar IntegerVar = True
isOpValid And _ _ = False
isOpValid Or  _ _ = False
isOpValid Xor _ _ = False
isOpValid RSh _ _ = False
isOpValid LSh _ _ = False
isOpValid _ (UnknownType _) (UnknownType _) = False
isOpValid _ t1 t2 = t1 == t2

getOpType :: BinaryOp -> Type -> Type -> Type
getOpType BAnd _ _ = BooleanVar
getOpType BOr  _ _ = BooleanVar
getOpType Equ  _ _ = BooleanVar
getOpType Neq  _ _ = BooleanVar
getOpType Gt   _ _ = BooleanVar
getOpType Gte  _ _ = BooleanVar
getOpType Lt   _ _ = BooleanVar
getOpType Lte  _ _ = BooleanVar
getOpType _ t _ = t

findVarType :: Scope -> String -> Maybe Value
findVarType [] _ = Nothing
findVarType ((varName, var):scope) name
    | name == varName = return var
    | otherwise = findVarType scope name

getExpressionType :: Expression -> ([Message], Maybe Type)
getExpressionType (Unary _ (Nbr n))                     = ([], Just IntegerVar)
getExpressionType (Unary _ (RealNbr n))                 = ([], Just FloatingVar)
getExpressionType (Unary _ (Var _ _ t))                 = ([], Just t)
getExpressionType (Unary _ (Call (Proto _ _ retType) _))= ([], Just retType)
getExpressionType (IfExpr cond ifExprs Nothing)         = case checkExpressionsType ifExprs of
    (msgs, Nothing)-> (msgs, Nothing)
    (msgs, _)      -> case getExpressionType cond of
        (msgs2, Just BooleanVar)-> (msgs ++ msgs2, Just Void)
        (msgs2, Nothing)        -> (msgs ++ msgs2, Nothing)
        (msgs2, Just t)         -> (msgs ++ msgs2 ++ [Error $ "Couldn't match expected type bool with actual type " ++ show t, getExpr cond], Nothing)
getExpressionType (IfExpr cond ifExprs (Just elseExprs))= case checkExpressionsType ifExprs of
    (msgs, Nothing)-> (msgs, Nothing)
    (msgs, _)      -> case checkExpressionsType elseExprs of
        (msgs, Nothing)-> (msgs, Nothing)
        (msgs2, _)     -> case getExpressionType cond of
            (msgs3, Just BooleanVar)-> (msgs ++ msgs2 ++ msgs3, Just Void)
            (msgs3, Nothing)        -> (msgs ++ msgs2 ++ msgs3, Nothing)
            (msgs3, Just t)         -> (msgs ++ msgs2 ++ msgs3 ++ [Error $ "Couldn't match expected type bool with actual type " ++ show t, getExpr cond], Nothing)
getExpressionType (WhileExpr cond whileExprs)           = case checkExpressionsType whileExprs of
    (msgs, Nothing)-> (msgs, Nothing)
    (msgs, _)      -> case getExpressionType cond of
        (msgs2, Just BooleanVar)-> (msgs ++ msgs2, Just Void)
        (msgs2, Nothing)        -> (msgs ++ msgs2, Nothing)
        (msgs2, Just t)         -> (msgs ++ msgs2 ++ [Error $ "Couldn't match expected type bool with actual type " ++ show t, getExpr cond], Nothing)
getExpressionType v@(Expr expr1 op expr2)                = case getExpressionType expr1 of
    (msgs, Nothing) -> (msgs, Nothing)
    (msgs, Just t1) -> case getExpressionType expr2 of
        (msgs2, Nothing) -> (msgs ++ msgs2, Nothing)
        (msgs2, Just t2) ->
            if isOpValid op t1 t2 then
                (msgs ++ msgs2, Just $ getOpType op t1 t2)
            else
                (msgs ++ msgs2 ++ [Error $ "Invalid operand '" ++ show op ++ "' between " ++ show t1 ++ " and " ++ show t2, getExpr v], Nothing)

getExpressionType (Fct fct)                             = ([], Just Void)
getExpressionType (Cast t _)                            = ([], Just t)
getExpressionType (Extern name t)                       = ([], Just Void)
getExpressionType expr@(Unary _ (GlobVar n))            = ([Error $ "Cannot get type of " ++ show expr, getExpr expr], Nothing)
getExpressionType expr@(Unary _ (GlobCall n args))      = ([Error $ "Cannot get type of " ++ show expr, getExpr expr], Nothing)
getExpressionType _                                     = ([Error "Not implemented"], Nothing)

checkExpressionsType :: [Expression] -> ([Message], Maybe [Expression])
checkExpressionsType exprs = case fmap sequence_ $ foldl (\(a, b)(c, d) -> (a ++ c, d:b)) ([], []) $ fmap getExpressionType exprs of
    (msgs, Nothing) -> (msgs, Nothing)
    (msgs, _) -> (msgs, Just exprs)

checkExpression :: Scope -> Expression -> (([Message], Maybe Expression), Scope)
checkExpression scope val@(Unary ops (GlobVar v)) = case findVarType scope v of
    Nothing -> ((varNotFound v val, Nothing), scope)
    Just t -> (([], Just $ Unary ops t), scope)
checkExpression scope val@(Unary ops var@(Var _ v _)) = case findVarType scope v of
    Nothing -> (([], Just val), (v, var):scope)
    Just t2 -> (([
        Error $ "Variable " ++ v ++ " was previously defined",
        Info $ "-> " ++ show t2,
        getExpr val
        ], Nothing), scope)
checkExpression scope val@(Unary ops (GlobCall v args)) = case findVarType scope v of
    Nothing -> ((varNotFound v val, Nothing), scope)
    Just (Var _ _ (Function proto@(Proto _ argsType _))) -> case length argsType == length args of
        True -> case inferTypes scope args of
            (msgs, Nothing) -> ((msgs, Nothing), scope)
            (msgs, Just va) -> ((msgs, Just $ Unary ops $ Call proto va), scope)
        False -> (([Error $ "Not enough arguments for function " ++ show proto, getExpr val], Nothing), scope)
    Just t -> (([Error $ "Cannot call " ++ show t, getExpr val], Nothing), scope)

checkExpression scope (IfExpr cond ifExprs Nothing) = case checkExpression scope cond of
    expr@((_, Nothing), _) -> expr
    ((msgs, Just econd), newScope) -> case inferTypes scope ifExprs of
        (msgs, Nothing) -> ((msgs, Nothing), newScope)
        (msgs, Just expr) -> ((msgs, Just $ IfExpr econd expr Nothing), newScope)
checkExpression scope (IfExpr cond ifExprs (Just elseExprs)) =  case checkExpression scope cond of
    expr@((_, Nothing), _) -> expr
    ((msgs, Just econd), newScope) -> case inferTypes scope ifExprs of
        expr@(msgs, Nothing) -> ((msgs, Nothing), scope)
        (msgs, Just expr) -> case inferTypes scope elseExprs of
             (ms, Nothing) -> ((msgs ++ ms, Nothing), scope)
             (ms, elseExpr) -> ((msgs ++ ms, Just $ IfExpr econd expr elseExpr), scope)

checkExpression scope (WhileExpr cond ifExprs) = case checkExpression scope cond of
    expr@((_, Nothing), _) -> expr
    ((msgs, Just econd), newScope) -> case inferTypes scope ifExprs of
        (msgs, Nothing) -> ((msgs, Nothing), newScope)
        (msgs, Just expr) -> ((msgs, Just $ WhileExpr econd expr), newScope)

checkExpression scope val@(Expr (Unary ops (GlobVar v)) Asg expr) = case findVarType scope v of
    Nothing -> case checkExpression scope expr of
        ((msgs, _), newScope) -> (((varNotFound v val) ++ msgs, Nothing), newScope)
    Just t -> checkExpression scope $ Expr (Unary ops t) Asg expr
checkExpression scope (Expr (Unary ops val@(Var _ v t)) Asg expr) = case checkExpression ((v, val):scope) expr of
    ((msgs, Just x), newScope) -> ((msgs, Just (Expr (Unary ops val) Asg x)), newScope)
    va -> va
checkExpression scope expr@(Expr val Asg _) = (([Error "Unexpected identifier '='", getExpr expr], Nothing), scope)

checkExpression scope expr@(Cast t ex) = case checkExpression scope ex of
    ((msgs, Just x), newScope) -> ((msgs, Just $ Cast t x), newScope)
    va -> va

checkExpression scope expr@(Extern name t) = (([], Just expr), (name, (Var Global name t)):scope)

checkExpression scope (Fct (Decl proto@(Proto name args _) exprs)) = case findVarType scope name of
    Nothing ->
        case filter ((/=) Nothing) $ fmap (\(s, t) -> findVarType scope s) args of
            [] -> case inferTypes (scope ++ fmap (\(n, vt) -> (n, Var Local n vt)) args) exprs of
                (msgs, Nothing) -> ((msgs, Nothing), (name, Var Global name $ Function proto):scope)
                (msgs, Just exs) -> ((msgs, Just (Fct (Decl proto exs))), (name, Var Global name $ Function proto):scope)
            arr -> (([Error "Shadow is prohibited", Info $ "In expression \'" ++ show proto ++ "\'\n"], Nothing), (name, Var Global name $ Function proto):scope)
    Just t -> (([Error $ "Variable " ++ name ++ " already defined with type " ++ show t, Info $ "In expression \'" ++ show proto ++ "\'\n"], Nothing), scope)

checkExpression scope val@(Expr ex1 op ex2) = case checkExpression scope ex1 of
    result@((_, Nothing), _) -> result
    ((msgs, Just exp1), newScope1) -> case checkExpression newScope1 ex2 of
        result@((_, Nothing), _) -> result
        ((msgs, Just exp2), newScope2) -> ((msgs, Just (Expr exp1 op exp2)), newScope2)

checkExpression scope e = (([], Just e), scope)

checkExpressions :: Scope -> [Expression] -> [([Message], Maybe Expression)]
checkExpressions _ [] = []
checkExpressions scope (x:xs) = case checkExpression scope x of
    (val, newScope) -> val:(checkExpressions newScope xs)

inferTypes :: Scope -> [Expression] -> ([Message], Maybe [Expression])
inferTypes _ [] = ([Warning "Empty file given"], Just [])
inferTypes scope exprs = fmap (sequence . reverse) $ foldl (\(a, b) (c, d) -> (a ++ c, d:b)) ([], []) $ checkExpressions scope exprs