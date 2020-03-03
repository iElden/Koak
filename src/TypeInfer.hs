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
    checkExpression
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
isCastValid IntegerVar FloatingVar = True
isCastValid FloatingVar IntegerVar = True
isCastValid a b = a == b

findVarType :: Scope -> String -> Maybe Value
findVarType [] _ = Nothing
findVarType ((varName, var):scope) name
    | name == varName = return var
    | otherwise = findVarType scope name

checkExpression :: Scope -> Expression -> (([Message], Maybe Expression), Scope)
checkExpression scope val@(Un (Unary ops (GlobVar v))) = case findVarType scope v of
    Nothing -> ((varNotFound v val, Nothing), scope)
    Just t -> (([], Just $ Un $ Unary ops t), scope)
checkExpression scope val@(Un (Unary ops var@(Var _ v _))) = case findVarType scope v of
    Nothing -> (([], Just val), (v, var):scope)
    Just t2 -> (([
        Error $ "Variable " ++ v ++ " was previously defined",
        Info $ "-> " ++ show t2,
        getExpr val
        ], Nothing), scope)
checkExpression scope val@(Un (Unary ops (GlobCall v args))) = case findVarType scope v of
    Nothing -> ((varNotFound v val, Nothing), scope)
    Just (Var _ _ (Function proto@(Proto _ argsType _))) -> case length argsType == length args of
        True -> case inferTypes scope args of
            (msgs, Nothing) -> ((msgs, Nothing), scope)
            (msgs, Just va) -> ((msgs, Just $ Un $ (Unary ops $ Call proto va)), scope)
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

checkExpression scope val@(Expr (Unary ops (GlobVar v)) Asg expr) = case findVarType scope v of
    Nothing -> case checkExpression scope expr of
        ((msgs, _), newScope) -> (((varNotFound v val) ++ msgs, Nothing), newScope)
    Just t -> checkExpression scope $ Expr (Unary ops t) Asg expr
checkExpression scope (Expr (Unary ops val@(Var _ v t)) Asg expr) = case checkExpression ((v, val):scope) expr of
    ((msgs, Just x), newScope) -> ((msgs, Just (Expr (Unary ops val) Asg x)), newScope)
    va -> va
checkExpression scope expr@(Expr val Asg _) = (([Error "Unexpected identifier '='", getExpr expr], Nothing), scope)

checkExpression scope (Fct (Decl proto@(Proto name args _) exprs)) = case findVarType scope name of
    Nothing ->
        case filter ((/=) Nothing) $ fmap (\(s, t) -> findVarType scope s) args of
            [] -> case inferTypes (scope ++ fmap (\(n, vt) -> (n, Var Local n vt)) args) exprs of
                (msgs, Nothing) -> ((msgs, Nothing), (name, Var Global name $ Function proto):scope)
                (msgs, Just exs) -> ((msgs, Just (Fct (Decl proto exs))), (name, Var Global name $ Function proto):scope)
            arr -> (([Error "Shadow is prohibited", Info $ "In expression \'" ++ show proto ++ "\'\n"], Nothing), (name, Var Global name $ Function proto):scope)
    Just t -> (([Error $ "Variable " ++ name ++ " already defined with type " ++ show t, Info $ "In expression \'" ++ show proto ++ "\'\n"], Nothing), scope)

checkExpression scope val@(Expr (Unary ops (GlobVar v)) op expr) = case findVarType scope v of
    Nothing -> case checkExpression scope expr of
        ((msgs, _), newScope) -> (((varNotFound v val) ++ msgs, Nothing), newScope)
    Just t -> case checkExpression scope expr of
        result@((_, Nothing), end) -> result
        ((msgs, Just ex), newScope) -> ((msgs, Just $ Expr (Unary ops t) op ex), newScope)
checkExpression scope val@(Expr un@(Unary ops (Var _ v t)) op expr) = case findVarType scope v of
    Nothing ->  case checkExpression scope expr of
        result@((_, Nothing), _) -> result
        ((msgs, Just ex), newScope) -> ((msgs, Just (Expr un op ex)), newScope)
    Just t2 -> (([
        Error $ "Variable " ++ v ++ " was previously defined",
        Info $ "-> " ++ show t2,
        getExpr val
        ], Nothing), scope)
checkExpression scope val@(Expr (Unary ops (GlobCall v args)) op expr) = case findVarType scope v of
    Nothing -> case checkExpression scope expr of
        ((msgs, _), newScope) -> (((varNotFound v val) ++ msgs, Nothing), newScope)
    Just (Var _ _ (Function proto@(Proto _ argsType _))) -> case length argsType == length args of
        True -> case inferTypes scope args of
            (msgs, Nothing) -> ((msgs, Nothing), scope)
            (msgs, Just ex) -> case checkExpression scope $ Expr (Unary ops $ Call proto ex) op expr of
                ((msg, Nothing), newScope) -> ((msgs ++ msg, Nothing), newScope)
                va -> va
        False -> (([Error $ "Not enough arguments for function " ++ show proto, getExpr val], Nothing), scope)
    Just t -> (([Error $ "Cannot call " ++ v ++ ": " ++ show t, getExpr val], Nothing), scope)
checkExpression scope val@(Expr p1 op expr) = case checkExpression scope expr of
    result@((_, Nothing), _) -> result
    ((msgs, Just ex), newScope) -> ((msgs, Just (Expr p1 op ex)), newScope)

checkExpression scope e = (([], Just e), scope)

checkExpressions :: Scope -> [Expression] -> [([Message], Maybe Expression)]
checkExpressions _ [] = []
checkExpressions scope (x:xs) = case checkExpression scope x of
    (val, newScope) -> val:(checkExpressions newScope xs)

inferTypes :: Scope -> [Expression] -> ([Message], Maybe [Expression])
inferTypes _ [] = ([Warning "Empty file given"], Just [])
inferTypes scope exprs = fmap (sequence . reverse) $ foldl (\(a, b) (c, d) -> (a ++ c, d:b)) ([], []) $ checkExpressions scope exprs