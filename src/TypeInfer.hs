module TypeInfer (
    inferTypes,
    Message
    ) where

import AST

data Message =
    Info String |
    Warning String |
    Error String

instance Show Message where
    show (Info s) = "info: " ++ s
    show (Warning s) = "warning: " ++ s
    show (Error s) = "fatal error: " ++ s

notImplemented :: [(String, Type)] -> (([Message], Maybe Expression), [(String, Type)])
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
isCastValid _ _ = False

findVarType :: [(String, Type)] -> String -> Maybe Type
findVarType [] _ = Nothing
findVarType ((varName, varType):scope) name
    | name == varName = return varType
    | otherwise = findVarType scope name

checkExpression :: [(String, Type)] -> Expression -> (([Message], Maybe Expression), [(String, Type)])
checkExpression scope val@(Un (Unary ops (GlobVar v))) = case findVarType scope v of
    Nothing -> ((varNotFound v val, Nothing), scope)
    Just t -> (([], Just $ Un $ Unary ops $ Var v t), scope)
checkExpression scope val@(Un (Unary ops (Var v t))) = case findVarType scope v of
    Nothing -> (([], Just val), (v, t):scope)
    Just t2 -> case isCastValid t2 t of
        True -> (([], Just val), scope)
        False -> ((castError v t2 t val, Nothing), scope)

checkExpression scope val@(Expr (Unary ops (GlobVar v)) Asg expr) = case findVarType scope v of
    Nothing -> case checkExpression scope expr of
        ((msgs, _), newScope) -> (((varNotFound v val) ++ msgs, Nothing), newScope)
    Just t -> checkExpression scope $ Expr (Unary ops $ Var v t) Asg expr
checkExpression scope (Expr (Unary ops val@(Var v t)) Asg expr) = case checkExpression ((v, t):scope) expr of
    ((msgs, Just x), newScope) -> ((msgs, Just (Expr (Unary ops val) Asg x)), newScope)
    v -> v
checkExpression scope expr@(Expr val Asg _) = (([Error "Unexpected identifier '='", getExpr expr], Nothing), scope)

checkExpression scope (Fct (Decl proto@(Proto name args _) exprs)) = case findVarType scope name of
    Nothing ->
        case filter ((/=) Nothing) $ fmap (\(s, t) -> findVarType scope s) args of
            [] -> case inferTypes (scope ++ args) exprs of
                (msgs, Nothing) -> ((msgs, Nothing), scope)
                (msgs, Just exs) -> ((msgs, Just (Fct (Decl proto exs))), scope)
            arr -> (([Error "Shadow is prohibited", Info $ "In expression \'" ++ show proto ++ "\'\n"], Nothing), scope)
    Just t -> (([Error $ "Variable " ++ name ++ " already defined with type " ++ show t, Info $ "In expression \'" ++ show proto ++ "\'\n"], Nothing), scope)

checkExpression scope val@(Expr (Unary ops (GlobVar v)) op expr) = case findVarType scope v of
    Nothing -> case checkExpression scope expr of
        ((msgs, _), newScope) -> (((varNotFound v val) ++ msgs, Nothing), newScope)
    Just t -> case checkExpression scope expr of
        result@((_, Nothing), end) -> result
        ((msgs, Just ex), newScope) -> ((msgs, Just $ Expr (Unary ops (Var v t)) op ex), newScope)
checkExpression scope val@(Expr un@(Unary ops (Var v t)) op expr) = case findVarType scope v of
    Nothing ->  case checkExpression scope expr of
        result@((_, Nothing), _) -> result
        ((msgs, Just ex), newScope) -> ((msgs, Just (Expr un op ex)), newScope)
    Just t2 -> case isCastValid t2 t of
        True -> case checkExpression scope expr of
            result@((_, Nothing), _) -> result
            ((msgs, Just ex), newScope) -> ((msgs, Just (Expr un op ex)), newScope)
        False -> case checkExpression scope expr of
            ((msgs, Just ex), newScope) -> (((castError v t2 t val) ++ msgs, Just ex), newScope)
checkExpression scope val@(Expr p1 op expr) = case checkExpression scope expr of
    result@((_, Nothing), _) -> result
    ((msgs, Just ex), newScope) -> ((msgs, Just (Expr p1 op ex)), newScope)

checkExpression scope e = (([], Just e), scope)

checkExpressions :: [(String, Type)] -> [Expression] -> [([Message], Maybe Expression)]
checkExpressions _ [] = []
checkExpressions scope (x:xs) = case checkExpression scope x of
    (val, newScope) -> val:(checkExpressions newScope xs)

inferTypes :: [(String, Type)] -> [Expression] -> ([Message], Maybe [Expression])
inferTypes _ [] = ([Warning "Empty file given"], Just [])
inferTypes scope exprs = fmap (sequence . reverse) $ foldl (\(a, b) (c, d) -> (a ++ c, d:b)) ([], []) $ checkExpressions scope exprs