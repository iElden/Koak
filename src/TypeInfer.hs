module TypeInfer (
    inferTypes
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
getExpr expr = Info $ "In expression \'" ++ show expr ++ "\'"

castError :: String -> Type -> Type -> Expression -> [Message]
castError varName typeFrom typeTo expr = [Error $ "Cannot cast variable " ++ varName ++ " from " ++ show typeFrom ++ " to " ++ show typeTo, getExpr expr]

varNotFound :: String -> Expression -> [Message]
varNotFound varName expr = [Error $ "Use of undeclared identifier " ++ varName, getExpr expr]

isCastValid :: Type -> Type -> Bool
isCastValid _ _ = False

findVarType :: [(String, Type)] -> String -> Maybe Type
findVarType [] _ = Nothing
findVarType ((varName, varType):scope) name
    | name == varName = return varType
    | otherwise = findVarType scope name

checkExpression :: [(String, Type)] -> Expression -> Bool -> (([Message], Maybe Expression), [(String, Type)])
checkExpression scope val@(Un (Unary ops (GlobVar v))) _ = case findVarType scope v of
    Nothing -> ((varNotFound v val, Nothing), scope)
    Just t -> (([], Just $ Un $ Unary ops $ Var v t), scope)
checkExpression scope val@(Un (Unary ops (Var v t))) _ = case findVarType scope v of
    Nothing -> (([], Just val), (v, t):scope)
    Just t2 -> case isCastValid t2 t of
        True -> (([], Just val), scope)
        False -> ((castError v t2 t val, Nothing), scope)

checkExpression scope val@(Expr (Unary ops (GlobVar v)) Asg expr) _ = notImplemented scope
checkExpression scope val@(Expr (Unary ops (Var v t)) Asg expr) _ = notImplemented scope

checkExpression scope val@(Expr (Unary ops (GlobVar v)) binOp expr) _ = case findVarType scope v of
    Nothing -> ((varNotFound v val, Nothing), scope)
    Just t -> (([], Just $ Expr (Unary ops $ Var v t) binOp expr), scope)
checkExpression scope val@(Expr (Unary ops (Var v t)) binOp expr) _ = case findVarType scope v of
    Nothing -> (([], Just val), (v, t):scope)
    Just t2 -> case isCastValid t2 t of
        True -> (([], Just val), scope)
        False -> ((castError v t2 t val, Nothing), scope)

checkExpression scope e False = (([Warning "This statement has no effect", getExpr e], Just e), scope)
checkExpression scope e _ = (([], Just e), scope)

checkExpressions :: [(String, Type)] -> [Expression] -> [([Message], Maybe Expression)]
checkExpressions _ [] = []
checkExpressions scope (x:[]) = case checkExpression scope x True of
    (val, newScope) -> [val]
checkExpressions scope (x:xs) = case checkExpression scope x False of
    (val, newScope) -> val:(checkExpressions newScope xs)

inferTypes :: [Expression] -> ([Message], Maybe [Expression])
inferTypes [] = ([Warning "Empty file given"], Just [])
inferTypes exprs = fmap (sequence . reverse) $ foldl (\(a, b) (c, d) -> (c ++ a, d:b)) ([], []) $ checkExpressions [] exprs