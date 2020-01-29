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

checkExpression :: [(String, Type)] -> Expression -> (([Message], Maybe Expression), [(String, Type)])
checkExpression scope e = (([Error "Not implemented"], Nothing), scope)

checkExpressions :: [(String, Type)] -> [Expression] -> [([Message], Maybe Expression)]
checkExpressions _ [] = []
checkExpressions scope (x:xs) = case checkExpression scope x of
    (val, newScope) -> val:(checkExpressions newScope xs)

inferTypes :: [Expression] -> ([Message], Maybe [Expression])
inferTypes [] = ([Warning "Empty file given"], Just [])
inferTypes exprs = fmap sequence $ foldl (\(a, b) (c, d) -> (c ++ a, d:b)) ([], []) $ checkExpressions [] exprs