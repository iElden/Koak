module Parser(
    Parser (..),
    runParser,
    parseChar,
    parseCharBlackList,
    parseCharSequence,
    parseInteger,
    parseDouble,
    parseLiteral,
    parseIdentifier
) where

import AST
import Text.Read
import Data.Maybe
import Control.Applicative

data Parser a = Parse (String -> Maybe (a, String))

instance Functor Parser where
    fmap f (Parse parser) = Parse (\str -> do
        (val, remain) <- parser str
        return (f val, remain))

instance Applicative Parser where
    pure a = Parse (\s -> Just (a, s))
    (<*>) (Parse p1) p2 = Parse (\s -> do
        (f, remain) <- p1 s
        (v, remain2) <- runParser p2 remain
        return (f v, remain2))

instance Monad Parser where
    (>>=) (Parse v) f = Parse (\s -> do
        (a, remain) <- v s
        runParser (f a) remain)

instance Alternative Parser where
    empty = Parse (\_ -> Nothing)
    (<|>) (Parse p1) (Parse p2) = Parse (\s -> do
        case p1 s of
            Just v -> return v
            Nothing -> p2 s)

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parse f) s = f s

parseChar :: String -> Parser Char
parseChar list = Parse $ parse list
    where
        parse _ "" = Nothing
        parse list (c:str)
            | elem c list = return (c, str)
            | otherwise = Nothing

parseCharBlackList :: String -> Parser Char
parseCharBlackList list = Parse $ parse list
    where
        parse _ "" = Nothing
        parse list (c:str)
            | elem c list = Nothing
            | otherwise = return (c, str)

parseCharSequence :: String -> Parser String
parseCharSequence "" = pure ""
parseCharSequence (c:str) = (:) <$> parseChar [c] <*> parseCharSequence str

parseString :: [String] -> Parser String
parseString [] = empty
parseString (v:others) = parseCharSequence v <|> parseString others

parseDigit :: Parser Char
parseDigit = parseChar ['0'..'9']

parseAlpha :: Parser Char
parseAlpha = parseChar $ ['a'..'z'] ++ ['A'..'Z']

parseAlphaNum :: Parser Char
parseAlphaNum = parseDigit <|> parseAlpha

parseWhiteSpace :: Parser Char
parseWhiteSpace = parseChar " \t\n"

parseInteger :: Parser Value
parseInteger = do
    intPart <- (readMaybe :: String -> Maybe Int) <$> many parseDigit
    maybe empty (pure . Nbr) intPart

parseDouble :: Parser Value
parseDouble = do
    nbr <- (readMaybe :: String -> Maybe Double) <$> do
        intPart <- many parseDigit
        _ <- parseChar "."
        decPart <- many parseDigit
        return $ "0" ++ intPart ++ "." ++ decPart ++ "0"
    maybe empty (pure . RealNbr) nbr

parseIdentifier :: Parser Value
parseIdentifier = GlobVar <$> ((:) <$> parseAlpha <*> many parseAlphaNum)

parseType :: Parser Type
parseType = do
    t <- (:) <$> parseAlpha <*> many parseAlphaNum
    case t of
        "int" -> return IntegerVar
        "void" -> return Void
        "double" -> return FloatingVar
        "" -> empty
        _ -> return $ UnknownType t

parseTypedIdentifier :: Parser Value
parseTypedIdentifier = Var <$> ((:) <$> parseAlpha <*> many parseAlphaNum) <*> (many parseWhiteSpace *> parseChar ":" *> many parseWhiteSpace *> parseType)

parseBinOp :: Parser BinaryOp
parseBinOp = do
    test <- many parseWhiteSpace *> parseString ["+", "-", "**", "*", "/", "&", "|", "^", "%", ">>", "<<", "==", "!=", "<=", "<", ">=", ">", "="]
    case test of
        "+" -> return Add
        "-" -> return Sub
        "*" -> return Mul
        "/" -> return Div
        "&" -> return And
        "|" -> return Or
        "^" -> return Xor
        "**" -> return Pow
        "%" -> return Mod
        ">>" -> return RSh
        "<<" -> return LSh
        "==" -> return Equ
        "!=" -> return Neq
        ">" -> return Gt
        ">=" -> return Gte
        "<" -> return Lt
        "<=" -> return Lte
        "=" -> return Asg

parseUnOp :: Parser UnaryOp
parseUnOp = do
    test <- many parseWhiteSpace *> parseString ["+", "-", "!", "~"]
    case test of
        "+" -> return Plus
        "-" -> return Minus
        "!" -> return BoolNot
        "~" -> return BinNot

parseBinExpr :: Parser Expression
parseBinExpr = Expr <$> (many parseWhiteSpace *> parseUnary <* many parseWhiteSpace) <*> (parseBinOp <* many parseWhiteSpace) <*> parseExpression

parseExpression :: Parser Expression
parseExpression = parseBinExpr <|> Un <$> parseUnary

parseUnary :: Parser Unary
parseUnary = Unary <$> many (many parseWhiteSpace *> parseUnOp) <*> (many parseWhiteSpace *> parseLiteral)

parseLiteral :: Parser Value
parseLiteral = parseDouble <|> parseInteger <|> parseTypedIdentifier <|> parseIdentifier