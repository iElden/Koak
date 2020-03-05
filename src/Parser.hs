module Parser(
    Parser (..),
    runParser,
    parseChar,
    parseCharBlackList,
    parseCharSequence,
    parseInteger,
    parseDouble,
    parseLiteral,
    parseIdentifier,
    parseExpression,
    parseFile,
    parseEOF,
    parseString,
    parseDigit,
    parseAlpha,
    parseAlphaNum,
    parseWhiteSpace,
    parseType,
    parseTypedIdentifier,
    parseBinOp,
    parseUnary,
    parseUnOp,
    parseBinExpr
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

parseEOF :: Parser ()
parseEOF = Parse $ \s -> case s of
    "" -> return ((), s)
    _ -> Nothing

parseAlpha :: Parser Char
parseAlpha = parseChar $ ['a'..'z'] ++ ['A'..'Z']

parseAlphaNum :: Parser Char
parseAlphaNum = parseDigit <|> parseAlpha

parseSoftSeparator :: Parser Char
parseSoftSeparator = parseChar " \t"

parseWhiteSpace :: Parser Char
parseWhiteSpace = parseSoftSeparator <|> parseChar "\n"

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

parseFunctionType :: Parser Type
parseFunctionType = Function <$> (Proto "" <$> (parseChar "(" *> many ((,) <$> many parseAlpha <*> (many parseWhiteSpace *> parseChar ":" *> many parseWhiteSpace *> parseType)) <* parseChar ")") <*> (many parseWhiteSpace *> parseChar ":" *> many parseWhiteSpace *> parseType))

parseBuiltinType :: Parser Type
parseBuiltinType = do
    t <- (:) <$> parseAlpha <*> many parseAlphaNum
    case t of
        "int" -> return IntegerVar
        "void" -> return Void
        "double" -> return FloatingVar
        "" -> empty
        _ -> return $ UnknownType t

parseType :: Parser Type
parseType = parseBuiltinType <|> parseFunctionType

parseScopeIdentifier :: Parser VarScope
parseScopeIdentifier = do
    result <- parseString ["global", "local"]
    case result of
        "global" -> return Global
        "local" -> return Local

parseTypedIdentifier :: Parser Value
parseTypedIdentifier = Var <$> parseScopeIdentifier <*> (many parseWhiteSpace *> ((:) <$> parseAlpha <*> many parseAlphaNum)) <*> (many parseWhiteSpace *> parseChar ":" *> many parseWhiteSpace *> parseType)

getOperatorByPriority :: Int -> [String]
getOperatorByPriority 0 = ["="]
getOperatorByPriority 1 = ["||"]
getOperatorByPriority 2 = ["&&"]
getOperatorByPriority 3 = ["|"]
getOperatorByPriority 4 = ["^"]
getOperatorByPriority 5 = ["&"]
getOperatorByPriority 6 = ["==", "!="]
getOperatorByPriority 7 = ["<=", "<", ">=", ">"]
getOperatorByPriority 8 = [">>", "<<"]
getOperatorByPriority 9 = ["+", "-"]
getOperatorByPriority 10 = ["*", "/", "%"]
getOperatorByPriority 11 = ["**"]
getOperatorByPriority _ = []

parseBinOp :: Int -> Parser BinaryOp
parseBinOp priority = do
    test <- many parseSoftSeparator *> (parseString $ getOperatorByPriority priority)
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
        "&&" -> return BAnd
        "||" -> return BOr
        "=" -> return Asg

parseBinExpr :: Int -> Parser Expression
parseBinExpr priority
    | priority < 12 = do
        expr <- parseBinExpr (priority + 1)
        (Expr expr <$> (parseBinOp priority <* many parseSoftSeparator) <*> parseBinExpr priority) <|> pure expr
    | otherwise = parseUnary <|> (parseChar "(" *> parseBinExpr 0  <* many parseWhiteSpace <* parseChar ")")

parseUnOp :: Parser UnaryOp
parseUnOp = do
    test <- many parseWhiteSpace *> parseString ["+", "-", "!", "~"]
    case test of
        "+" -> return Plus
        "-" -> return Minus
        "!" -> return BoolNot
        "~" -> return BinNot

parseName :: Parser String
parseName = ((:) <$> parseAlpha <*> many parseAlphaNum)

parseArgument :: Parser (String, Type)
parseArgument = (,) <$> ((:) <$> parseAlpha <*> many parseAlphaNum) <*> (many parseWhiteSpace *> parseChar ":" *> many parseWhiteSpace *> parseType)

parseFunctionPrototype :: Parser FunctionPrototype
parseFunctionPrototype = Proto <$> (parseName <* many parseSoftSeparator) <*> (parseChar "(" *> many parseWhiteSpace *> many (parseArgument <* many parseWhiteSpace) <* parseChar ")") <*> (many parseWhiteSpace *> parseChar ":" *> many parseWhiteSpace *> parseType)

parseFunctionDeclaration :: Parser FunctionDeclaration
parseFunctionDeclaration = Decl <$> (parseCharSequence "def" *> many parseWhiteSpace *> parseFunctionPrototype <* many parseWhiteSpace) <*> (many parseWhiteSpace *> parseChar "{" *> many parseWhiteSpace *> many (parseExpression <* many parseWhiteSpace) <* many parseWhiteSpace <* parseChar "}")

parseFunction :: Parser Expression
parseFunction = Fct <$> parseFunctionDeclaration

parseFunctionCall :: Parser Value
parseFunctionCall = GlobCall <$> (parseName <* many parseSoftSeparator) <*> (parseChar "(" *> many parseWhiteSpace *> many (parseExpression <* many parseWhiteSpace) <* parseChar ")")

parseIf :: Parser Expression
parseIf = IfExpr <$>
    (parseCharSequence "if" *> many parseWhiteSpace *> parseChar "(" *> many parseWhiteSpace *> parseExpression <* many parseWhiteSpace <* parseChar ")") <*>
    (many parseWhiteSpace *> parseChar "{" *> many parseWhiteSpace *> many (parseExpression <* many parseWhiteSpace) <* many parseWhiteSpace <* parseChar "}") <*>
    optional (many parseWhiteSpace *> parseCharSequence "else" *> many parseWhiteSpace *> parseChar "{" *> many parseWhiteSpace *> many (parseExpression <* many parseWhiteSpace) <* many parseWhiteSpace <* parseChar "}")

parseWhile :: Parser Expression
parseWhile = WhileExpr <$>
    (parseCharSequence "while" *> many parseWhiteSpace *> parseChar "(" *> many parseWhiteSpace *> parseExpression <* many parseWhiteSpace <* parseChar ")") <*>
    (many parseWhiteSpace *> parseChar "{" *> many parseWhiteSpace *> many (parseExpression <* many parseWhiteSpace) <* many parseWhiteSpace <* parseChar "}")

parseUnary :: Parser Expression
parseUnary = Unary <$> many (many parseSoftSeparator *> parseUnOp) <*> (many parseSoftSeparator *> parseLiteral)

parseExpression :: Parser Expression
parseExpression = parseWhile <|> parseIf <|> parseFunction <|> parseBinExpr 0

parseLiteral :: Parser Value
parseLiteral = parseFunctionCall <|> parseDouble <|> parseInteger <|> parseTypedIdentifier <|> parseIdentifier

parseFile :: Parser [Expression]
parseFile = many parseWhiteSpace *> ((:) <$> parseExpression <*> many (some parseWhiteSpace *> parseExpression)) <* many parseWhiteSpace <* parseEOF