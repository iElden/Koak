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

parseInteger :: Parser Literal
parseInteger = do
    intPart <- (readMaybe :: String -> Maybe Int) <$> (many $ parseChar ['0'..'9'])
    maybe empty (pure . Nbr) intPart

parseDouble :: Parser Literal
parseDouble = do
    nbr <- (readMaybe :: String -> Maybe Double) <$> do
        intPart <- many $ parseChar ['0'..'9']
        _ <- parseChar "."
        decPart <- many $ parseChar ['0'..'9']
        pure ("0" ++ intPart ++ "." ++ decPart ++ "0")
    maybe empty (pure . RealNbr) nbr

parseLiteral :: Parser Literal
parseLiteral = parseDouble <|> parseInteger

parseIdentifier :: Parser String
parseIdentifier =
    (:) <$> (parseChar $ ['a'..'z'] ++ ['A'..'Z']) <*> (many $ parseChar $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])