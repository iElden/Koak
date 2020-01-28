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

parseDigit :: Parser Char
parseDigit = parseChar ['0'..'9']

parseAlpha :: Parser Char
parseAlpha = parseChar $ ['a'..'z'] ++ ['A'..'Z']

parseAlphaNum :: Parser Char
parseAlphaNum = parseDigit <|> parseAlpha

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

parseLiteral :: Parser Value
parseLiteral = parseDouble <|> parseInteger

parseIdentifier :: Parser String
parseIdentifier = (:) <$> parseAlpha <*> many parseAlphaNum