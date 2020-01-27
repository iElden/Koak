module Parser(
    Parser (..),
    runParser,
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

myOptional :: Parser a -> Parser [a]
myOptional f = Parse (\s -> do
    (v, remain) <- runParser (optional f) s
    return (maybeToList v, remain))

combineParsers :: [Parser a] -> Parser [a]
combineParsers parsers = Parse (\s -> case go parsers s of
    Just ([], _) -> Nothing
    val -> val)
    where
        go [] s = return ([], s)
        go ((Parse parser):others) s = do
            (v, remain) <- parser s
            (val, remain) <- go others remain
            return ([v] ++ val, remain)

combineResults :: Parser [[a]] -> Parser [a]
combineResults = fmap $ foldl (++) []

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
parseCharSequence str = combineParsers $ createParsers str
    where
        createParsers "" = []
        createParsers (c:str) = [parseChar [c]] ++ (createParsers str)

parseNumber :: Parser Literal
parseNumber = Parse $ \s -> do
    (nbr, remain) <- runParser (many $ parseChar ['0'..'9']) s
    case runParser (parseCharBlackList []) remain of
        Just ('.', _) -> Nothing
        _ -> do
            nbr <- readMaybe nbr :: Maybe Int
            return (Nbr nbr, remain)

parseDouble :: Parser Literal
parseDouble = Parse $ \s -> do
    (nbr, remain) <- runParser (
        combineParsers (
            [some $ parseChar ['0'..'9'], parseCharSequence ".",  many $ parseChar ['0'..'9']]
        ) <|> combineParsers (
            [parseCharSequence ".",  some $ parseChar ['0'..'9']]
        )
        ) s
    nbr <- readMaybe $ "0" ++ (foldl (++) "" nbr) ++ "0" :: Maybe Double
    return (RealNbr nbr, remain)

parseRealNumber :: Parser Literal
parseRealNumber = Parse $ \s -> do
    runParser (parseNumber <|> parseDouble) s

parseIdentifier :: Parser String
parseIdentifier = Parse $ \s -> do
    (c, remain) <- runParser (parseChar (['a'..'z'] ++ ['A'..'Z'])) s
    (str, remain2) <- runParser (many $ parseChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])) remain
    return (c:str, remain2)