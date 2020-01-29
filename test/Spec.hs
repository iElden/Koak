import Test.Hspec
import ParserSpec

main :: IO ()
main = do
    parsingTests

parsingTests :: IO ()
parsingTests = hspec $ do
    parseCharTest
    parseCharBlackListTest
    parseCharSequenceTest
    parseStringTest
    parseDigitTest
    parseEOFTest
    parseAlphaTest
    parseAlphaNumTest
    parseWhiteSpaceTest
    parseIntegerTest
    parseDoubleTest
    parseIdentifierTest
    parseTypedIdentifierTest
    parseBinOpTest
    parseUnOpTest
    parseBinExprTest
    parseUnaryTest
    parseExpressionTest
    parseLiteralTest
    parseFileTest