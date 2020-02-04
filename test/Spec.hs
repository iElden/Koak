import Test.Hspec
import ParserSpec
import ASTSpec

main :: IO ()
main = do
    astTests
    parsingTests

astTests :: IO ()
astTests = hspec $ do
    expressionShowTest
    functionDeclarationTest
    functionPrototypeTest
    unaryTest
    valueTest
    unaryOpTest
    binaryOpTest
    typeTest
    dispListTest

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