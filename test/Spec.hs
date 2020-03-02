import Test.Hspec
import TypeInferSpec
import ParserSpec
import ASTSpec

main :: IO ()
main = do
    astTests
    parsingTests
    typeInferTests

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

typeInferTests :: IO ()
typeInferTests = hspec $ do
    notImplementedTest
    getExprTest
    castErrorTest
    varNotFoundTest
    noEffectTest
    isCastValidTest
    findVarTypeTest
    checkExpressionTestUnaries
    checkExpressionTestExpressions