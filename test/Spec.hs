import Test.Hspec
import ParserSpec

main :: IO ()
main = hspec $ do
    parseIdentifierTest
    parseLiteralTest
    parseDoubleTest
    parseIntegerTest
    parseCharSequenceTest
    parseCharBlackListTest
    parseCharTest

