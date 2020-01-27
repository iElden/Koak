module ParserSpec
    (
        parseIdentifierTest,
        parseLiteralTest,
        parseDoubleTest,
        parseIntegerTest,
        parseCharSequenceTest,
        parseCharBlackListTest,
        parseCharTest
    ) where

import Test.Hspec
import Parser
import AST


parseIdentifierTest :: Spec
parseIdentifierTest = describe "parseIdentifierTest (UT)" $ do
    it "parse nothing" $
        runParser parseIdentifier [] `shouldBe` Nothing
    it "parse not identifier" $
        runParser parseIdentifier "0azeza" `shouldBe` Nothing
    it "parse identifier" $
        runParser parseIdentifier "Add" `shouldBe` Just ("Add", [])
    it "parse identifier + rest" $
        runParser parseIdentifier "A0tz3 76" `shouldBe` Just ("A0tz3", " 76")
    it "parse space + identifier" $
        runParser parseIdentifier " Adding" `shouldBe` Nothing

parseLiteralTest :: Spec
parseLiteralTest = describe "parseLiteralTest (FT)" $ do
    it "Not a number" $
        runParser parseLiteral "oui" `shouldBe` Nothing
    it "integer" $
        runParser parseLiteral "1" `shouldBe` Just (Nbr 1, [])
    it "Pattern : ." $
        runParser parseLiteral "." `shouldBe` Just (RealNbr 0.0, [])
    it "Pattern : N." $
        runParser parseLiteral "1." `shouldBe` Just (RealNbr 1.0, [])
    it "Pattern : .N" $
        runParser parseLiteral ".1" `shouldBe` Just (RealNbr 0.1, [])
    it "Pattern : N.N" $
        runParser parseLiteral "1.1" `shouldBe` Just (RealNbr 1.1, [])
    it "Number + rest" $
        runParser parseLiteral "4.76 3" `shouldBe` Just (RealNbr 4.76, " 3")
    it "nothing" $
        runParser parseLiteral [] `shouldBe` Nothing

parseDoubleTest :: Spec
parseDoubleTest = describe "parseDoubleTest (UT)" $ do
    it "Not a number" $
        runParser parseDouble "oui" `shouldBe` Nothing
    it "Pattern : N.X" $
        runParser parseDouble "7824.a" `shouldBe` Just (RealNbr 7824.0, "a")
    it "Pattern : ." $
        runParser parseDouble "." `shouldBe` Just (RealNbr 0.0, [])
    it "Pattern : N." $
        runParser parseDouble "1." `shouldBe` Just (RealNbr 1.0, [])
    it "Pattern : .N" $
        runParser parseDouble ".1" `shouldBe` Just (RealNbr 0.1, [])
    it "Pattern : N.N" $
        runParser parseDouble "1.1" `shouldBe` Just (RealNbr 1.1, [])
    it "Number + rest" $
        runParser parseDouble "4.76 3" `shouldBe` Just (RealNbr 4.76, " 3")
    it "nothing" $
        runParser parseDouble [] `shouldBe` Nothing

parseIntegerTest :: Spec
parseIntegerTest = describe "parseIntegerTest (UT)" $ do
    it "Not a Integer" $
        runParser parseInteger "oui" `shouldBe` Nothing
    it "integer" $
        runParser parseInteger "1" `shouldBe` Just (Nbr 1, [])
    it "integer 23" $
        runParser parseInteger "23" `shouldBe` Just (Nbr 23, [])
    it "seems integer" $
        runParser parseInteger "5489a" `shouldBe` Just (Nbr 5489, "a")
    it "Pattern : ." $
        runParser parseInteger "." `shouldBe` Nothing
    it "Pattern : N." $
        runParser parseInteger "1." `shouldBe` Just (Nbr 1, ".")
    it "Pattern : .N" $
        runParser parseInteger ".1" `shouldBe` Nothing
    it "Pattern : N.N" $
        runParser parseInteger "1.1" `shouldBe` Just (Nbr 1, ".1")
    it "Integer + rest" $
        runParser parseInteger "4.76 3" `shouldBe` Just (Nbr 4, ".76 3")
    it "nothing" $
        runParser parseInteger [] `shouldBe` Nothing

parseCharSequenceTest :: Spec
parseCharSequenceTest = describe "parseCharSequenceTest (UT)" $ do
    it "first patt" $
        runParser (parseCharSequence "add") "add t" `shouldBe` Just ("add", " t")
    it "choked patt" $
        runParser (parseCharSequence "add") "ad d" `shouldBe` Nothing
    it "nothing nothing" $
        runParser (parseCharSequence []) [] `shouldBe` Just ([], [])
    it "second patt" $
        runParser (parseCharSequence "add") "address" `shouldBe` Just ("add", "ress")

parseCharBlackListTest :: Spec
parseCharBlackListTest = describe "parseCharBlackListTest (UT)" $ do
    it "nothing blacklisted" $
        runParser (parseCharBlackList []) "add 2 3 4" `shouldBe` Just ('a', "dd 2 3 4")
    it "blacklist numbers" $
        runParser (parseCharBlackList (['0'..'9'] ++ [' '])) "add 2 3 4" `shouldBe` Just ('a', "dd 2 3 4")
    it "blacklist things but parsing nothing" $
        runParser (parseCharBlackList (['0'..'9'] ++ [' '])) [] `shouldBe` Nothing
    it "blacklist first character" $
        runParser (parseCharBlackList (['0'..'9'] ++ [' '])) "1231 a" `shouldBe` Nothing

parseCharTest :: Spec
parseCharTest = describe "parseCharTest (UT)" $ do
    it "nothing and nothing" $
        runParser (parseChar []) [] `shouldBe` Nothing
    it "nothing to parse" $
        runParser (parseChar "ade") [] `shouldBe` Nothing
    it "nothing on parameter" $
        runParser (parseChar []) "ade" `shouldBe` Nothing
    it "something wrong to parse" $
        runParser (parseChar "ade") "bte" `shouldBe` Nothing
    it "something correct to parse" $
        runParser (parseChar "ade") "dae" `shouldBe` Just ('d', "ae")