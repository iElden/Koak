module ParserSpec
    (
        parseCharTest,
        parseCharBlackListTest,
        parseCharSequenceTest,
        parseStringTest,
        parseDigitTest,
        parseEOFTest,
        parseAlphaTest,
        parseAlphaNumTest,
        parseWhiteSpaceTest,
        parseIntegerTest,
        parseDoubleTest,
        parseIdentifierTest,
        parseTypedIdentifierTest,
        parseBinOpTest,
        parseUnOpTest,
        parseBinExprTest,
        parseUnaryTest,
        parseExpressionTest,
        parseLiteralTest,
        parseFileTest
    ) where

import Test.Hspec
import TypeInfer
import Parser
import AST

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
    it "exotic chars" $
        runParser (parseChar "\x01\x02\x03\x04") "\x03\x30\x01" `shouldBe` Just ('\x03', "\x30\x01")

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

parseStringTest :: Spec
parseStringTest = describe "parseStringTest (FT)" $ do
    it "nothing" $
        runParser (parseString []) [] `shouldBe` Nothing
    it "nothing to parse" $
        runParser (parseString ["add", "sub", "mult"]) [] `shouldBe` Nothing
    it "things to parse correctly" $
        runParser (parseString ["add", "sub", "mult"]) "subing 2 43" `shouldBe` Just ("sub", "ing 2 43")
    it "things to parse correctly 2" $
        runParser (parseString ["tt", "vv", "cc"]) "cc c moi, vv" `shouldBe` Just ("cc", " c moi, vv")

parseDigitTest :: Spec
parseDigitTest = describe "parseDigitTest (UT)" $ do
    it "nothing" $
        runParser parseDigit [] `shouldBe` Nothing
    it "digit" $
        runParser parseDigit "0" `shouldBe` Just ('0', [])
    it "not a digit" $
        runParser parseDigit "a0" `shouldBe` Nothing

parseEOFTest :: Spec
parseEOFTest = describe "parseEOFTest (UT)" $ do
    it "nothing" $
        runParser parseEOF [] `shouldBe` Just ((), [])
    it "something" $
        runParser parseEOF " " `shouldBe` Nothing

parseAlphaTest :: Spec
parseAlphaTest = describe "parseAlphaTest (UT)" $ do
    it "nothing" $
        runParser parseAlpha [] `shouldBe` Nothing
    it "parse alpha" $
        runParser parseAlpha "a" `shouldBe` Just ('a', [])
    it "parse not alpha" $
        runParser parseAlpha "8" `shouldBe` Nothing

parseAlphaNumTest :: Spec
parseAlphaNumTest = describe "parseAlphaNumTest (FT)" $ do
    it "nothing" $
        runParser parseAlphaNum [] `shouldBe` Nothing
    it "parse alpha" $
        runParser parseAlphaNum "a" `shouldBe` Just ('a', [])
    it "parse number" $
        runParser parseAlphaNum "0" `shouldBe` Just ('0', [])
    it "parse not alphanum" $
        runParser parseAlphaNum ":" `shouldBe` Nothing

parseWhiteSpaceTest :: Spec
parseWhiteSpaceTest = describe "parseWhiteSpaceTest (UT)" $ do
    it "nothing" $
        runParser parseWhiteSpace [] `shouldBe` Nothing
    it "parse white space" $
        runParser parseWhiteSpace " " `shouldBe` Just (' ', [])
    it "parse tab" $
        runParser parseWhiteSpace "\t" `shouldBe` Just ('\t', [])
    it "parse carriage return" $
        runParser parseWhiteSpace "\n" `shouldBe` Just ('\n', [])
    it "parse nothing matching it" $
        runParser parseWhiteSpace "a" `shouldBe` Nothing

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

parseIdentifierTest :: Spec
parseIdentifierTest = describe "parseIdentifierTest (UT)" $ do
    it "parse nothing" $
        runParser parseIdentifier [] `shouldBe` Nothing
    it "parse not identifier" $
        runParser parseIdentifier "0azeza" `shouldBe` Nothing
    it "parse identifier" $
        runParser parseIdentifier "Add" `shouldBe` Just (GlobVar "Add", [])
    it "parse identifier + rest" $
        runParser parseIdentifier "A0tz3 76" `shouldBe` Just (GlobVar "A0tz3", " 76")
    it "parse space + identifier" $
        runParser parseIdentifier " Adding" `shouldBe` Nothing

parseTypeTest :: Spec
parseTypeTest = describe "parseTypeTest (FT)" $ do
    it "parse nothing" $
        runParser parseType [] `shouldBe` Nothing
    it "parse int" $
        runParser parseType "int " `shouldBe` Just (IntegerVar, " ")
    it "parse not int" $
        runParser parseType "inting" `shouldBe` Just (UnknownType "inting", [])
    it "parse void" $
        runParser parseType "void " `shouldBe` Just (Void, " ")
    it "parse double" $
        runParser parseType "double " `shouldBe` Just (FloatingVar, " ")
    it "parse string ????" $
        runParser parseType "string " `shouldBe` Just (UnknownType "string", [])

parseTypedIdentifierTest :: Spec
parseTypedIdentifierTest = describe "parseTypedIdentifierTest (FT)" $ do
    it "parse nothing" $
        runParser parseTypedIdentifier [] `shouldBe` Nothing
    it "parses correct type" $
        runParser parseTypedIdentifier "local i:int" `shouldBe` Just (Var Local "i" IntegerVar, [])
    it "parses correct types with spaces" $
        runParser parseTypedIdentifier "global     dub    :    double  " `shouldBe` Just (Var Global "dub" FloatingVar, "  ")
    it "incorrect pattern typed identifier" $
        runParser parseTypedIdentifier "voi:" `shouldBe` Nothing
    it "incorrect pattern typed identifier 2" $
        runParser parseTypedIdentifier ": void" `shouldBe` Nothing
    it "incorrect pattern" $
        runParser parseTypedIdentifier " : " `shouldBe` Nothing

parseBinOpTest :: Spec
parseBinOpTest = describe "parseBinOpTest (UT)" $ do
    it "parse nothing" $
        runParser (parseBinOp 0)[] `shouldBe` Nothing
    it "parse add with space" $
        runParser (parseBinOp 9) "       +  " `shouldBe` Just (Add, "  ")
    it "parse add" $
        runParser (parseBinOp 9) "+" `shouldBe` Just (Add, [])
    it "parse sub" $
        runParser (parseBinOp 9) "-" `shouldBe` Just (Sub, [])
    it "parse mul" $
        runParser (parseBinOp 10) "*" `shouldBe` Just (Mul, [])
    it "parse div" $
        runParser (parseBinOp 10) "/" `shouldBe` Just (Div, [])
    it "parse and" $
        runParser (parseBinOp 5) "&" `shouldBe` Just (And, [])
    it "parse or" $
        runParser (parseBinOp 3) "|" `shouldBe` Just (Or, [])
    it "parse xor" $
        runParser (parseBinOp 4) "^" `shouldBe` Just (Xor, [])
    it "parse pow" $
        runParser (parseBinOp 11) "**" `shouldBe` Just (Pow, [])
    it "parse mod" $
        runParser (parseBinOp 10) "%" `shouldBe` Just (Mod, [])
    it "parse rsh" $
        runParser (parseBinOp 8) ">>" `shouldBe` Just (RSh, [])
    it "parse lsh" $
        runParser (parseBinOp 8) "<<" `shouldBe` Just (LSh, [])
    it "parse equ" $
        runParser (parseBinOp 6) "==" `shouldBe` Just (Equ, [])
    it "parse neq" $
        runParser (parseBinOp 6) "!=" `shouldBe` Just (Neq, [])
    it "parse gt" $
        runParser (parseBinOp 7) ">" `shouldBe` Just (Gt, [])
    it "parse gte" $
        runParser (parseBinOp 7) ">=" `shouldBe` Just (Gte, [])
    it "parse lt" $
        runParser (parseBinOp 7) "<" `shouldBe` Just (Lt, [])
    it "parse lte" $
        runParser (parseBinOp 7) "<=" `shouldBe` Just (Lte, [])
    it "parse asg" $
        runParser (parseBinOp 0) "=" `shouldBe` Just (Asg, [])
    it "parse notBin" $
        runParser (parseBinOp 0) ":" `shouldBe` Nothing

parseUnOpTest :: Spec
parseUnOpTest = describe "parseUnOpTest (UT)" $ do
    it "parse nothing" $
        runParser parseUnOp [] `shouldBe` Nothing
    it "parse plus with spaces" $
        runParser parseUnOp "    +   " `shouldBe` Just (Plus, "   ")
    it "parse plus" $
        runParser parseUnOp "+" `shouldBe` Just (Plus, [])
    it "parse minus" $
        runParser parseUnOp "-" `shouldBe` Just (Minus, [])
    it "parse BoolNot" $
        runParser parseUnOp "!" `shouldBe` Just (BoolNot, [])
    it "parse BinNot" $
        runParser parseUnOp "~" `shouldBe` Just (BinNot, [])
    it "parse not unary" $
        runParser parseUnOp ">" `shouldBe` Nothing

parseBinExprTest :: Spec
parseBinExprTest = describe "parseBinExprTest (FT)" $ do
    it "parse nothing" $
        runParser (parseBinExpr 0) [] `shouldBe` Nothing
    it "parse correct binary" $
        runParser (parseBinExpr 0) "3<4" `shouldBe` Just (Expr (Unary [] (Nbr 3)) Lt (Unary [] (Nbr 4)), [])
    it "parse not correct binary" $
        runParser (parseBinExpr 0) "<4" `shouldBe` Nothing
    it "parse not correct binary 2" $
        runParser (parseBinExpr 0) "4<" `shouldBe` Nothing
    it "parse t=." $
        runParser (parseBinExpr 0) "t=." `shouldBe` Just (Expr (Unary [] (GlobVar "t")) Asg (Unary [] (RealNbr 0.0)), [])


parseUnaryTest :: Spec
parseUnaryTest = describe "parseUnaryTest (FT)" $ do
    it "parse nothing" $
        runParser parseUnary [] `shouldBe` Nothing
    it "parse only a number" $
        runParser parseUnary "3" `shouldBe` Just (Unary [] (Nbr 3), [])
    it "parse unaryNumber" $
        runParser parseUnary "-3" `shouldBe` Just (Unary [Minus] (Nbr 3), [])
    it "parse more unaries with a number" $
        runParser parseUnary "-+-+-+-3" `shouldBe` Just (Unary [Minus, Plus, Minus, Plus, Minus, Plus, Minus] (Nbr 3), [])
    it "only unaries" $
        runParser parseUnary "~~~~++" `shouldBe` Nothing
    it "wrong value" $
        runParser parseUnary "!:" `shouldBe` Nothing
    it "identifier typed but actually failed" $
        runParser parseUnary "!t:" `shouldBe` Just (Unary [BoolNot] (GlobVar "t"), ":")
    it "typed identifiers test" $
        runParser parseUnary "!global t:int" `shouldBe` Just (Unary [BoolNot] (Var Global "t" IntegerVar), [])
    it "NOT ." $
        runParser parseUnary "!." `shouldBe` Just (Unary [BoolNot] (RealNbr 0.0), [])

parseExpressionTest :: Spec
parseExpressionTest = describe "parseExpressionTest (FT)" $ do
    it "parse nothing" $
        runParser parseExpression [] `shouldBe` Nothing
    it "parse number" $
        runParser parseExpression "4" `shouldBe` Just ((Unary [] (Nbr 4)), [])
    it "parse expr" $
        runParser parseExpression "4+5.0" `shouldBe` Just (Expr (Unary [] (Nbr 4)) Add ((Unary [] (RealNbr (5.0)))), [])
    it "parse wrong expr" $
        runParser parseExpression "\"" `shouldBe` Nothing

parseLiteralTest :: Spec
parseLiteralTest = describe "parseLiteralTest (FT)" $ do
    it "Not a number" $
        runParser parseLiteral "oui" `shouldBe` Just (GlobVar "oui", [])
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

parseFileTest :: Spec
parseFileTest = describe "parseFileTest (FT)" $ do
    it "nothing" $
        runParser parseFile [] `shouldBe` Nothing
    it "parse file testAssign" $
        runParser parseFile "local test: int = 0\ntest2 = 0\ntest = 1"
        `shouldBe`
        Just ([Expr (Unary [] (Var Local "test" IntegerVar)) Asg ((Unary [] (Nbr 0))),
        Expr (Unary [] (GlobVar "test2")) Asg ((Unary [] (Nbr 0))),
        Expr (Unary [] (GlobVar "test")) Asg ((Unary [] (Nbr 1)))], [])
    it "parse file testFile" $
        runParser parseFile "global test: int = 0\ntest = test + 1\nlocal test2: double\ntest = global test2: int - 5\n2 test"
        `shouldBe`
        Just ([Expr (Unary [] (Var Global "test" IntegerVar)) Asg ((Unary [] (Nbr 0))),
        Expr (Unary [] (GlobVar "test")) Asg (Expr (Unary [] (GlobVar "test")) Add ((Unary [] (Nbr 1)))),
        (Unary [] (Var Local "test2" FloatingVar)),
        Expr (Unary [] (GlobVar "test")) Asg (Expr (Unary [] (Var Global "test2" IntegerVar)) Sub ((Unary [] (Nbr 5)))),
        (Unary [] (Nbr 2)),
        (Unary [] (GlobVar "test"))], [])