module ASTSpec (
        expressionShowTest,
        functionDeclarationTest,
        functionPrototypeTest,
        unaryTest,
        valueTest,
        unaryOpTest,
        binaryOpTest,
        typeTest,
        dispListTest,
    ) where

import AST
import Test.Hspec

dispListTest :: Spec
dispListTest = describe "dispListTest (UT)" $ do
    it "no sep and list" $
        dispList "" "" `shouldBe` []
    it "sep and no list" $
        dispList ", " "" `shouldBe` []
    it "no sep and list" $
        dispList [] [Nbr 3, RealNbr 2.0, GlobVar "t"] `shouldBe` "32.0@t"
    it "sep and list" $
        dispList ", " [Nbr 3, RealNbr 2.0, GlobVar "t"] `shouldBe` "3, 2.0, @t"
    it "sep and list 2" $
        dispList " -> " [(Function IntegerVar [Void, FloatingVar]), (UnknownType "string")]
        `shouldBe` "function(void, double): int -> string"

typeTest :: Spec
typeTest = describe "typeTest (UT)" $ do
    it "Void" $
        show Void `shouldBe` "void"
    it "IntegerVar" $
        show IntegerVar `shouldBe` "int"
    it "FloatingVar" $
        show FloatingVar `shouldBe` "double"
    it "Unknown Type with nothing" $
        show (UnknownType []) `shouldBe` []
    it "Unknown Type with existing type" $
        show (UnknownType "string") `shouldBe` "string"
    it "Function Type Nothing" $
        show (Function IntegerVar []) `shouldBe` "function(): int"
    it "Function Type Something" $
        show (Function IntegerVar [IntegerVar, FloatingVar, Void]) `shouldBe`
        "function(int, double, void): int"
    it "Function Function Functions" $
        show (Function (Function IntegerVar [IntegerVar, FloatingVar]) [Function Void [Void, Void], FloatingVar])
        `shouldBe` "function(function(void, void): void, double): function(int, double): int"

binaryOpTest :: Spec
binaryOpTest = describe "binaryOpTest (UT)" $ do
    it "Add" $
        show Add `shouldBe` "+"
    it "Sub" $
        show Sub `shouldBe` "-"
    it "Mul" $
        show Mul `shouldBe` "*"
    it "Div" $
        show Div `shouldBe` "/"
    it "And" $
        show And `shouldBe` "&"
    it "Or" $
        show Or `shouldBe` "|"
    it "Xor" $
        show Xor `shouldBe` "^"
    it "Pow" $
        show Pow `shouldBe` "**"
    it "Mod" $
        show Mod `shouldBe` "%"
    it "RSh" $
        show RSh `shouldBe` ">>"
    it "LSh" $
        show LSh `shouldBe` "<<"
    it "Equ" $
        show Equ `shouldBe` "=="
    it "Neq" $
        show Neq `shouldBe` "!="
    it "Gt" $
        show Gt `shouldBe` ">"
    it "Gte" $
        show Gte `shouldBe` ">="
    it "Lt" $
        show Lt `shouldBe` "<"
    it "Lte" $
        show Lte `shouldBe` "<="
    it "Asg" $
        show Asg `shouldBe` "="

unaryOpTest :: Spec
unaryOpTest = describe "unaryOpTest (UT)" $ do
    it "BinNot" $
        show BinNot `shouldBe` "~"
    it "BoolNot" $
        show BoolNot `shouldBe` "!"
    it "Minus" $
        show Minus `shouldBe` "-"
    it "Plus" $
        show Plus `shouldBe` "+"

valueTest :: Spec
valueTest = describe "valueTest (UT)" $ do
    it "show nbr" $
        show (Nbr 0) `shouldBe` "0"
    it "show realnbr" $
        show (RealNbr 2.4) `shouldBe` "2.4"
    it "show globvar" $
        show (GlobVar "t") `shouldBe` "@t"
    it "show var" $
        show (Var "t" IntegerVar) `shouldBe` "t: int"
    it "show globcall" $
        show (GlobCall "fun" []) `shouldBe` "@fun()"
    it "show globcall with args" $
        show (GlobCall "t" [Nbr 4, RealNbr 3.2, GlobVar "u"]) `shouldBe` "@t(4, 3.2, @u)"
    it "show call" $
        show (Call (Proto "func" Void []) []) `shouldBe` "func()"
    it "show call with args" $
        show (Call (Proto "func" IntegerVar []) [(Nbr 3), RealNbr 2.54, Var "tru" IntegerVar]) `shouldBe`
        "func(3, 2.54, tru: int)"

unaryTest :: Spec
unaryTest = describe "unaryTest (UT)" $ do
    it "show unary" $
        show (Unary [] (Nbr 4)) `shouldBe` "4"
    it "show unary with ops" $
        show (Unary [Minus, Plus, Minus, Plus, BoolNot, BinNot] (GlobVar "glob")) `shouldBe` "-+-+!~@glob"

functionPrototypeTest :: Spec
functionPrototypeTest = describe "functionPrototypeTest (UT)" $ do
    it "show func proto void" $
        show (Proto "func" Void []) `shouldBe` "func(): void"
    it "show func proto one arg" $
        show (Proto "one" IntegerVar [("itgr", IntegerVar)]) `shouldBe` "one(itgr: int): int"
    it "show func multiple args" $
        show (Proto "multiple" FloatingVar [("itgr", IntegerVar), ("vd", Void), ("flt", FloatingVar)]) `shouldBe` "multiple(itgr: int, vd: void, flt: double): double"

functionDeclarationTest :: Spec
functionDeclarationTest = describe "funcitonDeclarationTest (UT)" $ do
    it "show fct" $
        show (Decl (Proto "func" FloatingVar [("a", IntegerVar), ("b", FloatingVar)])
        [(Expr (Unary [Minus] (Var "a" IntegerVar)) Sub (Un (Unary [] (Var "b" FloatingVar))))])
        `shouldBe` "func(a: int, b: double): double {\n-a: int - b: double}"

expressionShowTest :: Spec
expressionShowTest = describe "ExpressionShowTest (UT)" $ do
    it "show unary 5.0" $
        show (Un (Unary [] (RealNbr (5.0)))) `shouldBe` "5.0"
    it "show unary binop Exp" $
        show (Expr (Unary [] (Nbr 4)) Add (Un (Unary [Minus] (RealNbr (5.0))))) `shouldBe`
        "4 + -5.0"
    it "show fct" $
        show (Fct (Decl (Proto "Func" IntegerVar [("tr", IntegerVar)]) [])) `shouldBe`
        "Func(tr: int): int {\n}"
    it "show ExtFct" $
        show (ExtFct (Proto "Func" Void [])) `shouldBe` "extern Func(): void"
