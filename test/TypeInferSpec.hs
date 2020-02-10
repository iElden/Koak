module TypeInferSpec (
        notImplementedTest,
        getExprTest,
        castErrorTest,
        varNotFoundTest,
        noEffectTest,
        isCastValidTest,
        findVarTypeTest,
        --checkExpressionTest,
    ) where

import TypeInfer
import AST
import Test.Hspec

notImplementedTest :: Spec
notImplementedTest = describe "notImplementedTest (UT)" $ do
    it "not implemented with nothing" $
        notImplemented [] `shouldBe` (([Error "Not implemented"], Nothing), [])
    it "not implemented with something" $
        notImplemented [("trops : int", IntegerVar)] `shouldBe` (([Error "Not implemented"], Nothing), [("trops : int", IntegerVar)])

getExprTest :: Spec
getExprTest = describe "getExprTest (UT)" $ do
    it "expr 4+5.0" $
        getExpr (Expr (Unary [] (Nbr 4)) Add (Un (Unary [] (RealNbr (5.0)))))
        `shouldBe` (Info "In expression \'4 + 5.0\'\n")

castErrorTest :: Spec
castErrorTest = describe "castErrorTest (UT)" $ do
    it "cast from int to unknown type string" $
        castError "ntv" IntegerVar (UnknownType "string") (Expr (Unary [] (Var "ntv" (UnknownType "string"))) Asg (Un (Unary [] (Nbr 5))))
        `shouldBe` [Error "Cannot cast variable ntv from int to string", Info "In expression \'ntv: string = 5\'\n"]

varNotFoundTest :: Spec
varNotFoundTest = describe "varNotFoundTest (UT)" $ do
    it "var not found" $
        varNotFound "ntv" (Expr (Unary [] (GlobVar "ntv")) Asg (Expr (Unary [] (GlobVar "ntv")) Add (Un (Unary [] (Nbr 5))))) `shouldBe`
        [Error "Use of undeclared identifier ntv", Info "In expression \'@ntv = @ntv + 5\'\n"]

noEffectTest :: Spec
noEffectTest = describe "noEffectTest (UT)" $ do
    it "no effect found" $
        noEffect (Expr (Unary [] (GlobVar "ntv")) Asg (Expr (Unary [] (GlobVar "ntv")) Add (Un (Unary [] (Nbr 0))))) `shouldBe`
        [Warning "This statement has no effect", Info "In expression \'@ntv = @ntv + 0\'\n"]
    it "no effect found 2" $
        noEffect (Un (Unary [] (Var "ntv" IntegerVar))) `shouldBe` [Warning "This statement has no effect", Info "In expression \'ntv: int\'\n"]

isCastValidTest :: Spec
isCastValidTest = describe "isCastValidTest (UT)(NI)" $ do
    it "isCastValid int to float" $
        isCastValid IntegerVar FloatingVar `shouldBe` False
    it "isCastValid float to int" $
        isCastValid FloatingVar IntegerVar `shouldBe` False
    it "isCastValid void to int" $
        isCastValid Void IntegerVar `shouldBe` False
    it "isCastValid int to void" $
        isCastValid IntegerVar Void `shouldBe` False
    it "isCastValid void to float" $
        isCastValid Void FloatingVar `shouldBe` False
    it "isCastValid float to void" $
        isCastValid FloatingVar Void `shouldBe` False

findVarTypeTest :: Spec
findVarTypeTest = describe "findVarTypeTest (UT)" $ do
    it "nothing" $
        findVarType [] [] `shouldBe` Nothing
    it "type found" $
        findVarType [("str", UnknownType "string"), ("i", IntegerVar), ("flt", FloatingVar), ("food", Void)] "i" `shouldBe`
        (Just IntegerVar)
    it "type not found" $
        findVarType [("str", UnknownType "string"), ("i", IntegerVar), ("flt", FloatingVar), ("food", Void)] "zoon" `shouldBe`
        Nothing

--checkExpressionTest :: Spec
--checkExpressionTest = describe "checkExpressionTest (FT)" $ do
--    it "" $