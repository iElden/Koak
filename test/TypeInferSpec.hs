module TypeInferSpec (
        notImplementedTest,
        getExprTest
    ) where

import TypeInfer
import AST
import Test.Hspec

notImplementedTest :: Spec
notImplementedTest = describe "notImplementedTest (UT)" $ do
    it "not implemented with nothing" $
        notImplemented [] `shouldBe` (([Error "Not Implemented"], Nothing), [])
    it "not implemented with something" $
        notImplemented [("trops : int", IntegerVar)] `shouldBe` (([Error "Not Implemented"], Nothing), [("trops : int", IntegerVar)])

getExprTest :: Spec
getExprTest = describe "getExprTest (UT)" $ do
    it "expr 4+5.0" $
        getExpr (Expr (Unary [] (Nbr 4)) Add (Un (Unary [] (RealNbr (5.0)))))
        `shouldBe` (Info "In expression \'\'\n")