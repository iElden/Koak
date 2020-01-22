module AST (
    ) where

type Identifier = String
type CallExpr = [Expression]
type PostFix =
    (Primary, Maybe CallExpr)
type Unary =
    (UnaryOp, Either Unary PostFix)
type Expression =
    (Unary, [(BinaryOp, Either Unary Expression)])
type FctParam =
    (Identifier, Type)
type FctProtoArgs =
    ([FctParam], Type)
type Function =
    (FctProto, FctProtoArgs, Expressions)
type ForExpr =
    (Identifier, Expression, Identifier, Expression, Expression, Expressions)
type IfExpr =
    (Expression, Expressions, Maybe Expressions)
type WhileExpr =
    (Expression, Expressions)

data BinaryOp =
    Add |
    Sub |
    Mul |
    Div |
    And |
    Or  |
    Xor |
    Pow |
    Mod |
    RSh |
    LSh |
    Equ |
    Neq |
    Gt  |
    Gte |
    Lt  |
    Lte |
    Asg |

data Literal =
    Nbr Int |
    RealNbr Double

data Primary =
    Var Identifier |
    Lit Literal |
    Ex Expressions

data FctProto =
    Un Int |
    Bin Int |
    Iden Identifier

data UnaryOp =
    BinNot  |
    BoolNot |
    Minus   |
    Plus    |

data Expression =
    For ForExpr |
    If IfExpr |
    While WhileExpr |
    Expr Expression [Expression]

data KoakAST =
    Fct Function |
    Expr Expression

