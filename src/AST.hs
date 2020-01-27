module AST (
    ) where

type Identifier = String
type CallExpr = [Expression]
type PostFix =
    (Primary, Maybe CallExpr)
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

data Type =
    Number |
    RealNumber |
    Void
    deriving Show

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
    Asg
    deriving Show

data Expression =
    E Unary [(BinaryOp, Either Unary Expression)]
    deriving Show

data Unary =
    Una UnaryOp (Either Unary PostFix)
    deriving Show

data Literal =
    Nbr Int |
    RealNbr Double
    deriving Show

data Primary =
    Var Identifier |
    Lit Literal |
    Ex Expressions
    deriving Show

data FctProto =
    Un Int |
    Bin Int |
    Iden Identifier
    deriving Show

data UnaryOp =
    BinNot  |
    BoolNot |
    Minus   |
    Plus
    deriving Show

data Expressions =
    For ForExpr |
    If IfExpr |
    While WhileExpr |
    Expr Expression [Expression]
    deriving Show

data KoakAST =
    Fct Function |
    Exp Expression
    deriving Show

