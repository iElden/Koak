module AST (
    Type (..),
    BinaryOp (..),
    UnaryOp (..),
    Value (..),
    Unary (..),
    FunctionPrototype (..),
    FunctionDeclaration (..),
    Expression (..),
    VarScope (..),
    dispList
    ) where


import Data.List

dispList :: Show a => String -> [a] -> String
dispList sep list = intercalate sep $ fmap show list


data Type =
    Void |
    IntegerVar |
    FloatingVar |
    UnknownType String |
    Function FunctionPrototype
    deriving Eq

instance Show Type where
    show Void = "void"
    show IntegerVar = "int"
    show FloatingVar = "double"
    show (UnknownType s) = s
    show (Function proto) = show proto


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
    deriving Eq

instance Show BinaryOp where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show And = "&"
    show Or = "|"
    show Xor = "^"
    show Pow = "**"
    show Mod = "%"
    show RSh = ">>"
    show LSh = "<<"
    show Equ = "=="
    show Neq = "!="
    show Gt = ">"
    show Gte = ">="
    show Lt = "<"
    show Lte = "<="
    show Asg = "="


data UnaryOp =
    BinNot  |
    BoolNot |
    Minus   |
    Plus
    deriving Eq

instance Show UnaryOp where
    show BinNot = "~"
    show BoolNot = "!"
    show Minus = "-"
    show Plus = "+"


data VarScope =
    Local |
    Global
    deriving Eq

instance Show VarScope where
    show Global = "global"
    show Local  = "local"

data Value =
    Nbr Int |
    RealNbr Double |
    GlobVar String |
    Var VarScope String Type |
    GlobCall String [Expression] |
    Call FunctionPrototype [Expression]
    deriving Eq

instance Show Value where
    show (Nbr n) = show n
    show (RealNbr n) = show n
    show (GlobVar n) = '@':n
    show (Var scope n t) = show scope ++ " " ++ n ++ ": " ++ show t
    show (GlobCall n args) = '@':n ++ "(" ++ dispList ", " args ++ ")"
    show (Call (Proto name _ _) args) = name ++ "(" ++ dispList ", " args ++ ")"


data Unary = Unary [UnaryOp] Value
    deriving Eq

instance Show Unary where
    show (Unary ops v) = dispList "" ops ++ show v


data FunctionPrototype =
    Proto String [(String, Type)] Type
    deriving Eq

instance Show FunctionPrototype where
    show (Proto name args retType) = name ++ "(" ++ (intercalate ", " $ fmap (\(name, t) -> name ++ ": " ++ show t) args) ++ "): " ++ show retType


data FunctionDeclaration =
    Decl FunctionPrototype [Expression]
    deriving Eq

instance Show FunctionDeclaration where
    show (Decl proto exprs) = "def " ++ show proto ++ " {\n" ++ dispList "\n" exprs ++ "\n}"


data Expression =
    ExtVar (String, Type) |
    ExtFct FunctionPrototype |
    Fct FunctionDeclaration |
    Expr Unary BinaryOp Expression |
    IfExpr Expression [Expression] |
    WhileExpr Expression [Expression] |
    Un Unary
    deriving Eq

instance Show Expression where
    show (Un unary) = show unary
    show (Expr unary op expr) = show unary ++ " " ++ show op ++ " " ++ show expr
    show (Fct fct) = show fct
    show (ExtVar (name, t)) = "extern " ++ name ++ ": " ++ show t
    show (ExtFct val) = "extern " ++ show val