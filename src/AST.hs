module AST (
    Type (..),
    BinaryOp (..),
    UnaryOp (..),
    Value (..),
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
    BooleanVar |
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
    BAnd|
    BOr |
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
    Boolean Bool |
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
    show (Boolean True) = "true"
    show (Boolean False) = "false"
    show (Var scope n t) = show scope ++ " " ++ n ++ ": " ++ show t
    show (GlobCall n args) = '@':n ++ "(" ++ dispList " " args ++ ")"
    show (Call (Proto name _ _) args) = name ++ "(" ++ dispList " " args ++ ")"


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
    Fct FunctionDeclaration |
    Expr Expression BinaryOp Expression |
    IfExpr Expression [Expression] (Maybe [Expression]) |
    WhileExpr Expression [Expression] |
    Unary [UnaryOp] Value |
    Cast Type Expression |
    Extern String Type
    deriving Eq

instance Show Expression where
    show (IfExpr cond ifExprs Nothing) = "if (" ++ show cond ++ ") {\n" ++ dispList "\n" ifExprs ++ "\n}"
    show (IfExpr cond ifExprs (Just elseExprs)) = "if (" ++ show cond ++ ") {\n" ++ dispList "\n" ifExprs ++ "\n} else {\n" ++ dispList "\n" elseExprs ++ "\n}"
    show (WhileExpr cond whileExprs) = "while (" ++ show cond ++ ") {\n" ++ dispList "\n" whileExprs ++ "\n}"
    show (Expr unary op expr) = "(" ++ show unary ++ ") " ++ show op ++ " (" ++ show expr ++ ")"
    show (Fct fct) = show fct
    show (Unary ops v) = dispList "" ops ++ show v
    show (Cast t e) = "cast<" ++ show t ++ ">(" ++ show e ++ ")"
    show (Extern name t) = "extern " ++ name ++ ": " ++ show t