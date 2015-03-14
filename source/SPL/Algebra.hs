module SPL.Algebra where

data Stmt =
    Stmts [Stmt]
    | VarDecl Type String Exp
    | FunDecl Type String [(Type, String)] [Stmt]
    | FunCall String [Exp]
    | Return (Maybe Exp)
    | Assign String [Field] Exp
    | If Exp Stmt (Maybe Stmt)
    | While Exp Stmt
    deriving (Eq, Show)

data Type =
    TPoly String
    | TInt
    | TBool
    | TChar
    | TTuple Type Type
    | TList Type
    | TVoid
    deriving (Eq, Show)

data Field =
    Head
    | Tail
    | First
    | Second
    deriving (Eq, Show)

data Op1 =
    ONot
    | ONeg
    deriving (Eq, Show)

data Op2 =
    OCons
    | OAnd
    | OOr
    | OEq
    | ONeq
    | OLt
    | OGt
    | OLe
    | OGe
    | OPlus
    | OMinus
    | OTimes
    | ODiv
    | OMod
    deriving (Eq, Show)

data Exp =
    EInt Int
    | EBool Bool
    | EChar Char
    | ENil
    | ETuple Exp Exp
    | EId String [Field]
    | EFunCall String [Exp]
    | EOp1 Op1 Exp
    | EOp2 Op2 Exp Exp
    deriving (Eq, Show)
