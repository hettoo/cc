module SPL where

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

data Exp =
    EInt Int
    | EBool Bool
    | EChar Char
    | ENil
    | ECons Exp Exp
    | ETuple Exp Exp
    | EId String [Field]
    | EFunCall String [Exp]
    | EAnd Exp Exp
    | EOr Exp Exp
    | EEq Exp Exp
    | ENeq Exp Exp
    | ELt Exp Exp
    | EGt Exp Exp
    | ELe Exp Exp
    | EGe Exp Exp
    | EPlus Exp Exp
    | EMinus Exp Exp
    | ETimes Exp Exp
    | EDiv Exp Exp
    | EMod Exp Exp
    | ENot Exp
    | ENeg Exp
    deriving (Eq, Show)
