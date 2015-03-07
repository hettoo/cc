module SPLParser where
import Parser
import Enlist
import Data.Char

data Type =
    TCustom String
    | TInt
    | TBool
    | TChar
    | TTuple Type Type
    | TList Type

data RetType =
    Type Type
    | Void

data Field =
    Head
    | Tail
    | First
    | Second

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

-- pSPL

-- pDecl

-- pVarDecl

-- pFunDecl

pRetType :: Parser Char RetType
pRetType = pType >@ Type \</ sseq "Void" >! Void

pType :: Parser Char Type
pType =
    pId >@ TCustom \>/ (
        sseq "Int" >! TInt \/ sseq "Bool" >! TBool \/ sseq "Char" >! TChar \/
        sym '(' -*. pType .*- sym ',' .*. pType .*- sym ')' >@ uncurry TTuple \/
        sym '[' -*. pType .*- sym ']' >@ TList)

pFArgs :: Parser Char [(Type, String)]
pFArgs = opt (pType .*. pId .*. star (sym ',' -*. pType .*. pId)) >@ enlist

-- pStmt

pExp :: Parser Char Exp
pExp = pOpExp \/ pNonOpExp

pOpExp :: Parser Char Exp
pOpExp =
    pOpExp1 .*. opt (sym ':' -*. pOpExp) >@
    \(a, m) -> case m of
        Nothing -> a
        Just b -> ECons a b

(.<<) :: Parser a v -> Parser a (v -> v) -> Parser a v
(.<<) p q = p .*. star q >@ uncurry (foldl (\a f -> f a))
infixl 4 .<<

pOpExp1 :: Parser Char Exp
pOpExp1 = pOpExp2 .<<
    sseq "&&" -*. pOpExp2 >@ EAnd \/
    sseq "||" -*. pOpExp2 >@ EOr

pOpExp2 :: Parser Char Exp
pOpExp2 = pOpExp3 .<<
    sseq "==" -*. pOpExp3 >@ EEq \/
    sseq "!=" -*. pOpExp3 >@ ENeq

pOpExp3 :: Parser Char Exp
pOpExp3 = pOpExp4 .<<
    sym '<' -*. pOpExp4 >@ ELt \/
    sym '>' -*. pOpExp4 >@ EGt \/
    sseq "<=" -*. pOpExp4 >@ ELe \/
    sseq ">=" -*. pOpExp4 >@ EGe

pOpExp4 :: Parser Char Exp
pOpExp4 = pOpExp5 .<<
    sym '+' -*. pOpExp5 >@ EPlus \/
    sym '-' -*. pOpExp5 >@ EMinus

pOpExp5 :: Parser Char Exp
pOpExp5 = pOpExp6 .<<
    sym '*' -*. pOpExp6 >@ ETimes \/
    sym '/' -*. pOpExp6 >@ EDiv \/
    sym '%' -*. pOpExp6 >@ EMod

pOpExp6 :: Parser Char Exp
pOpExp6 =
    sym '!' -*. pOpExp6 >@ ENot \/
    sym '-' -*. pOpExp6 >@ ENeg \/
    pNonOpExp

pNonOpExp :: Parser Char Exp
pNonOpExp =
    pInt >@ EInt \/ pBool >@ EBool \/ pChar >@ EChar \/ sseq "[]" >! ENil \/
    sym '(' -*. pExp .*- sym ',' .*. pExp .*- sym ')' >@ uncurry ETuple \/
    pId .*. pField >@ uncurry EId \/ pFunCall >@ uncurry EFunCall \/
    sym '(' -*. pExp .*- sym ')'

pField :: Parser Char [Field]
pField = sym '.' -*. (
    sseq "hd" >! Head \/
    sseq "tl" >! Tail \/
    sseq "fst" >! First \/
    sseq "snd" >! Second) .*. opt pField >@ enlist

pFunCall :: Parser Char (String, [Exp])
pFunCall = pId .*. (sym '(' -*. star pExp .*- sym ')')

pInt :: Parser Char Int
pInt = opt (sym '-') .*. plus (satisfy isDigit) >@ read . enlist

pBool :: Parser Char Bool
pBool = sseq "False" >! False \/ sseq "True" >! True

pChar :: Parser Char Char
pChar = sym '\'' -*. anything .*- sym '\''

pId :: Parser Char String
pId = satisfy isAlpha .*. star (sym '_' \/ satisfy isAlphaNum) >@ enlist
