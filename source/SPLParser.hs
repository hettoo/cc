module SPLParser where
import SPL
import Parser
import CharParser
import Enlist
import Data.Char

pSPL :: Parser Char Stmt
pSPL = star (pDecl .*- ows) >@ Stmts

pDecl :: Parser Char Stmt
pDecl = pVarDecl \/ pFunDecl

pVarDecl :: Parser Char Stmt
pVarDecl = pType .*-*. pId .*?*. (sym '=' -*?*. pExp .*?*- sym ';') >@
    (uncurry . uncurry) VarDecl

pFunDecl :: Parser Char Stmt
pFunDecl = pRetType .*-*. pId .*?*- sym '(' .*?*. pFArgs .*?*- sym ')' .*?*-
    sym '{' .*?*. (star (pVarDecl .*- ows) .*.
        star (pStmt .*- ows) >@ uncurry (++)) .*- sym '}' >@
    (uncurry . uncurry . uncurry) FunDecl

pRetType :: Parser Char Type
pRetType = pType \</ sseq "Void" >! TVoid

pType :: Parser Char Type
pType =
    pId >@ TCustom \>/ (
        sseq "Int" >! TInt \/ sseq "Bool" >! TBool \/ sseq "Char" >! TChar \/
        sym '(' -*?*. pType .*?*- sym ',' .*?*. pType .*?*- sym ')' >@
        uncurry TTuple \/
        sym '[' -*?*. pType .*?*- sym ']' >@ TList)

pFArgs :: Parser Char [(Type, String)]
pFArgs = opt (pType .*-*. pId .*?*. star (sym ',' -*?*. pType .*-*. pId)) >@
    enlist

pStmt :: Parser Char Stmt
pStmt = sym '{' -*?*. star (pStmt .*- ows) .*- sym '}' >@ Stmts \/
    pFunCall .*?*- sym ';' >@ uncurry FunCall \/
    sseq "return" -*-*. opt pExp .*?*- sym ';' >@ Return \/
    pId .*?*. pField .*?*. (sym '=' -*?*. pExp .*?*- sym ';') >@
        (uncurry . uncurry) Assign \/
    sseq "if" -*?*. (sym '(' -*?*. pExp .*?*- sym ')') .*?*.
        pStmt .*?*. opt (sseq "else" -*?*. pStmt) >@ (uncurry . uncurry) If \/
    sseq "while" -*?*. (sym '(' -*?*. pExp .*?*- sym ')') .*?*. pStmt >@
    uncurry While

pExp :: Parser Char Exp
pExp = pOpExp \/ pNonOpExp

pOpExp :: Parser Char Exp
pOpExp =
    pOpExp1 .*?*. opt (sym ':' -*?*. pOpExp) >@
    \(a, m) -> case m of
        Nothing -> a
        Just b -> ECons a b

(.<<) :: Parser Char v -> Parser Char (v -> v) -> Parser Char v
(.<<) p q = p .*. star (ows -*. q) >@ uncurry (foldl (\a f -> f a))
infixl 4 .<<

pOpExp1 :: Parser Char Exp
pOpExp1 = pOpExp2 .<<
    sseq "&&" -*?*. pOpExp2 >@ EAnd \/
    sseq "||" -*?*. pOpExp2 >@ EOr

pOpExp2 :: Parser Char Exp
pOpExp2 = pOpExp3 .<<
    sseq "==" -*?*. pOpExp3 >@ EEq \/
    sseq "!=" -*?*. pOpExp3 >@ ENeq

pOpExp3 :: Parser Char Exp
pOpExp3 = pOpExp4 .<<
    sym '<' -*?*. pOpExp4 >@ ELt \/
    sym '>' -*?*. pOpExp4 >@ EGt \/
    sseq "<=" -*?*. pOpExp4 >@ ELe \/
    sseq ">=" -*?*. pOpExp4 >@ EGe

pOpExp4 :: Parser Char Exp
pOpExp4 = pOpExp5 .<<
    sym '+' -*?*. pOpExp5 >@ EPlus \/
    sym '-' -*?*. pOpExp5 >@ EMinus

pOpExp5 :: Parser Char Exp
pOpExp5 = pOpExp6 .<<
    sym '*' -*?*. pOpExp6 >@ ETimes \/
    sym '/' -*?*. pOpExp6 >@ EDiv \/
    sym '%' -*?*. pOpExp6 >@ EMod

pOpExp6 :: Parser Char Exp
pOpExp6 =
    sym '!' -*?*. pOpExp6 >@ ENot \/
    sym '-' -*?*. pOpExp6 >@ ENeg \/
    pNonOpExp

pNonOpExp :: Parser Char Exp
pNonOpExp =
    pInt >@ EInt \/ pBool >@ EBool \/ pChar >@ EChar \/ sseq "[]" >! ENil \/
    sym '(' -*?*. pExp .*?*- sym ',' .*?*. pExp .*?*- sym ')' >@
    uncurry ETuple \/
    pId .*?*. pField >@ uncurry EId \/ pFunCall >@ uncurry EFunCall \/
    sym '(' -*?*. pExp .*?*- sym ')'

pField :: Parser Char [Field]
pField = sym '.' -*?*. (
    sseq "hd" >! Head \/
    sseq "tl" >! Tail \/
    sseq "fst" >! First \/
    sseq "snd" >! Second) .*. opt (ows -*. pField) >@ enlist

pFunCall :: Parser Char (String, [Exp])
pFunCall = pId .*?*. (sym '(' -*?*. star (pExp .*- ows) .*- sym ')')

pInt :: Parser Char Int
pInt = opt (sym '-') .*?*. plus (satisfy isDigit) >@ read . enlist

pBool :: Parser Char Bool
pBool = sseq "False" >! False \/ sseq "True" >! True

pChar :: Parser Char Char
pChar = sym '\'' -*. anything .*- sym '\''

pId :: Parser Char String
pId = satisfy isAlpha .*. star (sym '_' \/ satisfy isAlphaNum) >@ enlist
