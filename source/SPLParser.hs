module SPLParser where
import SPL
import Parser
import CharParser
import Enlist
import Data.Char

pSPL :: Parser Char Stmt
pSPL = ows -*. gstar (pDecl .*- ows) >@ Stmts

pDecl :: Parser Char Stmt
pDecl = pVarDecl \/ pFunDecl

pVarDecl :: Parser Char Stmt
pVarDecl l = (pType .*-*. pId .*?*. (sym '=' -*?*. pExp .*?*- sym ';') >@
    (uncurry . uncurry) VarDecl) l

pFunDecl :: Parser Char Stmt
pFunDecl = pRetType .*-*. pId .*?*.
    (sym '(' -*?*. commaList (pType .*-*. pId) .*?*- sym ')') .*?*.
    (sym '{' -*?*. (gstar (pVarDecl .*- ows) .*.
        gstar (pStmt .*- ows) >@ uncurry (++)) .*- sym '}') >@
    (uncurry . uncurry . uncurry) FunDecl

pRetType :: Parser Char Type
pRetType = pType \</ sseq "Void" >! TVoid

pType :: Parser Char Type
pType =
    pId >@ TCustom \>/ (
        sseq "Int" >! TInt \/ sseq "Bool" >! TBool \/ sseq "Char" >! TChar \/
        sym '[' -*?*. pType .*?*- sym ']' >@ TList) \/
        sym '(' -*?*. pType .*?*- sym ',' .*?*. pType .*?*- sym ')' >@
        uncurry TTuple

pStmt :: Parser Char Stmt
pStmt = sym '{' -*?*. gstar (pStmt .*- ows) .*- sym '}' >@ Stmts \/
    pId .*?*. pField .*?*. (sym '=' -*?*. pExp .*?*- sym ';') >@
        (uncurry . uncurry) Assign \/
    pFunCall .*?*- sym ';' >@ uncurry FunCall \/
    sseq "if" -*?*. (sym '(' -*?*. pExp .*?*- sym ')') .*?*.
        pStmt .*?*. gopt (sseq "else" -*?*. pStmt) >@ (uncurry . uncurry) If \/
    sseq "while" -*?*. (sym '(' -*?*. pExp .*?*- sym ')') .*?*. pStmt >@
    uncurry While \/
    sseq "return" -*-*. gopt pExp .*?*- sym ';' >@ Return

pExp :: Parser Char Exp
pExp =
    pExp1 .*?*. gopt (sym ':' -*?*. pExp) >@
    \(a, m) -> case m of
        Nothing -> a
        Just b -> ECons a b

(.<<) :: Parser Char v -> Parser Char (v -> v) -> Parser Char v
(.<<) p q = p .*. gstar (ows -*. q) >@ uncurry (foldl (\a f -> f a))
infixl 4 .<<

pExp1 :: Parser Char Exp
pExp1 = pExp2 .<<
    sseq "&&" -*?*. pExp2 >@ EAnd \/
    sseq "||" -*?*. pExp2 >@ EOr

pExp2 :: Parser Char Exp
pExp2 = pExp3 .<<
    sseq "==" -*?*. pExp3 >@ EEq \/
    sseq "!=" -*?*. pExp3 >@ ENeq

pExp3 :: Parser Char Exp
pExp3 = pExp4 .<<
    sym '<' -*?*. pExp4 >@ ELt \/
    sym '>' -*?*. pExp4 >@ EGt \/
    sseq "<=" -*?*. pExp4 >@ ELe \/
    sseq ">=" -*?*. pExp4 >@ EGe

pExp4 :: Parser Char Exp
pExp4 = pExp5 .<<
    sym '+' -*?*. pExp5 >@ EPlus \/
    sym '-' -*?*. pExp5 >@ EMinus

pExp5 :: Parser Char Exp
pExp5 = pExp6 .<<
    sym '*' -*?*. pExp6 >@ ETimes \/
    sym '/' -*?*. pExp6 >@ EDiv \/
    sym '%' -*?*. pExp6 >@ EMod

pExp6 :: Parser Char Exp
pExp6 =
    sym '!' -*?*. pExp6 >@ ENot \/
    sym '-' -*?*. pExp6 >@ ENeg \/
    pNonOpExp

pNonOpExp :: Parser Char Exp
pNonOpExp =
    pInt >@ EInt \/ pBool >@ EBool \/ pChar >@ EChar \/
    sym '[' .*?*. sym ']' >! ENil \/
    sym '(' -*?*. pExp .*?*- sym ',' .*?*. pExp .*?*- sym ')' >@
    uncurry ETuple \/
    pId .*?*. pField >@ uncurry EId \/ pFunCall >@ uncurry EFunCall \/
    sym '(' -*?*. pExp .*?*- sym ')'

pField :: Parser Char [Field]
pField = gstar (ows .*. sym '.' -*?*. (
    sseq "hd" >! Head \/
    sseq "tl" >! Tail \/
    sseq "fst" >! First \/
    sseq "snd" >! Second)) >@ enlist

pFunCall :: Parser Char (String, [Exp])
pFunCall = pId .*?*. (sym '(' -*?*. (commaList pExp) .*?*- sym ')')

pInt :: Parser Char Int
pInt = gopt (sym '-' .*- ows) .*. gplus (satisfy isDigit) >@ read . enlist

pBool :: Parser Char Bool
pBool = sseq "False" >! False \/ sseq "True" >! True

pChar :: Parser Char Char
pChar = sym '\'' -*. anything .*- sym '\''

pId :: Parser Char String
pId = satisfy isAlpha .*. gstar (sym '_' \/ satisfy isAlphaNum) >@ enlist
