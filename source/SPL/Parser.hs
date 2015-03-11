module SPL.Parser where
import SPL.Structure
import Parser
import CharParser
import Listify
import Data.Char
import Utils

pSPL :: Parser Char [Stmt]
pSPL = ows -*. star (pDecl .*- ows)

pDecl :: Parser Char Stmt
pDecl = pVarDecl \/ pFunDecl

pVarDecl :: Parser Char Stmt
pVarDecl l = (pType .*?*. pId .*?*. (sym '=' -*?*. pExp .*?*- sym ';') >@
    (uncurry . uncurry) VarDecl) l

pFunDecl :: Parser Char Stmt
pFunDecl = pRetType .*?*. pId .*?*.
    (sym '(' -*?*. commaList (pType .*?*. pId) .*?*- sym ')') .*?*.
    (sym '{' -*?*. (star (pVarDecl .*- ows) .*.
        star (pStmt .*- ows) >@ uncurry (++)) .*- sym '}') >@
        (uncurry . uncurry . uncurry) FunDecl

pRetType :: Parser Char Type
pRetType = sseq "Void" .*- nalphanum_ >! TVoid \/ pType

pType :: Parser Char Type
pType = pBasicType \/
    pId >@ TPoly \/
    sym '[' -*?*. pType .*?*- sym ']' >@ TList \/
    sym '(' -*?*. (pType .*?*- sym ',') .*?*. pType .*?*- sym ')' >@
        uncurry TTuple

pBasicType :: Parser Char Type
pBasicType = (sseq "Int" >! TInt \/ sseq "Bool" >! TBool \/
    sseq "Char" >! TChar) .*- nalphanum_

pStmt :: Parser Char Stmt
pStmt = sym '{' -*?*. star (pStmt .*- ows) .*- sym '}' >@ sm id Stmts \/
    sseq "if" -*?*. (sym '(' -*?*. pExp .*?*- sym ')') .*?*.
        pStmt .*?*. opt (sseq "else" -*?*. pStmt) >@ (uncurry . uncurry) If \/
    sseq "while" -*?*. (sym '(' -*?*. pExp .*?*- sym ')') .*?*. pStmt >@
        uncurry While \/
    (sseq "return" .*. nalphanum_) -*?*. opt pExp .*?*- sym ';' >@ Return \/
    pId .*?*. pField .*?*. (sym '=' -*?*. pExp .*?*- sym ';') >@
        (uncurry . uncurry) Assign \/
    pFunCall .*?*- sym ';' >@ uncurry FunCall

pExp :: Parser Char Exp
pExp = pExp1 .*?*. opt (sym ':' -*?*. pExp) >@
    \(a, m) -> case m of
        Nothing -> a
        Just b -> EOp2 OCons a b

(+<<) :: Parser Char v -> Parser Char (v -> v) -> Parser Char v
(+<<) p q = p .*. star (ows -*. q) >@ uncurry (foldl (\a f -> f a))
infixl 4 +<<

pExp1 :: Parser Char Exp
pExp1 = pExp2 +<<
    sseq "&&" -*?*. pExp2 >@ flip (EOp2 OAnd) \/
    sseq "||" -*?*. pExp2 >@ flip (EOp2 OOr)

pExp2 :: Parser Char Exp
pExp2 = pExp3 +<<
    sseq "==" -*?*. pExp3 >@ flip (EOp2 OEq) \/
    sseq "!=" -*?*. pExp3 >@ flip (EOp2 ONeq)

pExp3 :: Parser Char Exp
pExp3 = pExp4 +<<
    sym '<' -*?*. pExp4 >@ flip (EOp2 OLt) \/
    sym '>' -*?*. pExp4 >@ flip (EOp2 OGt) \/
    sseq "<=" -*?*. pExp4 >@ flip (EOp2 OLe) \/
    sseq ">=" -*?*. pExp4 >@ flip (EOp2 OGe)

pExp4 :: Parser Char Exp
pExp4 = pExp5 +<<
    sym '+' -*?*. pExp5 >@ flip (EOp2 OPlus) \/
    sym '-' -*?*. pExp5 >@ flip (EOp2 OMinus)

pExp5 :: Parser Char Exp
pExp5 = pExp6 +<<
    sym '*' -*?*. pExp6 >@ flip (EOp2 OTimes) \/
    sym '/' -*?*. pExp6 >@ flip (EOp2 ODiv) \/
    sym '%' -*?*. pExp6 >@ flip (EOp2 OMod)

pExp6 :: Parser Char Exp
pExp6 = pNonOpExp \/
    sym '!' -*?*. pExp6 >@ EOp1 ONot \/
    sym '-' -*?*. pExp6 >@ EOp1 ONeg

pNonOpExp :: Parser Char Exp
pNonOpExp = pInt >@ EInt \/ pBool >@ EBool \/ pChar >@ EChar \/
    pFunCall >@ uncurry EFunCall \/
    sym '(' -*?*. pExp .*?*- sym ')' \/
    sym '[' .*?*. sym ']' >! ENil \/
    sym '(' -*?*. pExp .*?*- sym ',' .*?*. pExp .*?*- sym ')' >@
        uncurry ETuple \/
    pId .*?*. pField >@ uncurry EId

pField :: Parser Char [Field]
pField = star (ows .*. sym '.' -*?*. (
    sseq "hd" >! Head \/ sseq "tl" >! Tail \/
    sseq "fst" >! First \/ sseq "snd" >! Second)) >@ listify

pFunCall :: Parser Char (String, [Exp])
pFunCall = pId .*?*. (sym '(' -*?*. (commaList pExp) .*?*- sym ')')

pInt :: Parser Char Int
pInt = opt (sym '-' .*- ows) .*. plus (satisfy isDigit) >@ read . listify

pBool :: Parser Char Bool
pBool = sseq "False" >! False \/ sseq "True" >! True

pChar :: Parser Char Char
pChar = sym '\'' -*. anything .*- sym '\''

pId :: Parser Char String
pId = satisfy isAlpha .*. star (sym '_' \/ satisfy isAlphaNum) >@ listify
