module SPL.Parser where
import SPL.Structure
import Parser
import CharParser
import Listify
import Utils
import Data.Char
import Data.Either

parseSPL :: String -> [Stmt]
parseSPL s = (parse $ pPre >@ parse pSPL) (map double s)

pPre :: CharParser [(Char, CharState)]
pPre = star ((sseq "//" .*. star (nsym '\n') .*. (sym '\n' \+/ eof) \+/
    sseq "/*" .*. star (sym '*' -*. nsym '/' \/ nsym '*') .*.  sseq "*/") \+/
    reveal anything) .*- eof >@ rights

pSPL :: CharReParser [Stmt]
pSPL = ows -*. star (pDecl .*- ows) .*- eof

pDecl :: CharReParser Stmt
pDecl = pVarDecl \/ pFunDecl

pVarDecl :: CharReParser Stmt
pVarDecl l = (pType .*?*. pId .*?*. (sym '=' -*?*. pExp .*?*- sym ';') >@
    (uncurry . uncurry) VarDecl) l

pFunDecl :: CharReParser Stmt
pFunDecl = pRetType .*?*. pId .*?*.
    (sym '(' -*?*. commaList (pType .*?*. pId) .*?*- sym ')') .*?*.
    (sym '{' -*?*. (star (pVarDecl .*- ows) .*.
        star (pStmt .*- ows) >@ uncurry (++)) .*- sym '}') >@
        (uncurry . uncurry . uncurry) FunDecl

pRetType :: CharReParser Type
pRetType = sseq "Void" .*- nalphanum_ >! TVoid \/ pType

pType :: CharReParser Type
pType = pBasicType \/ pId >@ TPoly \/
    sym '[' -*?*. pType .*?*- sym ']' >@ TList \/
    sym '(' -*?*. (pType .*?*- sym ',') .*?*. pType .*?*- sym ')' >@
        uncurry TTuple

pBasicType :: CharReParser Type
pBasicType = (sseq "Int" >! TInt \/ sseq "Bool" >! TBool \/
    sseq "Char" >! TChar) .*- nalphanum_

pStmt :: CharReParser Stmt
pStmt = sseq "if" -*?*. (sym '(' -*?*. pExp .*?*- sym ')') .*?*.
        pStmt .*?*. opt (sseq "else" -*?*. pStmt) >@ (uncurry . uncurry) If \/
    sseq "while" -*?*. (sym '(' -*?*. pExp .*?*- sym ')') .*?*. pStmt >@
        uncurry While \/
    (sseq "return" .*. nalphanum_) -*?*. opt pExp .*?*- sym ';' >@ Return \/
    pStmtId \/ sym '{' -*?*. star (pStmt .*- ows) .*- sym '}' >@ sm id Stmts

pStmtId :: CharReParser Stmt
pStmtId = pId .*?*. (pField .*?*. (sym '=' -*?*. pExp) \+/
        sym '(' -*?*. commaList pExp .*?*- sym ')') .*?*- sym ';'
    >@ \(i, t) -> case t of
        Left (f, e) -> Assign i f e
        Right l -> FunCall i l

pExp :: CharReParser Exp
pExp = pExp1 .*?*. opt (sym ':' -*?*. pExp) >@
    \(a, m) -> case m of
        Nothing -> a
        Just b -> EOp2 OCons a b

(+<<) :: CharReParser v -> CharReParser (v -> v) -> CharReParser v
(+<<) p q = p .*. star (ows -*. q) >@ uncurry (foldl (\a f -> f a))
infixl 4 +<<

pExp1 :: CharReParser Exp
pExp1 = pExp2 +<<
    sseq "&&" -*?*. pExp2 >@ flip (EOp2 OAnd) \/
    sseq "||" -*?*. pExp2 >@ flip (EOp2 OOr)

pExp2 :: CharReParser Exp
pExp2 = pExp3 +<<
    sseq "==" -*?*. pExp3 >@ flip (EOp2 OEq) \/
    sseq "!=" -*?*. pExp3 >@ flip (EOp2 ONeq)

pExp3 :: CharReParser Exp
pExp3 = pExp4 +<<
    sym '<' -*?*. pExp4 >@ flip (EOp2 OLt) \/
    sym '>' -*?*. pExp4 >@ flip (EOp2 OGt) \/
    sseq "<=" -*?*. pExp4 >@ flip (EOp2 OLe) \/
    sseq ">=" -*?*. pExp4 >@ flip (EOp2 OGe)

pExp4 :: CharReParser Exp
pExp4 = pExp5 +<<
    sym '+' -*?*. pExp5 >@ flip (EOp2 OPlus) \/
    sym '-' -*?*. pExp5 >@ flip (EOp2 OMinus)

pExp5 :: CharReParser Exp
pExp5 = pExp6 +<<
    sym '*' -*?*. pExp6 >@ flip (EOp2 OTimes) \/
    sym '/' -*?*. pExp6 >@ flip (EOp2 ODiv) \/
    sym '%' -*?*. pExp6 >@ flip (EOp2 OMod)

pExp6 :: CharReParser Exp
pExp6 = pNonOpExp \/ sym '!' -*?*. pExp6 >@ EOp1 ONot \/
    sym '-' -*?*. pExp6 >@ EOp1 ONeg

pNonOpExp :: CharReParser Exp
pNonOpExp = pInt >@ EInt \/ pBool >@ EBool \/ pChar >@ EChar \/ pExpId \/
    sym '(' -*?*. pExp .*?*- sym ')' \/
    sym '[' .*?*. sym ']' >! ENil \/
    sym '(' -*?*. (pExp .*?*- sym ',') .*?*. pExp .*?*- sym ')' >@
        uncurry ETuple

pExpId :: CharReParser Exp
pExpId = pId .*?*. (sym '(' -*?*. commaList pExp .*?*- sym ')' \+/ pField)
    >@ \(i, e) -> case e of
        Left l -> EFunCall i l
        Right f -> EId i f

pField :: CharReParser [Field]
pField = star (ows .*. sym '.' -*?*. (
    sseq "hd" >! Head \/ sseq "tl" >! Tail \/
    sseq "fst" >! First \/ sseq "snd" >! Second)) >@ listify

pInt :: CharReParser Int
pInt = opt (sym '-' .*- ows) .*. plus (satisfy isDigit) >@ read . listify

pBool :: CharReParser Bool
pBool = sseq "False" >! False \/ sseq "True" >! True

pChar :: CharReParser Char
pChar = sym '\'' -*. anything .*- sym '\''

pId :: CharReParser String
pId = satisfy isAlpha .*. star (sym '_' \/ satisfy isAlphaNum) >@ listify
