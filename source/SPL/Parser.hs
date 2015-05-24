module SPL.Parser where
import SPL.Algebra
import Parser.Parser
import Parser.CharParser
import Listify
import Fix
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
pDecl = pDataDecl \/ pVarDecl \/ pFunDecl

pDataDecl :: CharReParser Stmt
pDataDecl = (sseq "data" -*-*. pTId .*. star (ws -*. pNId) .*?*- sym '=') .*?*.
    (opt (cons .*?*. star (ows -*. sym '|' -*?*. cons)) >@ listify') .*?*-
    sym ';' >@ (uncurry . uncurry) DataDecl
    where
    cons :: CharReParser (String, [(Type, String)])
    cons = pTId .*?*. (opt (sym '(' -*?*.
        commaList (pType .*?*. pNId) .*?*- sym ')') >@ listify)
    listify' m = case m of -- somehow listify will not compile :(
        Nothing -> []
        Just (p, l) -> p : l

pVarDecl :: CharReParser Stmt
pVarDecl l = (pType .*?*. pNId .*?*. (sym '=' -*?*. pExp .*?*- sym ';') >@
    (uncurry . uncurry) VarDecl) l

pFunDecl :: CharReParser Stmt
pFunDecl = pRetType .*?*. pNId .*?*.
    (sym '(' -*?*. commaList (pType .*?*. pNId) .*?*- sym ')') .*?*.
    (sym '{' -*?*. (star (pVarDecl .*- ows) .*.
        star (pStmt .*- ows) >@ Stmts . uncurry (++)) .*- sym '}') >@
        (uncurry . uncurry . uncurry) FunDecl

pRetType :: CharReParser Type
pRetType = sseq "Void" .*- nalphanum_ >! TVoid \/ pType

pType :: CharReParser Type
pType = pBasicType \/ pNId >@ TPoly \/
    sym '\\' -*?*. pTId .*. star (ws -*. pType) .*?*- sym '/' >@
        uncurry TCustom \/
    sym '[' -*?*. pType .*?*- sym ']' >@ tList \/
    sym '(' -*?*. (pType .*?*- sym ',') .*?*. pType .*?*- sym ')' >@
        uncurry tTuple

pBasicType :: CharReParser Type
pBasicType = (sseq "Int" >! TInt \/ sseq "Bool" >! TBool \/
    sseq "Char" >! TChar) .*- nalphanum_

pStmt :: CharReParser Stmt
pStmt = sseq "if" -*?*. (sym '(' -*?*. pExp .*?*- sym ')') .*?*.
        pStmt .*?*. opt (sseq "else" -*?*. pStmt) >@ (uncurry . uncurry) If \/
    sseq "while" -*?*. (sym '(' -*?*. pExp .*?*- sym ')') .*?*. pStmt >@
        uncurry While \/
    (sseq "return" .*. nalphanum_) -*?*. opt pExp .*?*- sym ';' >@ Return \/
    (sseq "case" .*. nalphanum_) -*?*. pExp .*?*.
        (sym '{' -*?*. star (pCase .*- ows) .*- sym '}') >@ uncurry Case \/
    pStmtId \/ sym '{' -*?*. star (pStmt .*- ows) .*- sym '}' >@ sm id Stmts

pCase :: CharReParser (String, Stmt)
pCase = pTId .*?*. pStmt

pStmtId :: CharReParser Stmt
pStmtId = pNId .*?*. (pField .*?*. (sym '=' -*?*. pExp) \+/
        sym '(' -*?*. commaList pExp .*?*- sym ')') .*?*- sym ';'
    >@ \(i, t) -> case t of
        Left (f, e) -> Assign i f e
        Right l -> FunCall i l

pExp :: CharReParser Exp
pExp = pExp1 .*?*. opt (sym ':' -*?*. pExp) >@
    \(a, m) -> case m of
        Nothing -> a
        Just b -> Fix $ EOp2 OCons a b

(+<<) :: CharReParser v -> CharReParser (v -> v) -> CharReParser v
(+<<) p q = p .*. star (ows -*. q) >@ uncurry (foldl (\a f -> f a))
infixl 4 +<<

flipFix f = \a b -> Fix $ f b a

pExp1 :: CharReParser Exp
pExp1 = pExp2 +<<
    sseq "&&" -*?*. pExp2 >@ flipFix (EOp2 OAnd) \/
    sseq "||" -*?*. pExp2 >@ flipFix (EOp2 OOr)

pExp2 :: CharReParser Exp
pExp2 = pExp3 +<<
    sseq "==" -*?*. pExp3 >@ flipFix (EOp2 OEq) \/
    sseq "!=" -*?*. pExp3 >@ flipFix (EOp2 ONeq)

pExp3 :: CharReParser Exp
pExp3 = pExp4 +<<
    sym '<' -*?*. pExp4 >@ flipFix (EOp2 OLt) \/
    sym '>' -*?*. pExp4 >@ flipFix (EOp2 OGt) \/
    sseq "<=" -*?*. pExp4 >@ flipFix (EOp2 OLe) \/
    sseq ">=" -*?*. pExp4 >@ flipFix (EOp2 OGe)

pExp4 :: CharReParser Exp
pExp4 = pExp5 +<<
    sym '+' -*?*. pExp5 >@ flipFix (EOp2 OPlus) \/
    sym '-' -*?*. pExp5 >@ flipFix (EOp2 OMinus)

pExp5 :: CharReParser Exp
pExp5 = pExp6 +<<
    sym '*' -*?*. pExp6 >@ flipFix (EOp2 OTimes) \/
    sym '/' -*?*. pExp6 >@ flipFix (EOp2 ODiv) \/
    sym '%' -*?*. pExp6 >@ flipFix (EOp2 OMod)

pExp6 :: CharReParser Exp
pExp6 = pNonOpExp \/ sym '!' -*?*. pExp6 >@ Fix . EOp1 ONot \/
    sym '-' -*?*. pExp6 >@ Fix . EOp1 ONeg

pNonOpExp :: CharReParser Exp
pNonOpExp = pInt >@ Fix . EInt \/ pBool >@ Fix . EBool \/
    pChar >@ Fix . EChar \/ pIdExp \/ pConsExp \/
    sym '(' -*?*. pExp .*?*- sym ')' \/ sym '[' .*?*. sym ']' >! Fix ENil \/
    sym '(' -*?*. (pExp .*?*- sym ',') .*?*. pExp .*?*- sym ')' >@
        Fix . uncurry ETuple

pIdExp :: CharReParser Exp
pIdExp = pNId .*?*. (sym '(' -*?*. commaList pExp .*?*- sym ')' \+/ pField)
    >@ \(i, e) -> case e of
        Left l -> Fix $ EFunCall i l
        Right f -> Fix $ EId i f

pConsExp :: CharReParser Exp
pConsExp = pTId .*. (sym '(' -*?*. commaList pExp .*?*- sym ')') >@
    Fix . uncurry ECons

pField :: CharReParser [String]
pField = star (ows .*. sym '.' -*?*. pId) >@ listify

pInt :: CharReParser Int
pInt = opt (sym '-' .*- ows) .*. plus (satisfy isDigit) >@ read . listify

pBool :: CharReParser Bool
pBool = sseq "False" >! False \/ sseq "True" >! True

pChar :: CharReParser Char
pChar = sym '\'' -*. anything .*- sym '\''

pId :: CharReParser String
pId = satisfy isAlpha .*. star (sym '_' \/ satisfy isAlphaNum) >@ listify

pNId :: CharReParser String
pNId = satisfy isLower .*. star (sym '_' \/ satisfy isAlphaNum) >@ listify

pTId :: CharReParser String
pTId = satisfy isUpper .*. star (sym '_' \/ satisfy isAlphaNum) >@ listify
