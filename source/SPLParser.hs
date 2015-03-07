module SPLParser where
import Parser
import Enlist
import Data.Char

data Type =
    VarType String
    | BInt
    | BBool
    | BChar
    | TType Type Type
    | LType Type

data RetType =
    Type Type
    | Void

data Field =
    Head
    | Tail
    | First
    | Second

-- pSPL

-- pDecl

-- pVarDecl

-- pFunDecl

pRetType :: Parser Char RetType
pRetType = pType >@ Type \/ sseq "Void" >! Void

-- Note that we unfold BasicType into Type
pType :: Parser Char Type
pType =
    pId >@ VarType \/
    sseq "Int" >! BInt \/
    sseq "Bool" >! BBool \/
    sseq "Char" >! BChar \/
    sym '(' -*. pType .*- sym ',' .*. pType .*- sym ')' >@ uncurry TType \/
    sym '[' -*. pType .*- sym ']' >@ LType

-- pFArgs

-- pStmt

-- pExp

-- pOpExp

-- pOpExp1

-- pOpExp2

-- pOpExp3

-- pOpExp4

-- pOpExp5

-- pOpExp6

-- pNonOpExp

pField :: Parser Char [Field]
pField = sym '.' -*. (
    sseq "hd" >! Head \/
    sseq "tl" >! Tail \/
    sseq "fst" >! First \/
    sseq "snd" >! Second) .*. opt pField >@ enlist

-- pFunCall

-- pActArgs

pInt :: Parser Char Int
pInt = opt (sym '-') .*. plus (satisfy isDigit) >@ read . enlist

pChar :: Parser Char Char
pChar = sym '\'' -*. anything .*- sym '\''

pId :: Parser Char String
pId = satisfy isAlpha .*. star (sym '_' \/ satisfy isAlphaNum) >@ enlist
