module SPLParser where
import Parser
import Enlist
import Data.Char

data Field =
    Head
    | Tail
    | First
    | Second

-- pSPL

-- pDecl

-- pVarDecl

-- pFunDecl

-- pRetType

-- pType

-- pBasicType

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
