module SPLPreParser where
import Parser
import Enlist

pPre :: Parser Char String
pPre = star (pUniWS \>/ pLineComment \>/ pBlockComment) >@ concat

pUniWS :: Parser Char String
pUniWS = anything >@ enlist \>/
    gplus (sym ' ' \/ sym '\t' \/ sym '\n' \/ sym '\r') >! " "

pLineComment :: Parser Char String
pLineComment = sseq "//" .*. star (nsym '\n') .*. sym '\n' >! ""

pBlockComment :: Parser Char String
pBlockComment = sseq "/*" .*. star (sym '*' -*. nsym '/' \/ nsym '*') .*.
    sseq "*/" >! ""
