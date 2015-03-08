module SPLPreParser where
import Parser
import Enlist

pPre :: Parser Char String
pPre = gstar pUniWS >@ concat

pSingleWS :: Parser Char Char
pSingleWS = sym ' ' \/ sym '\t' \/ sym '\n' \/ sym '\r'

pUniWS :: Parser Char String
pUniWS = anything >@ enlist \>/
    gplus (pSingleWS .*. gopt (pLineComment \/ pBlockComment)) >! " " \>/
    gplus (pLineComment \/ pBlockComment) >! ""

pLineComment :: Parser Char String
pLineComment = sseq "//" .*. gstar (nsym '\n') .*. sym '\n' >! ""

pBlockComment :: Parser Char String
pBlockComment = sseq "/*" .*. gstar (sym '*' -*. nsym '/' \/ nsym '*') .*.
    sseq "*/" >! ""
