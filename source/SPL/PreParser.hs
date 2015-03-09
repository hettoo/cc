module SPL.PreParser where
import Parser
import Listify

pPre :: Parser Char String
pPre = gstar pUniWS >@ concat

pUniWS :: Parser Char String
pUniWS = anything >@ listify \>/
    (pLineComment \/ pBlockComment) >! ""

pLineComment :: Parser Char String
pLineComment = sseq "//" .*. gstar (nsym '\n') .*. sym '\n' >! ""

pBlockComment :: Parser Char String
pBlockComment = sseq "/*" .*. gstar (sym '*' -*. nsym '/' \/ nsym '*') .*.
    sseq "*/" >! ""
