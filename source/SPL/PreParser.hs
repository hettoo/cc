module SPL.PreParser where
import Parser
import Listify

pPre :: Parser Char String
pPre = star pUniWS >@ concat

pUniWS :: Parser Char String
pUniWS = (pLineComment \/ pBlockComment) >! "" \/
    anything >@ listify

pLineComment :: Parser Char String
pLineComment = sseq "//" .*. star (nsym '\n') .*. sym '\n' >! ""

pBlockComment :: Parser Char String
pBlockComment = sseq "/*" .*. star (sym '*' -*. nsym '/' \/ nsym '*') .*.
    sseq "*/" >! ""
