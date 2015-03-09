module SPL.PreParser where
import Parser
import Enlist

pPre :: Parser Char String
pPre = gstar pUniWS >@ concat

pUniWS :: Parser Char String
pUniWS = anything >@ enlist \>/
    (pLineComment \/ pBlockComment) >! ""

pLineComment :: Parser Char String
pLineComment = sseq "//" .*. gstar (nsym '\n') .*. sym '\n' >! ""

pBlockComment :: Parser Char String
pBlockComment = sseq "/*" .*. gstar (sym '*' -*. nsym '/' \/ nsym '*') .*.
    sseq "*/" >! ""
