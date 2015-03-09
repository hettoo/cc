module Main where
import SPL.Structure
import Parser
import SPL.PreParser
import SPL.Parser

parseSPL :: String -> Stmt
parseSPL = parse $ pPre >@ parse pSPL

main = do
    s <- getContents
    print $ parseSPL s
