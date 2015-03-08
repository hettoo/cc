module Main where
import SPL
import Parser
import SPLPreParser
import SPLParser

parseSPL :: String -> Stmt
parseSPL = parse $ pPre >@ parse pSPL

main = do
    s <- getContents
    print $ parseSPL s
