module Main where
import SPL.Structure
import Parser
import SPL.PreParser
import SPL.Parser
import SPL.Printer

parseSPL :: String -> [Stmt]
parseSPL = parse $ pPre >@ parse pSPL

main = do
    s <- getContents
    putStr $ prettyPrint $ parseSPL s
