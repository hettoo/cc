module Main where
import Parser
import SPLParser

main = do
    s <- getContents
    print $ parse (pInt .*- sym '\n') s
