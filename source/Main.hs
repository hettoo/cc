module Main where
import Parser
import SPLPreParser
import SPLParser

main = do
    s <- getContents
    print $ parse (pInt .*- sym '\n') s
