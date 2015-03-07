module SPLParser where
import Parser
import Enlist
import Data.Char

data Field =
    Head
    | Tail
    | First
    | Second

pField :: Parser Char [Field]
pField = sym '.' -*. (
    sseq "hd" >! Head \/
    sseq "tl" >! Tail \/
    sseq "fst" >! First \/
    sseq "snd" >! Second) .*. opt pField >@ enlist

pInt :: Parser Char Int
pInt = opt (sym '-') .*. plus (satisfy isDigit) >@ read . enlist

pId :: Parser Char String
pId = satisfy isAlpha .*. star (sym '_' \/ satisfy isAlphaNum) >@ enlist
