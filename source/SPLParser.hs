module SPLParser where
import Parser
import Combine
import Data.Char

pInt :: Parser Char Int
pInt = opt (sym '-') .*. plus (satisfy isDigit) >@ read . combine

pId :: Parser Char String
pId = satisfy isAlpha .*. star (sym '_' \/ satisfy isAlphaNum) >@ combine
