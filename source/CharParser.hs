module CharParser where
import Parser
import Listify
import Utils
import Data.Char

ws :: Parser Char (Char, [Char])
ws = plus (satisfy (\c -> c `elem` [' ', '\n', '\r', '\t']))

ows :: Parser Char [Char]
ows = opt ws >@ listify

nalphanum_ :: Parser Char (Maybe Char)
nalphanum_ = sep (satisfy (\c -> not (c == '_' || isAlphaNum c)))

(.*?*.) :: Parser Char v -> Parser Char w -> Parser Char (v, w)
(.*?*.) p q = p .*- ows .*. q
infixl 7 .*?*.

(-*?*.) :: Parser Char v -> Parser Char w -> Parser Char w
(-*?*.) p q = (p .*. ows) -*. q
infixl 7 -*?*.

(.*?*-) :: Parser Char v -> Parser Char w -> Parser Char v
(.*?*-) p q = p .*- (ows .*. q)
infixl 7 .*?*-

neCommaList :: Parser Char v -> Parser Char (v, [v])
neCommaList p = p .*?*. star (sym ',' -*?*. p)

commaList :: Parser Char v -> Parser Char [v]
commaList p = opt (neCommaList p) >@ listify
