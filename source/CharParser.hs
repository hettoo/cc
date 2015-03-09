module CharParser where
import Parser
import Enlist
import Utils
import Data.Char

wspred :: Char -> Bool
wspred c = c `elem` [' ', '\n', '\r', '\t']

ws :: Parser Char (Char, [Char])
ws = gplus (satisfy wspred)

ows :: Parser Char [Char]
ows = gopt ws >@ enlist

nalphanum_ :: Parser Char ()
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
neCommaList p = p .*?*. gstar (sym ',' -*?*. p)

commaList :: Parser Char v -> Parser Char [v]
commaList p = gopt (neCommaList p) >@ enlist
