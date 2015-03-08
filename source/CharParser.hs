module CharParser where
import Parser
import Enlist

ws :: Parser Char Char
ws = sym ' '

ows :: Parser Char (Maybe Char)
ows = opt ws

(.*-*.) :: Parser Char v -> Parser Char w -> Parser Char (v, w)
(.*-*.) p q = p .*- ws .*. q
infixl 7 .*-*.

(-*-*.) :: Parser Char v -> Parser Char w -> Parser Char w
(-*-*.) p q = p .*. ws -*. q
infixl 7 -*-*.

(.*-*-) :: Parser Char v -> Parser Char w -> Parser Char v
(.*-*-) p q = p .*- (ws .*. q)
infixl 7 .*-*-

(.*?*.) :: Parser Char v -> Parser Char w -> Parser Char (v, w)
(.*?*.) p q = p .*- ows .*. q
infixl 7 .*?*.

(-*?*.) :: Parser Char v -> Parser Char w -> Parser Char w
(-*?*.) p q = p .*. ows -*. q
infixl 7 -*?*.

(.*?*-) :: Parser Char v -> Parser Char w -> Parser Char v
(.*?*-) p q = p .*- (ows .*. q)
infixl 7 .*?*-

neCommaList :: Parser Char v -> Parser Char (v, [v])
neCommaList p = p .*?*. gstar (sym ',' -*?*. p)

commaList :: Parser Char v -> Parser Char [v]
commaList p = gopt (neCommaList p) >@ enlist
