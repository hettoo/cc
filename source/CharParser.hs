module CharParser where
import Parser

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
