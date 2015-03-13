module CharParser where
import Parser
import Listify
import Utils
import Data.Char

type CharParser v = Parser Char () v

instance ParserState () where
    initial = ()
    update _ _ = ()
    merge _ _ = ()
    setError _ _ = ()
    getError _ = Nothing

ws :: CharParser (Char, [Char])
ws = plus (satisfy ((flip elem) [' ', '\n', '\r', '\t']))

ows :: CharParser [Char]
ows = opt ws >@ listify

nalphanum_ :: CharParser (Maybe Char)
nalphanum_ = sep (satisfy (\c -> not (c == '_' || isAlphaNum c)))

(.*?*.) :: CharParser v -> CharParser w -> CharParser (v, w)
(.*?*.) p q = (p .*- ows) .*. q
infixl 7 .*?*.

(-*?*.) :: CharParser v -> CharParser w -> CharParser w
(-*?*.) p q = (p .*. ows) -*. q
infixl 7 -*?*.

(.*?*-) :: CharParser v -> CharParser w -> CharParser v
(.*?*-) p q = p .*- (ows .*. q)
infixl 7 .*?*-

neCommaList :: CharParser v -> CharParser (v, [v])
neCommaList p = p .*?*. star (sym ',' -*?*. p)

commaList :: CharParser v -> CharParser [v]
commaList p = opt (neCommaList p) >@ listify
