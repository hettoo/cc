module Parser where
import Listify
import Utils

type Parser a s v = [a] -> s -> (s, Maybe (v, [a]))

class ParserState s where
    initial :: s
    update :: s -> a -> s
    merge :: s -> s -> s
    setError :: s -> String -> s
    getError :: s -> Maybe String

parse :: (ParserState s, Eq a, Eq v, Show v) =>
    Parser a s v -> [a] -> v
parse p l = case p l initial of
    (s, Just (v, r)) | isEmpty r -> v
    (s, _) -> error $ "parser error" ++ case getError s of
        Nothing -> ""
        Just e -> ": " ++ e

-- Main parser building blocks

nop :: Parser a s v
nop _ s = (s, Nothing)

yield :: v -> Parser a s v
yield v l s = (s, Just (v, l))

yieldError :: ParserState s =>
    String -> Parser a s v
yieldError e l s = nop l (setError s e)

skip :: ParserState s =>
    Parser a s a
skip l s = case l of
    a : r -> yield a r (update s a)
    _ -> yieldError "unexpected eof" l s

eof :: ParserState s =>
    Parser a s ()
eof l = (if isEmpty l then yield () else yieldError "expected eof") l

satisfy :: (ParserState s, Show a) =>
    (a -> Bool) -> Parser a s a
satisfy f l s = case l of
    a : r -> case f a of
        True -> yield a r s
        False -> yieldError ("unexpected symbol `" ++ show a ++ "'") l s
    _ -> yieldError "unexpected eof" l s

phantom :: Parser a s v -> Parser a s v
phantom p l s = pair (const s, fmap (\t -> (fst t, l))) (p l s)

cond :: ParserState s =>
    Parser a s v -> Parser a s v -> Parser a s v
cond p q l s = case p l s of
    (t, Nothing) -> case q l s of
        (u, Nothing) -> (merge t u, Nothing)
        x -> x
    x -> x

(>.) :: Parser a s v -> Parser a s (v -> w) -> Parser a s w
(>.) p q l s = case p l s of
    (t, Just (v, l')) -> pair (id, fmap (\(f, l'') -> (f v, l''))) (q l' t)
    (t, Nothing) -> (t, Nothing)
infixl 7 >.

-- Some useful abbreviations

(.*.) :: ParserState s =>
    Parser a s v -> Parser a s w -> Parser a s (v, w)
(.*.) p q = cond (p >. (q >@ \w v -> (v, w))) nop
infixl 7 .*.

(.*-) :: ParserState s =>
    Parser a s v -> Parser a s w -> Parser a s v
(.*-) p q = p .*. q >@ fst
infixl 7 .*-

(-*.) :: ParserState s =>
    Parser a s v -> Parser a s w -> Parser a s w
(-*.) p q = p .*. q >@ snd
infixl 7 -*.

(>@) :: Parser a s v -> (v -> w) -> Parser a s w
(>@) p f = p >. yield f
infixl 6 >@

(>!) :: Parser a s v -> w -> Parser a s w
(>!) p w = p >@ const w
infixl 6 >!

(\/) :: ParserState s =>
    Parser a s v -> Parser a s v -> Parser a s v
(\/) = cond
infixr 5 \/

(\+/) :: ParserState s =>
    Parser a s v -> Parser a s w -> Parser a s (Either v w)
(\+/) p q = (p >@ Left) \/ (q >@ Right)
infixr 5 \+/

sep :: ParserState s =>
    Parser a s v -> Parser a s (Maybe v)
sep p = phantom p >@ Just \/ eof >! Nothing

opt :: ParserState s =>
    Parser a s v -> Parser a s (Maybe v)
opt p = p >@ Just \/ yield Nothing

plus :: ParserState s =>
    Parser a s v -> Parser a s (v, [v])
plus p = p .*. (opt (plus p >@ uncurry (:)) >@ listify)

star :: ParserState s =>
    Parser a s v -> Parser a s [v]
star p = opt (plus p) >@ listify

anything :: (ParserState s, Show a) =>
    Parser a s a
anything = satisfy $ const True

sym :: (Eq a, ParserState s, Show a) =>
    a -> Parser a s a
sym a = satisfy ((==) a)

nsym :: (Eq a, ParserState s, Show a) =>
    a -> Parser a s a
nsym a = satisfy ((/=) a)

sseq :: (Eq a, ParserState s, Show a) =>
    [a] -> Parser a s [a]
sseq l = case l of
    [] -> yield []
    a : r -> sym a .*. sseq r >@ uncurry (:)
