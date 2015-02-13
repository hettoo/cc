module Tokenizer where
import SemiLattice
import Mealy

data Token = TAssign
           | TSemicolon
           | TComma
           | TLeftParenthesis
           | TRightParenthesis
           | TLeftCBracket
           | TRightCBracket
           | TLeftBracket
           | TRightBracket
           | TVoid
           | TIf
           | TElse
           | TWhile
           | TReturn
           | TNil
           | TBool
           | TBasicType
           | TOp1
           | TOp2
           | TChar
           | TField
           | TInt
           | TId
           deriving (Eq, Show)

-- Final tokens will be annotated with the corresponding string and its
-- location.
type TData = (String, Int, Int)
type AToken = (Token, TData)

-- We need to add some special tokens as tokenizer commands.
data CToken = NextLine
            | Clear
            | Token Token
            deriving (Eq, Show)

-- The state representation of the tokenizer happens to match the
-- representation of the annotated token data.
type TState = (String, Int, Int)

-- However, strings corresponding to tokens are recorded in reverse.
tdata :: TState -> TData
tdata (w, c, l) = (reverse w, c, l)

-- The character advance command is executed first on each new character.
readc :: Maybe Char -> Bool -> TState -> TState
readc a b (w, c, l) = case b of
    True -> as w
    False -> as []
    where
    as u = ((case a of
        Nothing -> u
        Just b -> b : u), c + 1, l)

-- Further effects depend on the command in the token.
command :: CToken -> TState -> TState
command c = case c of
    NextLine -> \(u, c, l) -> (u, 0, l + 1)
    Clear -> \(u, c, l) -> ("", c, l)
    _ -> id

-- We create some abbreviations for practical reasons.
type A = Maybe Char
type B = SimpleLattice CToken
type Var = Int

type TMealy q = Mealy A B q

tokenize :: TMealy q -> q -> String -> [AToken]
tokenize m i s = tokenize' i ([], 0, 0) $ (map Just s) ++ [Nothing]
    where
    tokenize' _ _ [] = []
    tokenize' q state (a : r) = case m q a of
        (s, p) -> case s of
            Bottom -> advance id True
            Val c -> case c of
                Token t -> (t, tdata state) : r
                _ -> r
                where
                r = advance (command c) False
            Top -> error "Overspecified tokenizer"
            where
            advance f b = tokenize' p (f $ readc a b state) r

type TFormula = MealyFormula A B Int

fresh :: (TFormula -> TFormula) -> Var
fresh f = case freshest (f FF) of
    Just n -> n + 1
    Nothing -> 0

tseq :: String -> TFormula -> TFormula
tseq l f = foldr (Trans . Just) f l

tmseq :: [String] -> TFormula -> TFormula
tmseq l f = foldr (\s -> Add (tseq s f)) FF l

tsum :: [A] -> TFormula -> TFormula
tsum l t = foldr (\a -> Add (Trans a t)) FF l

tosum :: [A] -> B -> TFormula
tosum l b = foldr (\a -> Add (Out a b)) FF l

topt :: (TFormula -> TFormula) -> TFormula -> TFormula
topt f g = f g `Add` g

tot :: A -> B -> TFormula -> TFormula
tot a b f = Trans a f `Add` Out a b

sub :: Eq a => [a] -> [a] -> [a]
sub a b = filter (\c -> not $ c `elem` b) a

mc :: [Char] -> [A]
mc = map Just

call = ['\0'..'\255']
cnum = ['0'..'9']
calpha = ['a'..'z'] ++ ['A'..'Z']
calphanum = calpha ++ cnum
calphanum_ = '_' : calphanum
cwhite = [' ', '\t', '\n']
mcall = Nothing : mc call

nt :: Token -> B
nt = Val . Token

taccept :: Token -> TFormula
taccept = tosum mcall . nt

tokenize_default = tokenize synthesize $ foldr add id formulas FF
    where
    add f g h = f h `Add` g h

formulas :: [TFormula -> TFormula]
formulas = [
    (\f -> tseq "=" (taccept TAssign `Add` f)),
    (\f -> tseq ";" (taccept TSemicolon `Add` f)),
    (\f -> tseq "," (taccept TComma `Add` f)),
    (\f -> tseq "(" (taccept TLeftParenthesis `Add` f)),
    (\f -> tseq ")" (taccept TRightParenthesis `Add` f)),
    (\f -> tseq "{" (taccept TLeftCBracket `Add` f)),
    (\f -> tseq "}" (taccept TRightCBracket `Add` f)),
    (\f -> tseq "[" (taccept TLeftBracket `Add` f)),
    (\f -> tseq "]" (taccept TRightBracket `Add` f)),
    (\f -> tseq "Void" (taccept TVoid `Add` f)),
    (\f -> tseq "if" (taccept TIf `Add` f)),
    (\f -> tseq "else" (taccept TElse `Add` f)),
    (\f -> tseq "while" (taccept TWhile `Add` f)),
    (\f -> tseq "return" (taccept TReturn `Add` f)),
    (\f -> tseq "[]" (taccept TNil `Add` f)),
    (\f -> tmseq ["True", "False"] (taccept TBool `Add` f)),
    (\f -> tmseq ["Int", "Bool", "Char"] (taccept TBasicType `Add` f)),
    (\f -> tmseq ["!", "-"] (taccept TOp1 `Add` f)),
    (\f -> tmseq ["+", "-", "*", "/", "%", "==", "<", ">", "<=", ">=", "!=",
                  "&&", "||", ":"] (taccept TOp2 `Add` f)),
    (\f -> tmseq (map (\c -> ['\'', c, '\'']) cnum) (taccept TChar `Add` f)),
    (\f -> (tmseq [".hd", ".tl", ".fst", ".snd"] $ Nu 0 $
        tosum (mcall `sub` mc ['.']) (nt TField) `Add` f
        `Add` tmseq [".hd", ".tl", ".fst", ".snd"] (Var 0))),
    (\f -> (topt (tseq "-") $ tsum (mc cnum) $ Nu 0 $
        tosum (mcall `sub` mc cnum) (nt TInt) `Add` f
        `Add` tsum (mc cnum) (Var 0))),
    (\f -> (tsum (mc calpha) $ Nu 0 $ tosum (mcall `sub` mc calphanum_) (nt TId)
        `Add` f `Add` tsum (mc calphanum_) (Var 0))),
    (\f -> tsum (mc cwhite) (Nu 0 $ tosum (mcall `sub` mc cwhite) (Val Clear)
        `Add` f `Add` tsum (mc cwhite) (Var 0))),
    (\f -> tseq "\n" (tosum mcall (Val NextLine) `Add` f)),
    (\f -> tseq "//" (Nu 0 $ tsum (mcall `sub` mc ['\n']) (Var 0)
        `Add` tseq "\n" (tosum mcall (Val Clear) `Add` f))),
    (\f -> tseq "/*" (Nu 0 $ tsum (mcall `sub` mc ['*']) (Var 0)
        `Add` tseq "*" (tsum (mcall `sub` mc ['/']) (Var 0)
            `Add` tseq "/" (tosum mcall (Val Clear) `Add` f))))]
