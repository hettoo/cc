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
           deriving Show

-- Final tokens will be annotated with the corresponding string and its
-- location.
type TData = (String, Int, Int)
type AToken = (Token, TData)

-- We need to add some special tokens as tokenizer commands.
data CToken = NextLine
            | Clear
            | Token Token

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

tokenize_default :: String -> [AToken]
tokenize_default = tokenize synthesize tassign

tseq :: String -> TFormula -> TFormula
tseq [] f = f
tseq (c : r) f = Trans (Just c) (tseq r f)

tmseq :: [String] -> TFormula -> TFormula
tmseq [] _ = FF
tmseq (s : r) f = tseq s f `Add` tmseq r f

tsum :: [A] -> B -> TFormula
tsum [] _ = FF
tsum (a : r) b = Out a b `Add` tsum r b

mcall :: [A]
mcall = Nothing : map Just ['\0'..'\FF']

mcnondigit :: [A]
mcnondigit = filter (\c -> not $ c `elem` map Just ['0'..'9']) mcall

mcnonalphanum :: [A]
mcnonalphanum = filter (\c -> not $ c `elem` map Just (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])) mcall

tsingle :: String -> Token -> TFormula
tsingle s t = tseq s (tsum mcall $ Val $ Token t)

tmulti :: [String] -> Token -> TFormula
tmulti [] _ = FF
tmulti (s : r) t = tsingle s t `Add` tmulti r t

tassign = tsingle "=" TAssign
tsemicolon = tsingle ";" TSemicolon
tcomma = tsingle "," TComma
tleftparenthesis = tsingle "(" TLeftParenthesis
trightparenthesis = tsingle ")" TRightParenthesis
tleftcbracket = tsingle "{" TLeftCBracket
trightcbracket = tsingle "}" TRightCBracket
tleftbracket = tsingle "[" TLeftBracket
trightbracket = tsingle "]" TRightBracket
tvoid = tsingle "Void" TVoid
tif = tsingle "if" TIf
telse = tsingle "else" TElse
twhile = tsingle "while" TWhile
treturn = tsingle "return" TReturn
tnil = tsingle "nil" TNil
tbool = tmulti ["True", "False"] TBool
tbasictype = tmulti ["Int", "Bool", "Char"] TBasicType
top1 = tmulti ["!", "-"] TOp1
top2 = tmulti ["+", "-", "*", "/", "%", "==", "<", ">", "<=", ">=", "!=", "&&",
               "||", ":"] TOp2
tchar = tmulti (map (\c -> ['\'', c, '\'']) ['0'..'9']) TChar
tfield = Nu 0 $ tsum (filter (\c -> c /= Just '.') mcall) (Val $ Token TField) `Add` tmseq [".hd", ".tl", ".fst", ".snd"] (Var 0)
tint = tmseq (map (\c -> ['-', c]) ['0'..'9']) $ Nu 0 $ tsum mcnondigit (Val $ Token TInt) `Add` tmseq (map (\c -> [c]) ['0'..'9']) (Var 0)
tid = tmseq (map (\c -> [c]) (['a'..'z'] ++ ['A'..'Z'])) $ Nu 0 $ tsum mcnonalphanum (Val $ Token TId) `Add` tmseq (map (\c -> [c]) (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])) (Var 0)
