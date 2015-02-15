module Tokenizer where
import Mealy
import MealyFormula
import MealyExpression
import JSL
import Utils

type Var = Int

instance Fresh Int where
    fresh f = case freshest f of
        Just n -> n + 1
        Nothing -> 0

type FPos = (Int, Int)

index :: FullMealy TChar FPos FPos
index = ((1, 1), \p a -> (p, advance a p))
    where
    advance a (l, c) = case a of
        Char '\n' -> (l + 1, 1)
        _ -> (l, c + 1)

data CommentClass = Slash
                  | Star
                  | Newline
                  | StarSlash
                  | NotSlash
                  | NotStar
                  | NotNewline
                  | NotStarNotSlash
                  deriving (Eq, Enum, Bounded, Show)

commentClassChars :: CommentClass -> TChar -> Bool
commentClassChars c a = case c of
    Slash -> a == Char '/'
    Star -> a == Char '*'
    Newline -> a == Char '\n'
    StarSlash -> commentClassChars Slash a || commentClassChars Star a
    NotSlash -> not $ commentClassChars Slash a
    NotStar -> not $ commentClassChars Star a
    NotNewline -> not $ commentClassChars Newline a
    NotStarNotSlash -> not $ commentClassChars StarSlash a

data CommentMark = Start
                 | Abort
                 | End
                 deriving (Eq, Show)

type CML = SimpleLattice CommentMark

commentMarker :: MealyFormula CommentClass CML Var
commentMarker =
    star
        (star (skip [NotSlash]) .*. [Slash] .|. Start .*.
            ([NotStarNotSlash] .|. Abort
            .+. (skip [Slash] .*. star (skip [NotNewline]) .*.
                [Newline] .|. End)
            .+. skip [Star] .*.
                star (skip [NotStar] .+. skip [Star, NotSlash]) .*.
                [Star, Slash] .|. End))

commentMarks :: String -> [CML]
commentMarks = (trace $ mealyList $ synthesize commentMarker)
    . fmap (classify commentClassChars) . tstring

unabort :: [CML] -> [CML]
unabort l = case l of
    [] -> []
    (a : Val Abort : r) -> Bottom : Bottom : unabort r
    (a : r) -> a : unabort r
