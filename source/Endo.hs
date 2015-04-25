module Endo where
import State
import Control.Monad

type Endo a = State a ()

eId :: Endo a
eId = st id

globalize :: (b -> a) -> (a -> b -> b) -> Endo a -> Endo b
globalize f g (ST h) = ST $ \b -> case h (f b) of
    Left (x, a) -> Left (x, g a b)
    Right e -> Right e

globalizef :: (b -> a) -> (a -> b -> b) -> (a -> a) -> Endo b
globalizef f g h = st $ \b -> g (h (f b)) b

endoSeq :: (a -> Endo b) -> [a] -> Endo b
endoSeq f = foldM (const f) ()
