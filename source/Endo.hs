module Endo where
import State
import Control.Monad

type Endo a = State a ()

eId :: Endo a
eId = st id

globalize :: (b -> a) -> (a -> b -> b) -> Endo a -> Endo b
globalize f g (ST h) = ST $ \t -> case h (f t) of
    Left (x, t') -> Left (x, g t' t)

endoSeq :: (a -> Endo b) -> [a] -> Endo b
endoSeq f = foldM (const f) ()
