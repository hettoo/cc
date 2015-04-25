module Endo where
import State
import Control.Monad

type Endo a = State a ()

eId :: Endo a
eId = st id

globalize :: (b -> a) -> (a -> b -> b) -> (a -> a) -> Endo b
globalize f g h = st $ \b -> g (h (f b)) b

endoSeq :: (a -> Endo b) -> [a] -> Endo b
endoSeq f = foldM (const f) ()
