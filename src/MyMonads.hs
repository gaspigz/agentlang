module MyMonads  where
import Data.Monoid

-- ==========================================
-- STATE
-- ==========================================

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f (State g) = State $ \s ->
        let (x, s') = g s
        in  (f x, s')

instance Applicative (State s) where
    pure x = State $ \s -> (x, s)
    (State f) <*> (State g) = State $ \s ->
        let (h, s')  = f s
            (x, s'') = g s'
        in  (h x, s'')

instance Monad (State s) where
    return = pure
    (State g) >>= f = State $ \s ->
        let (x, s') = g s
            State h = f x
        in  h s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put newS = State $ \_ -> ((), newS)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

-- ==========================================
-- WRITER
-- ==========================================

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
    fmap f (Writer (x, w)) = Writer (f x, w)

instance Monoid w => Applicative (Writer w) where
    pure x = Writer (x, mempty)
    (Writer (f, w)) <*> (Writer (x, w')) = Writer (f x, w `mappend` w')

instance Monoid w => Monad (Writer w) where
    return = pure
    (Writer (x, w)) >>= f =
        let Writer (y, w') = f x
        in  Writer (y, w `mappend` w')

tell :: w -> Writer w ()
tell w = Writer ((), w)

execWriter :: Writer w a -> w
execWriter (Writer (_, w)) = w