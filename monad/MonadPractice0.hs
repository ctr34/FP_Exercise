module MonadPractice where

data Expr = Con Int | Div Expr Expr

-- * Monadic interpreter by Maybe
mInterp :: Expr -> Maybe Int
mInterp (Con i) = Just i
mInterp (Div e1 e2) = 
    mInterp e1 >>= \i1 -> 
        mInterp e2 >>= \i2 ->
            (if i2 == 0 then Nothing else return (i1 `div` i2))  

-- * Monadic interpreter by custom Monad
data E a = Value a | Wrong
    deriving Show

class Monad' m where
    return' :: a -> m a
    (>>==)  :: m a -> (a -> m b) -> m b  

instance Monad' E where
    return'        = Value
    Wrong >>== f   = Wrong
    Value a >>== f = f a

abort :: E a
abort = Wrong

mInterp' :: Expr -> E Int
mInterp' (Con i) = return' i
mInterp' (Div e1 e2) = 
    mInterp' e1 >>== \i1 -> 
        mInterp' e2 >>== \i2 ->
            (if i2 == 0 then abort else return' (i1 `div` i2))  

e0 = Div (Con 2) (Con 0)            
e1 = Div (Con 5) (Con 2)