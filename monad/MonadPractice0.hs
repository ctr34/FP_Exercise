module MonadPractice where

import Control.Monad(ap,liftM,guard,join)

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

-- * Monadic interpreter with logging
newtype L a = L (a , [[Char]])
    deriving Show

interp' :: Expr -> L Int
interp' (Con i)     = L (i , ["Hit the constant " ++ show i ++ "\n"])
interp' (Div e1 e2) = L (i1 `div` i2,
                        "Hit the Div\n":
                        "Left recurcive\n":
                        msgL ++
                        "Right recurcive\n":
                        msgR
                        )
        where L (i1, msgL) = interp' e1
              L (i2, msgR) = interp' e2
            
runL :: Expr -> IO ()
runL e = do
        print $ show i
        putStrLn $ concat msgs
    where L (i, msgs) = interp' e

e2 = Div (Div (Con 5) (Con 2)) (Con 2)    

-- * Monadic interpreter with logging with custom Monad
instance Monad' L where
    return' a = L (a, [])
    L (x, msg) >>== f = case f x of
                        L (y, msg1) -> L (y,msg ++ msg1)

msg :: String -> L () 
msg s = L ((), [s])

interp'' :: Expr -> L Int
interp'' (Con i)     = 
    msg ("Hit Con" ++ show i ++ "\n") >>== \_ -> return' i
interp'' (Div e1 e2) =
    msg "Left recurcive\n" >>== \_ ->
        interp'' e1 >>== \r1 ->
            msg "Right recurcive\n" >>== \_ ->
                interp'' e2 >>== \r2 ->
                    return' (r1 `div` r2)

runL' e = do
        print ("The result: " ++ show i)
        putStrLn $ concat msgs
    where L (i, msgs) = interp'' e                    