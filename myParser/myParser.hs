module MyParser where

data Expr = Con Int | Div Expr Expr

-- | Create a monad for interpreting
data E a = Value a | Wrong
    deriving (Show)

interp :: Expr -> E Int 
interp (Con i)     = Value i
interp (Div e1 e2) = case interp e1 of
                        Wrong   -> Wrong
                        Value x -> case interp e2 of
                                        Wrong   -> Wrong
                                        Value y -> if y == 0 then Wrong
                                                             else Value $ x `div` y
-- | Use a Maybe for it
interp' :: Expr -> Maybe Int 
interp' (Con i)     = Just i
interp' (Div e1 e2) = case interp' e1 of
                        Nothing   -> Nothing
                        Just x -> case interp' e2 of
                                        Nothing   -> Nothing
                                        Just y -> if y == 0 then Nothing
                                                             else Just $ x `div` y

e0 :: Expr
e0 = Div (Con 2) (Con 2) 

e1 :: Expr
e1 = Div (Con 2) (Con 0)   

e2 :: Expr
e2 = Div (Con 2) (Con 3)   