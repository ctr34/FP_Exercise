doubleOne x = x + x
doubleMe x y = x + doubleOne y
doubleSmall x = if x > 50 
                then x
                else 2*x 

listAdd :: Num a => [a] -> [a]
listAdd xs = [x+2 | x <- xs]           

listDisplay :: [a] -> String
listDisplay xs = "listDisplay: " ++ what xs 
    where what [] = "This is a empty list!"
          what (x:_) = "Head is "

compMany a b c = a > b

getMax :: Ord a => [a] -> a
getMax [] = error "Empty list..."
getMax [a] = a
getMax (x:xs) = max x (getMax xs)

rep n x 
    | n <= 0 = []
    | otherwise = x : rep (n-1) x

qicksort :: Ord a => [a] -> [a]
qicksort [] = []      
qicksort (x:xs) = 
    let smaller = qicksort [s | s <- xs, s <= x]
        bigger = qicksort [s | s <- xs, s > x]
    in smaller ++ [x] ++ bigger

multiThree :: Num a => a -> a -> a -> a
multiThree x y z = x*y*z

applytwice :: (t -> t) -> t -> t
applytwice f x = f (f x)

--TODO: fold composition