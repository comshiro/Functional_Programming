
data Nat = Cons [Bool] deriving Show --deriving (Eq, Ord, Integral, Num)

instance Eq Nat where
    (==) (Cons []) (Cons []) = True
    (==) (Cons []) (Cons (True:y)) = False
    (==) (Cons []) (Cons (False:y)) = (==) (Cons []) (Cons y)
    (==) (Cons (True:y)) (Cons []) = False 
    (==) (Cons (False:y)) (Cons []) = (==) (Cons y) (Cons [])
    (==) (Cons (x:y)) (Cons (m:p)) = ((==) x m) && ((==) (Cons y) (Cons p))

instance Ord Nat where 
    (<=) (Cons []) (Cons []) = True
    (<=) (Cons []) (Cons (x:y)) = True
    (<=) (Cons (True:y)) (Cons []) = False
    (<=) (Cons (False:y)) (Cons []) = (<=) (Cons y) (Cons [])
    (<=) (Cons (x:y)) (Cons (m:p)) = if (==) x m then (<=) (Cons y) (Cons p) else (<=) x m 

aux :: Nat -> Nat -> Bool
aux (Cons (x:xs)) (Cons (y:ys)) 
    | x == y    = aux (Cons xs) (Cons ys)
    | otherwise = x <= y 



--instance Integral Nat where 
--instance Num

Complex a where
    real :: a-> Bool 
    imaginary :: a-> Bool 

instance Complex Num where 





--instance Complex Num where 
    --is Complex :: 