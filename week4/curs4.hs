data ABC = Empty | Nod Maybe Integer ABC ABC deriving Show

t1:: ABC 
t1=Empty

t2:: ABC 
t2 = Nod 2 Empty Empty

t3 :: ABC
t3 = Nod 7 t2 (Nod 6 Empty Empty) 


minim :: ABC -> Maybe Integer
minim Empty = Nothing
minim (Nod x Empty r) = Just x
minim (Nod x l r) = minim l

maxim :: ABC -> Maybe Integer
maxim Empty = Nothing
maxim (Nod x l Empty) = Just x
maxim (Nod x l r) = maxim l

--minim e o fct partial definita

isABC :: ABC -> Bool
isABC Empty = True
isABC (Nod x l r) = (x > maxim l) && (x < minim r) && isABC l && isABC r 