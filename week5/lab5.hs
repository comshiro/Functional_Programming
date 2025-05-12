f :: (Int, Int) -> Int
f (x,y) = x + y

g :: Int -> Int -> Int
g x y = x + y

curry' :: ((Int, Int) -> Int) -> (Int ->Int -> Int)
curry' f = \ x y -> f (x,y)

addThree :: (Int, Int, Int) -> Int
addThree (x,y,z) = x + y + z

addThree_curried :: Int ->Int ->Int ->Int
addThree_curried a = \ b -> (\c -> a + b +c ) 

process :: (Int -> Int) -> Int -> Int
process f x = f x


ex2_1 :: (Int -> Int) -> Int -> Int -> Int 
ex2_1 f a b = if a < b then (f a) + (ex2_1 f (a+1) b)  
                else 0

compf :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
compf f g x = f (g x)

compf_list:: [(a -> a)] -> (a -> a)
compf_list [] = id
compf_list (hd : tl) = (.) hd (compf_list tl)

sum_list:: [Int] -> Int
sum_list [] = 0
sum_list (hd:tl) = hd + sum_list tl 

apply :: [a] -> (a->a) -> [a]
apply [] _ = []
apply (hd : tl) f = (f hd) : (apply tl f)

check_true :: [a] -> (a-> Bool) -> [a]
check_true (hd:tl) f = filter f (hd:tl) 

data List = Empty | Cons Int List deriving Show

foldl' ::  (Int ->Int->Int) -> Int -> List-> Int 
foldl' _ x Empty = x
foldl' f x (Cons hd tl) = foldl' f (f x hd) tl

data Arb = Frunza | Nod Int Arb Arb deriving (Show, Eq)

preOrder:: Arb -> (Int -> Int)-> [Int]
preOrder Frunza f = [] 
preOrder (Nod i j k) f = [f i] ++ preOrder j f ++ preOrder k f 

inOrder:: Arb -> (Int -> Int)-> [Int]
inOrder Frunza f = []
inOrder (Nod i j k) f= inOrder j f ++ [f i] ++ inOrder k f

postOrder:: Arb -> (Int -> Int) -> [Int]
postOrder Frunza f = []
postOrder (Nod i j k) f= postOrder j f ++ postOrder k f++ [f i ]

parcurgere_arb :: Arb -> ( Arb -> (Int -> Int) -> [Int] )-> [Int]
parcurgere_arb arb f = f arb id

sort' :: [a] -> (a->a->Bool) -> [a] 
sort' (hd:(hd' : tl)) f = if (f hd hd' ) then 
                            (hd' : (hd:tl))
                            sort' (hd' : tl) f 
                        else 
                           sort' (hd' : tl) f  

data Arb = Frunza | Nod t Arb Arb deriving (Show, Eq)
--compara elem t 
