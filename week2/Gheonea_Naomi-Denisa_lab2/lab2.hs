--ex1
and' :: Bool -> Bool -> Bool
and' False _ = False
and' _ False = False
and' _ _ = True

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ =False

neg' :: Bool -> Bool
neg' True = False
neg' False = True

nand' :: Bool -> Bool -> Bool
nand' a b = neg'( and' a b)

nor' :: Bool -> Bool -> Bool
nor' a b = neg'( nor' a b)

--q -> p ==> !q or p

impl'::Bool->Bool->Bool
impl' q p = or' (neg' q) p

dimpl' ::Bool->Bool->Bool
dimpl' q p = and' (impl' q p) (impl' p q)

--ex2
hasDivisors :: Integer -> Integer -> Integer -> Bool
hasDivisors n a b | a > b = False
hasDivisors n a b | mod n a == 0 = True
hasDivisors n a b  = hasDivisors n (a+1) b 

isPrime :: Integer -> Bool
isPrime n = neg' (hasDivisors n 1 (n-1))

cmmdc::Int->Int->Int
cmmdc a b | a == 0 = b
cmmdc a b | b == 0 = a
cmmdc a b | a > b = cmmdc (a-b) b
cmmdc a b | a < b = cmmdc a (b-a)
cmmdc a b | a==b  = a

cmmdc'::Int->Int->Int
cmmdc' a b | a==b = a
cmmdc' a b | and' (mod a 2 ==0) (mod b 2==0) = 2* (cmmdc' (div a 2) (div b 2))
cmmdc' a b | (mod a 2) == 0 = cmmdc' (div a 2) b
cmmdc' a b | (mod b 2) == 0 = cmmdc' a (div b 2)
cmmdc' a b = if a > b then cmmdc' (a-b) b
             else cmmdc' a (b-a)

tail_cmmdc':: Int->Int->Int->Int
tail_cmmdc' x y a | x==y = x * a 
tail_cmmdc' x y a | and' (mod x 2 ==0) (mod y 2==0) = tail_cmmdc' (div x 2) (div y 2) (2*a)
tail_cmmdc' x y a |(mod x 2) == 0 = tail_cmmdc' (div x 2) y a
tail_cmmdc' x y a |(mod y 2) == 0 = tail_cmmdc' x (div y 2) a
tail_cmmdc' x y a = if x > y then tail_cmmdc' (x-y) y a
             else tail_cmmdc' x (y-a) a

fibo::Int->Int
fibo 0 = 0
fibo 1= 1
fibo n =fibo (n-1) + fibo(n-2)

fiboaux ::Int->Int->Int->Int
fiboaux 0 a b =a
fiboaux n a b = fiboaux (n-1) b (a+b)

fibo' :: Int-> Int 
fibo' n = fiboaux n 0 1

--gcd_extended::Int->Int->(Int, Int, Int)
-- standard alg:given x y, compute d=gcd(x,y)
--extended alg: also computes a, b s.t a*x+b*y = d
--Pe internet e implementat cu let/where
{- gcdExtended :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExtended a 0 = (a, 1, 0)
gcdExtended a b = (g, y, x - (a `div` b) * y)
  where (g, x, y) = gcdExtended b (a `mod` b)-}

succ'::Int->Int
succ' x = x+1

pred'::Int->Int
pred' x = x-1

add'::Int->Int->Int
add' 0 y = y
add' x y = add' (pred' x) (succ' y)

prod'::Int->Int->Int
prod' a 1 = a 
prod' a b = a + (prod' a (pred' b))

exp' ::Int->Int->Int
exp' a 0 = 1
exp' a 1 = a 
exp' a b = prod' a (exp' a (b-1))

div' ::Int->Int->Int
div' a 1 = a
div' a b | a < b = 0
div' a b = 1 + div' (a-b) b  

mod' :: Int->Int->Int
mod' a b | a< b = a
mod' a b = mod' (a-b) b