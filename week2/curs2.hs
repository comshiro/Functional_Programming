--referential transparency

f::Int->Int
f x = x+13

add::Int->Int->Int
add x y = x +y 

-- pure programming  = referntial transparency
--ghci is a REPL

--compute product of the first n positive integers

prod :: Int-> Int
prod 0 = 1
prod n = n * (prod(n-1))


--tail-cal optimisation -- after the call there is nothing to do but pass on the result
--         n       a
prod'' :: Int -> Int -> Int
prod'' 0 a = a
prod'' n a = prod'' (n-1) (a*n) -- a = acumulator 
--prod'' 5 1 = prod'' 4 5 = prod'' 3 (5*4) ==....= prod'' 0 (5*4*3*2*1) = 120

fibo::Int->Int

fibo 0 = 0
fibo 1= 1
fibo n =fibo (n-1) + fibo(n-2)

fibo_aux ::Int->Int->Int->Int
fibo_aux 0 a b =a
--fibo_aux 1 a b = b --can be removed
fibo_aux n a b = fibo_aux (n-1) b (a+b)

fibo_aux'::Int->Int->Int->Int
fibo_aux' 0 a b =a
--fibo_aux' 1 a b = b --can be removed
fibo_aux' n a b = fibo_aux' (n-1) b (a+b)



fibo'' n = fibo_aux n 0 1

--equational reasoning

-- show, using eq reasoning that prod and prod'' compute the same value

{-
prod :: Int-> Int
prod 0 = 1
prod n = n * (prod(n-1))

prod'' :: Int -> Int -> Int
prod'' 0 a = a
prod'' n a = prod'' (n-1) (a*n) -- a = acumulator 

prove for all n, prod n = prod'' n 1
by induction on n:

Base case: n = 0
prod 0 = 1
prod'' 0 1 = a = 1

Inductive case (n>0):
IH: prod(n-1) = prod '' (n-1) 1
prod n = n* (prod(n-1)) = n * prod '' (n-1) 1
prod'' n 1 = prod'' (n-1) n 

IT CANT BE PROVED BY INDUCTION 

prove that 

prove for all n, prod n = prod'' n a
by induction on n:

Base case: n = 0
a * prod 0 = a* 1
prod'' 0 a = a 

Inductive case (n>0):
IH: for all a, a* prod(n-1) = prod '' (n-1) a
a * prod n = a* n* (prod(n-1)) = (a*n) prod(n-1) =(IH)=
                                =prod'' (n-1) (a*n)

prod'' n a = prod'' (n-1) (a*n) 



-}

--Can we write a function that has 2 outputs?
--ANswer 1: no

--f1::Int->Int->Int->Int
--   i n p u t s    single output

--take an agle as I and output the point on the unit circle at that angle
--f2::Float->(Float, FLoat)

xCoord::Float -> Float
xCoord angle = cos (pi * angle/180)
yCoord::Float -> Float
yCoord angle = sin (pi * angle/180)

coords::Float->(Float, Float)
coords angle = (xCoord angle, yCoord angle)

succ' :: Int->Int
succ' x = x+1

pred' :: Int->Int
pred' x = x-1

add'::Int -> Int->Int
add' 0 y = y
add' x y = add' (pred' x) (succ' y)

--gcd_extended::Int->Int->(Int, Int, Int)
-- standard alg:given x y, compute d=gcd(x,y)
--extended alg: also computes a, b s.t a*x+b*y = d 


fst_3uple::(Int, Int, Int) -> Int
fst_3uple (x, _, _)=x