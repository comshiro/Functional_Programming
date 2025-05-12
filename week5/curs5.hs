--high order functions -take functions as input, or the output is a function
--lamda notation: g = (\ x -> x+10)

add:: Int -> Int -> Int 
--unoficially takes 2 arguments
add x y = x + y 
--Officially
-- add x = \ y -> x+y 

app :: String -> String -> String 
app a b =a ++ b 

swap :: (String -> String -> String ) -> (String -> String -> String )
swap f = \ s1 s2 -> f s2 s1

mystery :: (Int -> Int) -> Int 
mystery f = f 0 

adder :: Int -> (Int -> Int)
adder step = \ x -> x + step 

g = adder 3
g'  = adder 10 

--"curried " functions 
add3 :: Int -> (Int->(Int->Int))
add3 x y z= x+y+z 

add2 :: (Int, Int) -> Int 
add2 (x,y) = x+y --uncurried function 

curry' :: ((Int, Int) -> Int) -> (Int ->Int -> Int)
curry' f = \ x y -> f (x,y)

mystery' f g x = f (g x)
