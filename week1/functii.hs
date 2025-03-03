id :: Int -> Int
id x = x 

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

produs3 :: Int -> Int -> Int -> Int
produs3 x y z = x * y * z

myMax :: Int -> Int -> Int
myMax x y = if x <= y then y else x

max3 :: Int -> Int -> Int -> Int
max3 x y z = if x <= y then
                if y <= z then z else y
            else
                if x <= z then z else x

mySum :: Int -> Int
mySum x = if x <= 0 then 0 else x + mySum (x - 1)

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib x = (fib (x-1)) + (fib (x-2)) 

cmmdc :: Int -> Int -> Int
cmmdc a b = if a == b then a else
            if a > b then cmmdc (a-b) b else cmmdc a (b-a)
