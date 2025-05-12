qs :: Ord a => [a] -> [a]
qs [] = []
qs (hd:tl) = qs (filter (<=hd) tl) ++ [ hd ] ++ qs (filter (>hd) tl)

minList :: Ord a => [a] -> a
minList l@(hd:tl) = (qs l) !! 0;

listaAux :: Integer -> [Integer]
listaAux i = i : listaAux (i + 1)

listaNat :: [Integer]
listaNat = listaAux 0

listExample :: Integer -> [Integer] -> [Integer]  
listExample i tl = if i> 0 then
                        listExample (i-1) (i:tl)         
                       else tl

--minList' :: Ord a => [a] -> [a]
--minList' (hd: (hd':tl)) = 

insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort [x] = [x]
insertSort (x:xs) = insert x (insertSort xs)

insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a [x]
    | a <= x    = a : [x]
    | otherwise = [x, a]
insert a (x:y:xs)
    | a <= x         = a : x : y : xs
    | a >= x && a <= y = x : a : y : xs
    | otherwise      = x : insert a (y:xs)

count :: [a] -> Int
count [] = 0
count (hd : tl) = 1 + count tl

maxList :: Ord a => [a] -> a
maxList l@(hd:tl) = (qs l) !! ((count l) - 1) 

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x - 2) + fib (x - 1) -- (*)

--fibList :: Integer -> [Integer]
--fibList 1 = [1]
--fibList 2 = [1,1]
--fibList i = ((hd + hd'):(hd:(hd':tl)))

maxList' :: Ord a => [a] -> a
maxList' l@(hd:tl) = (qs' l) !! 0

qs' :: Ord a => [a] -> [a]
qs' [] = []
qs' (hd:tl) = qs (filter (>=hd) tl) ++ [ hd ] ++ qs (filter (<hd) tl)

hasDivisors :: Int -> Bool 
