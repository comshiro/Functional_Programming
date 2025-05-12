

data Culori = Violet
            |Galben
            |Verde
            deriving(Show)

data MobileDevice = Smartphone Culori
                    | Laptop Culori
                    | Tablet Int String Culori
                    deriving(Show)

descriere :: MobileDevice -> String
descriere (Laptop _) = "Acesta este un laptop de culoare roz."
descriere (Tablet _ _ _) = "Aceasta este o tableta mov."
descriere (Smartphone _)= "Acesta este un telefon mobil."

getColor :: MobileDevice -> Culori
getColor (Laptop c) = c
getColor (Smartphone c) = c
getColor (Tablet _ _ c) = c

data Arb = Frunza | Nod Integer Arb Arb deriving (Show, Eq)

minBST :: Arb -> Integer
minBST (Nod i Frunza _)= i
minBST (Nod i (Nod j k l) Frunza) = minBST (Nod j k l)

maxBST :: Arb -> Integer
maxBST (Nod i _ Frunza)= i
maxBST (Nod i Frunza (Nod j k l)) = maxBST (Nod j k l)

isBST :: Arb -> Bool
isBST Frunza = True
isBST (Nod i Frunza Frunza) = True
isBST ( Nod i j Frunza) = (isBST j)  && ((maxBST j) <i ) 
isBST ( Nod i Frunza j) = (isBST j) && ((minBST j) > i ) 
isBST (Nod i j k) = (isBST j) && (isBST k) && ((maxBST j) < i) && ((minBST k) > i) 

search :: Arb -> Integer -> Bool 
search (Nod i j k) z = if (i  == z) then True
                        else (search j z) || (search k z)
search Frunza j = False

insert::Arb->Integer->Arb
insert Frunza x = Nod x Frunza Frunza
insert (Nod i j k) l = if( l < i) then (Nod i (insert j l) k) 
                        else (Nod i j (insert k l)) 

removeMax :: Arb -> Arb
removeMax Frunza = Frunza
removeMax (Nod i Frunza Frunza) = Frunza
removeMax (Nod i j (Nod k Frunza Frunza)) = (Nod i j Frunza)
removeMax (Nod i j k) = (Nod i j (removeMax k))

remove :: Arb -> Integer -> Arb
remove Frunza _ = Frunza
remove (Nod i Frunza Frunza) x
    | i == x    = Frunza
    | otherwise = Nod i Frunza Frunza
remove (Nod i Frunza k) x
    | x < i     = Nod i Frunza k
    | x > i     = Nod i Frunza (remove k x)
    | otherwise = k
remove (Nod i j Frunza) x
    | x < i     = Nod i (remove j x) Frunza
    | x > i     = Nod i j Frunza
    | otherwise = j
remove (Nod i j k) x
    | x < i     = Nod i (remove j x) k
    | x > i     = Nod i j (remove k x)
    | otherwise = Nod (maxBST j) (removeMax j) k

preOrder :: Arb -> [Integer]
preOrder Frunza = []
preOrder (Nod i j k) = [i] ++ preOrder j ++ preOrder k

inOrder :: Arb -> [Integer]
inOrder Frunza = []
inOrder (Nod i j k) = inOrder j ++ [i] ++ inOrder k

postOrder :: Arb -> [Integer]
postOrder Frunza = []
postOrder (Nod i j k) = postOrder j ++ postOrder k ++ [i]