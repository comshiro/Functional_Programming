

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

height :: Arb -> Int
height Frunza = 0
height (Nod x Frunza Frunza) = 1
height (Nod x l r) = if (1 + (height l)> 1 + (height r)) 
                            then  1 + (height l) 
                            else 1 + (height r)

--isAVL :: Arb -> Bool

--rotateLeft :: Arb -> Arb 
--rotateRight :: Arb -> Arb 

--doubleRotateLeft :: Arb -> Arb 
--doubleRotateRight :: Arb -> Arb 

--echilibrare :: Arb -> Arb 

--insertAVL :: Arb -> Integer -> Arb 

--removeAVL :: Arb -> Integer -> Arb 

----------------------------------------------

data Nat = Zero | Succ Nat deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add Zero y = y
add (Succ x) y = Succ (add x y)

scadere :: Nat -> Nat -> Nat
scadere y Zero = y
scadere Zero y = Zero
scadere (Succ x) (Succ y) = scadere x y 

mult :: Nat -> Nat -> Nat
mult Zero y = Zero
mult (Succ Zero) y = y  
mult (Succ x) y = add (add y y ) (mult (scadere x (Succ Zero)) y)

exp' :: Nat -> Nat -> Nat
exp' y (Succ Zero) = y 
exp' y Zero = Succ Zero 
exp' x y = mult x (exp' x (scadere x (Succ Zero)))



comp :: Nat -> Nat -> Bool -- comp x y inseamna "x < y"
comp  Zero Zero = False
comp x Zero = False
comp Zero x = True 
comp x y = if( scadere x y  == Zero) then False 
            else if( scadere x y == Zero) then True 
            else False

--impartire :: Nat -> Nat -> Nat
--rest :: Nat -> Nat -> Nat
--convert :: Nat -> Int
--convert' :: Int -> Nat

-------------------------------------
--data Var = "x" | "y" | "z" | "x'" | "y'" | "z'"| "x0"|"y0"|"z0"
data Boolean = T | F | AND Boolean Boolean| OR Boolean Boolean | NOT Boolean | Var String deriving Show

type Assignment = [(String, Bool)]

assignment :: Assignment
assignment = [("x", True), ("y", False), ("z'", True)]

lookup' :: Assignment -> String -> Bool
lookup' [] _ = False 
lookup' ((var,val):tl) x = if x == var then 
                            val 
                        else lookup' tl x

eval :: Boolean -> Bool
eval T = True
eval F = False 
eval (Var a) = lookup' assignment a
eval (AND x y) = (eval x) && (eval y)   
eval (OR x y) = (eval x) || (eval y)
eval (NOT x) = not (eval x) 

simpl :: Boolean -> Boolean
simpl T =T 
simpl F = F 
simpl (Var a) = Var a 
simpl (AND x y) = 
    let x' = simpl x in 
    let y' = simpl y in 
        case(x', y') of
            (T, T) -> T 
            (_ ,_) -> F 
simpl (OR x y) = 
    let x' = simpl x in 
    let y' = simpl y in 
        case(x', y') of
            (F, F) -> F 
            (_ ,_) -> T 
simpl (NOT x) = NOT (simpl x)

--FNC - conjunctii de disjunctii

--fnc :: Boolean -> Boolean 
--fnc (NOT( OR x y) = (AND X Y)

