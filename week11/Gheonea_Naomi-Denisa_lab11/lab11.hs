import Prelude hiding (Left, Right)
data Tree = Leaf | Node Int Tree Tree deriving (Show, Eq, Ord)

t1 :: Tree
t1 = Node 5
  (Node 3
   (Node 1 Leaf Leaf)
   (Node 7 (Node 10 Leaf Leaf) (Node 20 Leaf Leaf)))
  (Node 42 (Node 8 Leaf Leaf) Leaf)

data Crumb = L Int Tree | R Int Tree deriving (Show, Eq, Ord)

type Trail = [Crumb]

type Zipper = (Tree, Trail)

z1 :: Zipper
z1 = (t1, [])

goLeft :: Zipper -> Maybe Zipper
goLeft (Leaf, _) = Nothing
goLeft (Node x l r, trail) = Just (l, L x r : trail)

goRight :: Zipper -> Maybe Zipper
goRight (Leaf, _) = Nothing
goRight (Node x l r, trail) = Just (r, R x l : trail)

update :: Int -> Zipper -> Maybe Zipper
update _ (Leaf, _) = Nothing
update v (Node x l r, trail) = Just (Node v l r, trail)

goUp :: Zipper -> Maybe Zipper
goUp (t, []) = Nothing
goUp (t, L x r : trail) = Just (Node x t r, trail)
goUp (t, R x l : trail) = Just (Node x l t, trail)


example1 :: Maybe Zipper
example1 = Just z1 >>= goLeft >>= goLeft >>= update 12 >>= goUp

example2 :: Maybe Zipper
example2 = Just z1 >>= goRight >>= goRight >>= update 1 >>= goUp >>= goUp

example3 :: Maybe Zipper
example3 = Just z1 >>= goRight >>= goRight >>= update 1 >>= goUp

example4 :: Maybe Zipper
example4 = Just z1 >>= goRight >>= update 1 >>= goUp


l1 :: [Int]
l1 = [ 3, 7, 4, 6, 42 ]

type ZipperList = ([Int], [Int])

goFwd :: ZipperList -> Maybe ZipperList
goFwd ([], _) = Nothing
goFwd (hd:tl, trail) = Just (tl, hd : trail)

update'' :: Int -> ZipperList -> Maybe ZipperList
update'' _ ([], _) = Nothing
update'' v (hd:tl, trail) = Just (v:tl, trail)

goBwd :: ZipperList -> Maybe ZipperList
goBwd (_, []) = Nothing 
goBwd (l, x : trail) = Just (x : l, trail)

example5 = Just (l1, []) >>= goFwd >>= goFwd >>= update'' 0 >>= goBwd
example6 = Just (l1, []) >>= goFwd >>= goFwd >>= update'' 0 >>= goFwd  >>= goFwd  >>= goFwd 

data Tree2 = Nod Int [Tree2] deriving (Show, Eq)


data CrumbTree = Down Int| Right Tree deriving (Show, Eq)

type TrailTree = [CrumbTree]

type ZipperTree = ([Tree2], TrailTree)

t = Nod 10 [Nod 3 [],
 Nod 4 [Nod 1 [], Nod 2 [], Nod 22 [], Nod 33[] ],
 Nod 5 [], Nod 6 []]

data Dir = D| Ri deriving (Show, Eq)

atPos :: [Tree2] -> [Dir] -> Int
atPos (Nod x _ : _) [] = x
atPos (Nod _ target : _) (D: tl) = atPos target tl
atPos (_ : rest) (Ri: tl) = atPos rest tl

goDown :: Zipper-> Zipper 
goDown ([], _) = error "Cannot go down in leaf."
goDown ( (Node x (t:tl) : tl'), trail) = (  ,(t:tl), Down _ : trail)

--goRight :: ZipperTree-> ([Tree], [Crumb])
--goRight ([], _) = error " "

--change :: Int -> ZipperTree-> ZipperTree
--change _ (Node [], ) 

--goBack :: ZipperTree-> ZipperTree
--goBack (t, []) = error "Cannot go back (no crumbs)"
--goBack ( , Down x _ :trail) = (Node x parent, trail) 
--goBack (t, Right x _ :trail) = (Node x )


