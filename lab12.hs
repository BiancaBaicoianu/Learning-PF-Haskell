import Data.Monoid

--1. Implementati următoarele functii folosind foldMap si/sau foldr din clasa Foldable,
--apoi testati-le cu mai multe tipuri care au instantă pentru Foldable


-- Any x <> Any y = Any ( x | | y )

--daca x e element al unei liste date
elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
--elem1 x l = foldr(\a val -> a == x || val) False l
elem1 x l = getAny(foldMap (\a -> Any (a==x))  l)

--t11 = elem1 2 [1,2,3,4] == True
--t12 = elem1 'a' [ 'b','c'] == False

--verifica daca o lista data e goala
null1 :: (Foldable t) => t a -> Bool
--null1 l = foldr(\a val -> False) True l
null1 l = getAll(foldMap (\a -> All False) l)

--t21 = null1 [1,2,3,4] == False
--t22 = null1 [] == True

--returneaza lungimea unei liste
length1 :: (Foldable t) => t a -> Int
length1 l = foldr(\a val -> 1 + val) 0 l
--length1 l = getSum(foldMap (\a -> Sum 1) l)


--t31 = length1 [1,2,3,4] == 4
--t32 = length1 [] == 0

--face lista din al doilea element din pereche
toList1 :: (Foldable t) => t a -> [a]
--toList1 l = foldr(\a val -> a : val) [] l
toList1 l = foldMap (\a -> [a]) l

--t41 = toList1 (1,2) == [2]
--t42 = toList1 (1,2,3,4) == [2,3,4]

--combină elementele unei structuri folosind structura de monoid a acestora.
-- Hint: folosiți foldMap
fold1 :: (Foldable t, Monoid m) => t m -> m
fold1 = foldMap id

--t51 = fold1 [Sum 21, Sum 2, Sum 8] == Sum {getSum = 31}

--2. Scrieti instante ale lui Foldable pentru următoarele tipuri, implementand functia foldMap.

data Constant a b = Constant b deriving (Show)
instance Foldable (Constant a) where
    foldMap f (Constant b) = f b
-- t1 = foldMap Sum (Constant 1) == Sum {getSum = 1}

data Two a b = Two a b deriving (Show)
instance Foldable (Two a) where 
    foldMap f (Two a b) = f b
-- t1 = foldMap Sum (Two 1 2) == Sum {getSum = 2}

data Three a b c = Three a b c deriving (Show)
instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c
--t1 = foldMap Sum (Three 1 2 3) == Sum {getSum = 3}

data Three' a b = Three' a b b deriving (Show)
instance Foldable (Three' a) where
    foldMap f (Three' a b1 b2) = f b1 <> f b2
-- t1 = foldMap Sum (Three' 1 2 3) == Sum {getSum = 5}

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving Show
instance Foldable GoatLord where
    foldMap f NoGoat = mempty
    foldMap f (OneGoat a) = f a
    foldMap f (MoreGoats a b c) = foldMap f a <> foldMap f b <> foldMap f c

--t1= foldMap Sum (MoreGoats (OneGoat 1) (OneGoat 2) (OneGoat 3)) == Sum {getSum = 6}
