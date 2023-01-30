-- sub 1 [ADT + Clase] 2p
data Point = Pt [Int]
  deriving Show

data Arb = Empty | Node Int Arb Arb
  deriving Show

class ToFromArb a where
  toArb :: a -> Arb
  fromArb :: Arb -> a

instance ToFromArb Point where
  toArb (Pt n) = insert n Empty
    where
      insert :: [Int] -> Arb -> Arb
      insert [] arb = arb
      insert (x:xs) arb = insert xs (insertNode x arb)

      insertNode :: Int -> Arb -> Arb
      insertNode x Empty = Node x Empty Empty
      insertNode x (Node y s d)
        | x <= y = Node y (insertNode x s) d
        | x > y = Node y s (insertNode x d)

  fromArb Empty = Pt []
  fromArb (Node x s d) = Pt (x:(toList s) ++ (toList d))
    where
      toList :: Arb -> [Int]
      toList Empty = []
      toList (Node x s d) = x:(toList s) ++ (toList d)

-- toArb (Pt [1,2,3])
-- Node 1 Empty (Node 2 Empty (Node 3 Empty Empty))

-- fromArb (Node 1 Empty (Node 2 Empty (Node 3 Empty Empty))) :: Point          !!! fara :: nu stie in ce sa-l duca
-- Pt [1,2,3]

--sub 2 [Liste+Monade] 3p
--Sa se scrie o functie care primeste doua numere intregi si o lista de numere intregi si construieste din
--lista initiala, lista numerelor aflate in intervalul definit de cele doua numere. Sa se rezolve problema in
--doua moduri (o solutie fara monade si o solutie cu monade).

getFromInterval :: Int -> Int -> [Int] -> [Int]
getFromInterval a b = filter (\x -> x >= a && x <= b)

getFromInterval2 :: Int -> Int -> [Int] -> [Int]
getFromInterval2 a b = foldr (\x y -> if x >= a && x <= b then x:y else y) []

