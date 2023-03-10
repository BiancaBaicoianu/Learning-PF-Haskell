data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)

--1.1 Să se instantieze clasa Show pentru tipul de date Expr, astfel încât să se afiseze mai simplu expresiile.

instance Show Expr where
    show (Const x) = show x
    show (x :+: y) = "(" ++ show x ++ "+" ++ show y ++ ")"
    show (x :*: y) = "(" ++ show x ++ "*" ++ show y ++ ")"

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)

-- 1.2. Să se scrie o functie evalExp :: Expr -> Int care evaluează o expresie determinând valoarea acesteia.

evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (x :+: y) = (evalExp x) + (evalExp y)
evalExp (x :*: y) = (evalExp x) * (evalExp y)

test11 = evalExp exp1 == 6
test12 = evalExp exp2 == 14
test13 = evalExp exp3 == 13
test14 = evalExp exp4 == 16

--1.3. Să se scrie o functie evalArb :: Tree -> Int 
--care evaluează o expresie modelată
--sub formă de arbore, determinând valoarea acesteia.

evalArb :: Tree -> Int
evalArb(Lf x) = x
evalArb (Node Add tree1 tree2) = evalArb(tree1) + evalArb(tree2)
evalArb (Node Mult tree1 tree2) = evalArb(tree1) * evalArb(tree2)

arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)
test21 = evalArb arb1 == 6
test22 = evalArb arb2 == 14
test23 = evalArb arb3 == 13
test24 = evalArb arb4 == 16

--1.4. Să se scrie o functie expToArb :: Expr -> Tree 
--care transformă o expresie în arborele corespunzător.

expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb(exp1 :+: exp2) = Node Add (expToArb exp1) (expToArb exp2)
expToArb(exp1 :*: exp2) = Node Mult (expToArb exp1) (expToArb exp2)

--expToArb (exp4 :+: exp1) = Node Add (Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3) (Lf 1))) (Lf 2)) (Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0) (Lf 5)))
--expToArb (exp1 :*: exp2) = Node Mult (Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0) (Lf 5))) (Node Mult (Lf 2) (Node Add (Lf 3) (Lf 4)))

-- clasa COLLECTION

--2.1. Adaugati definitii implicite (in functie de functiile celelalte) pentru
--a. keys
--b. values
--c. fromList

class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
      :: Ord key
      => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  keys = map fst. toList
  values :: c key value -> [value]
  values = map snd. toList
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value
  fromList [] = empty
  fromList(x:xs) = insert(fst x)(snd x)(fromList xs)

--2.2. Fie tipul listelor de perechi de forma cheie-valoare:

data PairList key value = PairList { getPairList :: [(key, value)] } deriving(Show)

--Faceti PairList instanta a clasei Collection.

instance Collection PairList where
  empty = PairList []
  singleton key value = PairList [(key, value)]
  insert key value (PairList l) = PairList (l ++ [(key,value)])
  clookup key (PairList []) = Nothing
  clookup key (PairList (x:xs))           -- cautam dupa cheie
    | fst x == key = Just (snd x)
    | otherwise = clookup key (PairList xs)
  delete key c = PairList (sterge key (getPairList c))
        where
            sterge _ [] = []
            sterge key (x:xs)
                | key == fst x = xs
                | otherwise = x:(sterge key xs)
  toList(PairList l) = l


--2.3. Fie tipul arborilor binari de cautare (ne-echilibrati):

data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

--Observati ca tipul valorilor este Maybe value. Acest lucru se face pentru a reduce timpul
--operatiei de stergere prin simpla marcare a unui nod ca fiind sters. Un nod sters va
--avea valoarea Nothing.
--Faceti SearchTree instanta a clasei Collection.

instance Collection SearchTree where
    empty = Empty
    singleton key value = BNode Empty key (Just value) Empty
    insert key value Empty = singleton key value
    insert key value (BNode l key1 value1 r)
        | key < key1 = BNode (insert key value l) key1 value1 r
        | otherwise = BNode l key1 value1 (insert key value r)
    clookup _ Empty = Nothing
    clookup key (BNode l key1 value1 r)   -- cautam dupa cheie
        | key == key1 = value1
        | key < key1 = clookup key l
        | otherwise = clookup key r
    delete key (BNode l key1 value1 r)      -- stergem dupa cheie
        | key == key1 = BNode l key Nothing r
        | key < key1 = BNode (delete key l) key1 value1 r
        | otherwise = BNode l key1 value1 (delete key r)
    toList Empty = []  
    toList (BNode l key (Just value) r) = toList l ++ [(key, value)] ++ toList r
    toList (BNode l key Nothing r) = toList l ++ toList r