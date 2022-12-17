--[ x^2 |x <- [1..10], x `rem` 3 == 2]
--[4,25,64]

--[(x,y)| x<- [1..5], y <- [x..(x+2)]]
--[(1,1),(1,2),(1,3),(2,2),(2,3),(2,4),(3,3),(3,4),(3,5),(4,4),(4,5),(4,6),(5,5),(5,6),(5,7)]

--[(x,y)| x<-[1..3], let k = x2, y <- [1..k]]
--[(1,1),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9)]

--[ x | x<- "Facultatea de Matematica si Informatica", elem x ['A'..'Z']]
--"FMI"

--[[x..y]| x <- [1..5], y <- [1..5], x < y]
--[[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[2,3],[2,3,4],[2,3,4,5],[3,4],[3,4,5],[4,5]]

--1
factori :: Int -> [Int]
factori x = [nr | nr <- [1..x], mod x nr == 0]

--2
prim :: Int -> Bool
prim x = length (factori x) == 2

--3
numerePrime :: Int -> [Int]
numerePrime n = [nr | nr <- [2..n] , prim nr]

--[(x,y)| x <- [1..5], y <- [1..3]]
--[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3),(4,1),(4,2),(4,3),(5,1),(5,2),(5,3)]

--zip [1..5] [1..3]
--[(1,1),(2,2),(3,3)]

--4
--myzip3 [1,2,3] [1,2] [1,2,3,4] == [(1,1,1),(2,2,2)]

myzip3 :: [Int] -> [Int] -> [Int]-> [(Int,Int, Int)]
myzip3 _ _ [] = []
myzip3 _ [] _ = []
myzip3 [] _ _ = []
myzip3 (a:b) (c:d) (e:f) = 
    (a,c,e) : myzip3 b d f
-- myzip3 _ _ _ = []


--MAP
--map (* 3) [1,3,4]
--[3,9,12]

--map ($ 3) [ ( 4 +) , (10 * ) , ( ^ 2) , sqrt ]
--[7.0,30.0,9.0,1.7320508075688772]

--map (\x -> 2 * x) [1..10]
--[2,4,6,8,10,12,14,16,18,20]

--map (1 `elem`) [[2,3], [1,2]]
--[False,True]

--map (`elem` [2,3]) [1,3,4,5]
--[False,True,False,False]


--FILTER
--filter :: (a -> Bool) -> [a] -> [a]
--filter p xs = [x | x <- xs, p x]

--filter (>2) [3,1,4,2,5]
--[3,4,5]
-- filter odd [3,1,4,2,5]
--[3,1,5]

--5
firstEl :: [(a,b)] -> [a]
firstEl xs = [x | (x,_) <- xs]


--firstEl [('a',3),('b',2), ('c',1)]
--"abc"

--6
suma :: [Int] -> Int
suma [] = 0
suma (a:b) = a + suma b

sumList1 :: [[Int]] -> [Int]
sumList1 [] = []
sumList1 (a:b) =  suma a : sumList b

--______________cu MAP____________
sumList :: [[Int]] -> [Int]
sumList l = map suma l


--sumList [[1,3], [2,4,5], [], [1,3,5,6]]
--[4,11,0,15]

--7
prel :: [Int] -> [Int]
prel [] = []
prel (a:b) = 
    if a `rem` 2 == 0
        then a `div` 2 : prel b
    else
        a*2 : prel b

-- _______________cu MAP____________
f7 :: Int -> Int
f7 x = 
    if x `rem` 2 == 0
        then x `div` 2
    else
        x*2

prel2 :: [Int] -> [Int]
prel2 l = map f7 l

--prel2 [2,4,5,6]
--[1,2,10,3]

--8
functie8 :: Char -> [String] -> [String]
functie8 c l = filter (elem c) l

--9
functie9 :: [Int] -> [Int]
functie9 l = map (\x -> x*x) (filter odd l)

--10
f :: [Int] -> [Int]
f l=[x|(i,x)<-zip[1..]l, odd i]

functie10 :: [Int] -> [Int]
functie10 l = map (\x -> x*x)(f l)

--11
f11 :: Char -> Bool
f11 x = x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u'

noC:: String -> String
noC xs = filter f11 xs 

numaiVocale :: [String] -> [String]
numaiVocale = map noC 

--12. Definiti recursiv functiile mymap si myfilter cu aceeasi functionalitate ca si functiile predefinite.

mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = f x : mymap f xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x:xs)
  | f x       = x : (myfilter f xs)
  | otherwise = myfilter f xs