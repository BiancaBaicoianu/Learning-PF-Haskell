--1) 
vocala :: Char -> Bool
vocala x = case x of
    'a' -> True
    'e' -> True
    'i' -> True
    'o' -> True
    'u' -> True
    otherwise -> False

vocale :: String -> Int 
vocale "" = 0 
vocale (x:xs) 
  | vocala x = 1 + vocale xs 
  | otherwise = vocale xs

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale(a:b) = 
    if (a == reverse a)
        then vocale a + nrVocale b
    else    
       nrVocale b


-- nrVocale ["sos", "civic", "palton", "desen", "aerisirea"] = 9

--2)

f :: Int -> [Int] -> [Int]
f e [] = []
f e (a:b) =
    if(a `mod` 2 == 0)
        then [a]++[e]++ f e b -- a : e : f e b
    else
        [a]++f e b

-- descrieri de liste
semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]

--3)
divizori :: Int -> [Int]
divizori d = [ n | n <- [1..d], d `mod` n == 0]

-- divizori 4 = [1,2,4]

--4)
listadiv :: [Int] -> [[Int]]
listadiv [] = []
listadiv(a:b) = 
    divizori a : listadiv b

-- listadiv [1,4,6,8] = [[1],[1,2,4],[1,2,3,6],[1,2,4,8]]

--5)

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec a b [] = []
inIntervalRec a b (x:xs) = 
    if (x >= a && x <= b)
        then x : inIntervalRec a b xs
    else
        inIntervalRec a b xs



inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b l = [n | n <- l, n >= a && n<= b]

--6)
-- pozitive [0,1,-3,-2,8,-1,6] == 3

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x:xs) = 
    if (x > 0)
        then 1 + pozitiveRec(xs)
    else
        pozitiveRec(xs)

pozitiveComp :: [Int] -> Int
pozitiveComp l = length [ n | n <- l, n > 0]

--7)
-- pozitiiImpare [0,1,-3,-2,8,-1,6,1] == [1,2,5,7]

pozitiiImpareRec :: [Int]  -> [Int]
pozitiiImpareRec l = pozitiiImpareRec1 l 0

pozitiiImpareRec1 :: [Int] -> Int -> [Int]
pozitiiImpareRec1 [] p =[]
pozitiiImpareRec1 (x:xs) p = 
    if (x `mod` 2 == 1)
        then p : pozitiiImpareRec1 xs (p+1)
    else
        pozitiiImpareRec1 xs (p+1)

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [i | (i, x) <- [0..] `zip` l, odd x]

--8)
-- multDigits "The time is 4:25" == 40
-- multDigits "No digits here!" == 1

--a
import Data.List
import Data.Char

multDigitsRec :: String -> Int 
multDigitsRec "" = 1
multDigitsRec (x : xs)
  | isDigit x = digitToInt x * multDigitsRec xs
  | otherwise = multDigitsRec xs


--8b

multDigitsComp :: String -> Int 
multDigitsComp list = foldl (*) 1 [digitToInt nr | nr <- list, isDigit nr]


