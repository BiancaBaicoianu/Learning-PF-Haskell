--foldr op unit [a1, a2, a3, ... , an] = a1 `op` (a2 `op` (a3 `op` .. `op` (an `op` unit)))

--foldr :: (a -> b -> b) -> b -> [a] -> b
--foldr op i [] = i

--foldr op i (x:xs) = x `op` (foldr op i xs)
--foldl op unit [a1, a2, a3, ... , an] = ((((unit `op` a1) `op` a2) `op` a3) `op` ..) `op` an

--foldl :: (b -> a -> b) -> b -> [a] -> b
--foldl op i [] = i
--foldl op i (x:xs) = foldl op (i `op` x) xs

--ghci> foldr (+) 0 [1..5]
--15

--ghci> foldr (*) 1 [2,3,4]
--24

--ghci> foldr (++) [] ["abc","def","ghi"]
--"abcdefghi"

--ghci> foldl (++) "first" ["abc","def","ghi"]
--"firstabcdefghi"

--ghci> foldr (++) "last" ["abc","def","ghi"]
--"abcdefghilast"


--1. Calculati suma pătratelor elementelor impare dintr-o listă dată ca parametru.

sumPatrateImpare :: [Int] -> Int
sumPatrateImpare ls = foldr (+) 0 (map (^2) (filter odd ls))


--DE INCERCAT SI FARA MAP!!!!!!!!!!!
--sumPatrateImpare2 :: [Int] -> Int
--sumPatrateImpare2 ls = foldr (\x sum -> if (odd x) then (x^2 + sum) else sum) 0 

--2. Scrieti o functie care verifică faptul că toate elementele dintr-o listă sunt True, folosind foldr.

allTrue :: [Bool] -> Bool
allTrue = foldr (&&) True

--3. Scrieti o functie care verifică dacă toate elementele dintr-o listă de numere întregi satisfac o proprietate dată ca parametru.
--DE INCERCAT SI FARA MAP
allVerifies :: (Int -> Bool) -> [Int] -> Bool 
allVerifies f l = foldr (&&) True (map f l)

--DE INCERCAT SI FARA MAP
--4. Scrieti o functie care verifică dacă există elemente într-o listă de numere întregi care satisfac o proprietate dată ca parametru.
anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies f l = foldr (||) True (map f l)

--5. Redefiniti functiile map si filter folosind foldr. Le puteti numi mapFoldr si filterFoldr.
mapFoldr :: (Int -> Int) -> [Int] -> [Int]
mapFoldr f = foldr (\x xs -> (f x):xs) [] 

filterFoldr :: (Int -> Bool) -> [Int] -> [Int]
filterFoldr f = foldr (\x xs -> if f x then x : xs else xs) []

--6. Folosind functia foldl, definiti functia listToInt care transformă o lista de cifre (un număr foarte mare stocat sub formă de listă)
-- în numărul intreg asociat. Se presupune ca lista de intrare este dată corect.

-- listToInt [2,3,4,5] = 2345

listToInt :: [Int] -> Int
listToInt = foldl (\x xs -> x*10+xs) 0 

--7
--(a) Scrieti o functie care elimină un caracter din sir de caractere.

rmChar :: Char -> String -> String
rmChar c = foldr (\x xs -> if c==x then xs else x:xs) []

--(b) Scrieti o functie recursivă care elimină toate caracterele din al doilea argument care se găsesc în primul argument, folosind rmChar.

rmCharRec :: String -> String -> String
rmCharRec [] s = s
rmCharRec (x:xs) s = rmCharRec xs (rmChar x s) 


--(c) Scrieti o functie echivalentă cu cea de la (b) care foloseste foldr în locul recursiei si rmChar.

rmCharFoldr :: String -> String -> String
rmCharFoldr s1 s2 = foldr (rmChar) s2 s1




