
--sp2 :: [Int] -> [Int]
--sp2[]=[]
--sp2 (x:xs)
--    x `mod` 2 == 0 = (x `div`2):(sp2 xs)
--    otherwise =sp2 xs


--1.Să se scrie o functie poly2 care are patru argumente de tip Double, a,b,c,x 
--si calculează a*xˆ2+b*x+c.
--Scrieti si signatura functiei (poly :: ceva).

poly2 :: Double -> Double -> Double -> Double -> Double
poly2 a b c x = a*x*x+b*x+c

--2. Să se scrie o functie eeny care întoarce “eeny” pentru input par si “meeny” pentru input impar. Hint:
--puteti folosi functia even (putet, i căuta pe https://hoogle.haskell.org/).

eeny :: Integer -> String
eeny a = if even a
            then "eeny"
        else
            "meeny"

--eeny :: Integer -> String
--eeny a = if (a `mod` 2==0)
--            then "eeny"
--        else
--            "meeny"


--3.Să se scrie o funct, ie fizzbuzz care întoarce “Fizz” pentru numerele divizibile cu 3, “Buzz” pentru
--numerele divizibile cu 5 s, i “FizzBuzz” pentru numerele divizibile cu ambele. Pentru orice alt număr se
--întoarce s, irul vid. Pentru a calcula modulo a două numere putet, i folosi funct, ia mod. Să se scrie această
--funct, ie în 2 moduri: folosind if s, i folosind gărzi (condit, ii).

fizzbuzz :: Integer -> String
fizzbuzz a = if(a `mod` 3 == 0)
                then "Fizz"
           else
                if(a `mod` 5 == 0)
                    then "Buzz"
                else
                    if(a `mod` 5 ==0 && a `mod` 3 == 0)
                        then "FizzBuzz"
                    else
                        ""

--FIBONACCI
fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)
fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)

--4. Numerele tribonacci
tribonacci :: Integer -> Integer
tribonacci  n =
    if (n <= 2)
        then  1
    else
        if(n == 3) 
            then 2
        else 
            tribonacci (n - 1) + tribonacci (n - 2) + tribonacci (n - 3)


tribonacciEcuational :: Integer -> Integer
tribonacciEcuational 0 = 0
tribonacciEcuational 1 = 1
tribonacciEcuational 2 = 1
tribonacciEcuational 3 = 2
tribonacciEcuational n =
    tribonacciEcuational (n - 1) + tribonacciEcuational (n - 2) + tribonacciEcuational (n - 3)


--5.Să se scrie o functie care calculează coeficientii binomiali, folosind recursivitate. 
--Acestia sunt determinati folosind urmatoarele ecuatii.
--B(n,k) = B(n-1,k) + B(n-1,k-1)
--B(n,0) = 1
--B(0,k) = 0

binomial :: Integer -> Integer -> Integer
binomial 0 k = 0
binomial n 0 = 1
binomial n k = binomial (n-1)k + binomial (n-1)(k-1)

--6. Să se implementeze următoarele functii folosind liste:
--a) verifL - verifică dacă lungimea unei liste date ca parametru este pară

verifL :: [Int] -> Bool
verifL x = 
    if(length x `mod` 2 == 0)
        then True
    else
        False


--b) takefinal - pentru o listă dată ca parametru si un număr n,
--întoarce lista cu ultimele n elemente.
--Dacă lista are mai putin de n elemente, se intoarce lista nemodificată.

takefinal :: [Int] -> Int -> [Int]
takefinal x n= 
    if(length x < n)
        then x
    else 
        drop (length x -n) x


--Cum trebuie să modificăm prototipul functiei pentru a putea fi folosită 
--si pentru siruri de caractere?

takefinal2 :: [String] -> Int -> [String]
takefinal2 x n= 
    if(length x < n)
        then x
    else 
        drop (length x -n) x

--c) remove - pentru o listă si un număr n se întoarce lista din care 
--se sterge elementul de pe pozitia n.
--(Hint: puteti folosi functiile take si drop). Scriti si prototipul functiei.

remove :: [Int] -> Int -> [Int]
remove x n=
    concat[take (n-1)x, drop n x]

--RECURSIVITATE PE LISTE
--7. Exercitii: să se scrie urmatoarele functii folosind recursivitate:
--a) myreplicate - pentru un întreg n si o valoare v întoarce lista de lungime n 
--ce are doar elemente egale cu v. Să se scrie si prototipul functiei.

myreplicate :: Int -> Int -> [Int]
myreplicate 0 v = []
myreplicate n v = v : myreplicate (n-1)v


--b) sumImp - pentru o listă de numere întregi, calculează suma valorilor impare. 
--Să se scrie s, i prototipul functiei.

sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (a:b)
  | mod a 2 == 1 = a + sumImp b
  |otherwise = sumImp b
     

--c) totalLen - pentru o listă de siruri de caractere, calculează suma lungimilor 
--sirurilor care încep cu caracterul ‘A’.

totalLen :: [String] -> Int
totalLen [] = 0
totalLen (a:b)
    | head a =='A' = length a + totalLen b
    | otherwise = totalLen b



