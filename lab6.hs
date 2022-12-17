--1. 
data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

--a) Scrieti o functie care indica daca un fruct este o portocala de Sicilia sau nu
-- soiurile sunt: Tarocco, Moro, Sanguinello
-- Portocala "Maro" 12

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _ _) = False
ePortocalaDeSicilia (Portocala soi _) = soi `elem` ["Moro", "Sanguinello", "Tarocco"]

--test_ePortocalaDeSicilia1 =
--    ePortocalaDeSicilia (Portocala "Moro" 12) == True
--test_ePortocalaDeSicilia2 =
--    ePortocalaDeSicilia (Mar "Ionatan" True) == False

--b) Scrieti o functie care calculeaza nr total de felii de portocale de Sicilia dintr-o lista de fructe

nrFeliiSicilia :: [Fruct] -> Int
--luam toate portocalele de Sicilia cu filter, apoi luam feliile(functie lambda; map), apoi facem suma lor(sum)
nrFeliiSicilia list = sum $ map (\(Portocala soi felii) -> felii) (filter ePortocalaDeSicilia list)

-- test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

--c) Scrieti o functie care calculeaza nr de mere care au viermi dintr-o lista de fructe

areViermi :: Fruct -> Int
areViermi (Mar _ True) = 1
areViermi _ = 0

nrMereViermi :: [Fruct] -> Int
nrMereViermi l = sum $ map areViermi l

--test_nrMereViermi = nrMereViermi listaFructe == 2

--2.
type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

--a) Scrieti o functie care intoarce "Meow" pentru pisica si "Woof" pentru caine

vorbeste :: Animal -> String
vorbeste (Pisica _ ) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

--b) Scrieti o functie care intoarce rasa unui caine dat ca parametru sau Nothing daca e pisica

rasa :: Animal -> Maybe String
rasa (Caine _ a) = Just a
rasa (Pisica _) = Nothing 

--3.
data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

--a) Scrieti o functie care verifica daca suma elementelor de pe fiecare linie este egala cu n (folosim foldr)

-- returneaza o lista cu suma liniilor
fctSuma :: [Linie] -> [Int]
fctSuma = map (\(L x) -> sum x)

f :: [Int] -> Int -> Bool
--verificam daca toate sumele obtinute pe linii sunt egale cu n
f l n = foldr ((&&) .(==n)) True l      --de incercat si cu lambda expresie

verifica :: Matrice -> Int -> Bool
verifica (M linie) = f (fctSuma linie)

--test_verif1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 == False
--test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25 == True

--b) Scrieti o fct care are param un elem de tip matrice si un nr intreg n  
--si care verifica daca toate liniile de lungime n din matrice au numar elem strict pozitive

doarPozN :: Matrice -> Int -> Bool
doarPozN (M linii) n = foldr (&&) True [n == length(filter(> 0)(ln))| L ln <- linii , length ln == n]

--testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == True
--testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == False

--c) Definiti predicatul corect care verifica daca toate liniile dintr-o matrice au aceeasi lungime

list1::Linie->[Int]  --lista de int
list1 (L a)= a

corect :: Matrice -> Bool
corect (M [L _]) = True
corect (M (x:xs))
    |length (list1 x) == length (list1(head xs)) = corect (M xs)   -- verific 2 cate 2
    |otherwise = False

--testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) == False
--testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) == True