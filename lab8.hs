-- 1. Se dau următoarele tipuri de date ce reprezinta puncte cu numar variabil de coordonate intregi:
data Punct = Pt [Int]

-- Arbori cu informatia în frunze şi clasă de tipuri ToFromArb

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
  toArb :: a -> Arb
  fromArb :: Arb -> a

-- a) Să se scrie o instantă a clasei Show pentru tipul de date Punct, astfel încât lista
--coordonatelor sa fie afisată sub forma de tuplu.

instance Show Punct where
  show (Pt []) = "()"
  show (Pt (x:xs)) = " (" ++ showSep x xs ++ ") " 
    where
      showSep x [] = show x
      showSep x (y:ys) = show x ++ ", " ++ showSep y ys

-- Pt [1,2,3]
-- (1, 2, 3)

-- Pt []
-- ()

--b) Să se scrie o instanţă a clasei ToFromArb pentru tipul de date Punct astfel incat
--lista coordonatelor punctului sa coincidă cu frontiera arborelui.

--class ToFromArb a where
--  toArb :: a -> Arb
--  fromArb :: Arb -> a

-- toArb (Pt [1,2,3])
-- N (F 1) (N (F 2) (N (F 3) Vid))
-- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
--  (1,2,3)

-- Arbori cu informatia în frunze şi clasă de tipuri ToFromArb

instance ToFromArb Punct where
  toArb (Pt []) = Vid
  toArb (Pt (x:xs)) = N (F x) (toArb(Pt xs))
  fromArb Vid = Pt []
  fromArb a = Pt (fromArb2 a)
    where
      fromArb2 Vid = []
      fromArb2 (N (F x) p) = x : (fromArb2 p)

-- 2. Se dă următorul tip de date reprezentând figuri geometrice.

data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

-- Si clasa GeoOps în care se definesc operatiile perimeter si area.

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

-- a) Să se instantieze clasa GeoOps pentru tipul de date Geo. Pentru valoarea pi există functia cu acelasi nume (pi).

instance GeoOps Geo where
  perimeter (Square a) = 4*a
  perimeter (Rectangle a b) = 2*(a+b)
  perimeter (Circle a) = 2*pi*a
  area (Square a) = a*a
  area (Rectangle a b) = a*b
  area (Circle a) = pi*a*a

-- b)Să se instantieze clasa Eq pentru tipul de date Geo, astfel încât două figuri geometrice să fie egale dacă au perimetrul egal.

instance (Eq a, Floating a) => Eq (Geo a) where
  (==) x y = perimeter(x) == perimeter(y)
 