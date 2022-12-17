{- 
class Functor f where 
     fmap :: (a -> b) -> f a -> f b 
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

Just length <*> Just "world"

Just (++" world") <*> Just "hello,"
pure (+) <*> Just 3 <*> Just 5
pure (+) <*> Just 3 <*> Nothing
(++) <$> ["ha","heh"] <*> ["?","!"]
-}

--1. Se da tipul de date
data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)

--Să se scrie instante Functor si Applicative pentru tipul de date List.

concatList :: List a -> List a -> List a
concatList xs Nil = xs
concatList Nil ys = ys
concatList (Cons x xs) ys = Cons x (concatList xs ys)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> xs = concatList (fmap f xs) (fs <*> xs)

-- pure 4 :: List Int 

g = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (g <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))


--2. Se da tipul de date
data Cow = Cow {
        name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)


--a) Să se scrie functiile noEmpty, respectiv noNegative care valideaza un string, respectiv un intreg.

noEmpty :: String -> Maybe String
noEmpty x = if x == "" then Nothing 
            else Just x

test21 = noEmpty "abc" == Just "abc"

noNegative :: Int -> Maybe Int
noNegative x = if x < 0 then Nothing 
               else Just x

test22 = noNegative (-5) == Nothing 
test23 = noNegative 5 == Just 5 

--b. Sa se scrie o functie care construieste un element de tip Cow verificând numele, varsta si greutatea cu functiile de la a).

-- (<$>) :: (a −> b) −> m a −> m b (este fmap)
-- ex : Prelude> (++ ) <$> ( Right " Hey " ) <*> ( Right "You ! " ) 
--      Right " Hey You ! "

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString s a b 
    | noEmpty s == Nothing = Nothing
    | noNegative a == Nothing = Nothing
    | noNegative b == Nothing = Nothing
    | otherwise = Just (Cow {name = s, age = a, weight = b})

test24 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})

-- c) Se se scrie functia de la b) folosind fmap si <*>.

cowFromString2 :: String -> Int -> Int -> Maybe Cow
cowFromString2 s a b = Cow <$> noEmpty s <*> noNegative a <*> noNegative b
                        
test242 = cowFromString2 "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})   

--3. Se dau următoarele tipuri de date:

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

--a) Să se implementeze o functie validateLength care validează lungimea unui sir (sa fie mai mică decât numărul dat ca parametru).

validateLength :: Int -> String -> Maybe String
validateLength a s = if length s > a then Nothing
                     else Just s

test31 = validateLength 5 "abc" == Just "abc"

--b) Să se implementeze functiile mkName si mkAddress care transformă un sir de
--caractere într-un element din tipul de date asociat, validând stringul cu functia
--validateLength (numele trebuie sa aiba maxim 25 caractere iar adresa maxim 100).

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s
--sau
mkName2 :: String -> Maybe Name
mkName2 s 
    | validateLength 25 s == Nothing = Nothing
    | otherwise = Just (Name s)

mkAddress :: String -> Maybe Address
mkAddress s = Address <$> validateLength 100 s
--sau
mkAddress2 :: String -> Maybe Address
mkAddress2 s 
    | validateLength 100 s == Nothing = Nothing
    | otherwise = Just (Address s)

test32 = mkName "Gigel" == Just (Name "Gigel")
test33 = mkAddress "Str Academiei" == Just (Address "Str Academiei")

--c) Să se implementeze functia mkPerson care primeste ca argument două siruri de caractere si formeaza un element de tip Person daca sunt validate conditiile,
-- folosind functiile implementate mai sus.

mkPerson :: String -> String -> Maybe Person
mkPerson s1 s2 = Person <$> mkName s1 <*> mkAddress s2

mkPerson2 :: String -> String -> Maybe Person
mkPerson2 s1 s2 
    | mkName s1 == Nothing = Nothing
    | mkAddress s2 == Nothing = Nothing
    | otherwise = Just (Person (Name s1) (Address s2))

test34 = mkPerson2 "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))