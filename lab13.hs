
{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}

-- 1. 

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

-- 1.1 Întelegeti ce face functia fct.

fct :: Maybe Int ->  Maybe Bool
fct  mx =  mx  >>= (\x -> Just (pos x))

-- returneaza daca nr dat( de tip Maybe Int) este pozitiv(rezultatul este Maybe Bool)

-- 1.2 Definiti functia fct folosind notatia do.
fct2 :: Maybe Int ->  Maybe Bool
fct2  mx =  do 
    x <- mx
    return (pos x)

--2. Vrem să definim o functie care adună două valori de tip Maybe Int

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = mx >>= (\x -> my >>= (\y -> Just (x+y)))

--2.2
addM2 :: Maybe Int -> Maybe Int -> Maybe Int
addM2 mx my = do
    x <- mx
    y <- my
    return (x+y)

--addM (Just 4) (Just 3)
--Just 7
--addM (Just 4) Nothing
--Nothing
--addM Nothing Nothing
--Nothing


-- 3. Să se treacă în notatia do urmatoarele functii:

cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))

cartesian_product2 xs ys = do
    x <- xs
    y <- ys
    return (x,y)

-- cartesian_product [1,2,3] [2,3,4]

prod f xs ys = [f x y | x <- xs, y<-ys]

prod2 f xs ys = do
    x <- xs
    y <- ys
    return (f x y)
--  prod (+) [1,2,3] [2,3,4]

--prod (+) [1,2,3] [2,3,4]

myGetLine :: IO String
myGetLine = getChar >>= \x ->
      if x == '\n' then
          return []
      else
          myGetLine >>= \xs -> return (x:xs)

myGetLine2 :: IO String
myGetLine2 = do
    x <- getChar
    if x == '\n' then
        return []
    else
        do
            xs <- myGetLine2
            return (x:xs)

-- 4. Să se treacă în notatia cu secventiere urmatoarea functie:

prelNo noin =  sqrt noin

ioNumber = do
     noin  <- readLn :: IO Float
     putStrLn $ "Intrare\n" ++ (show noin)
     let  noout = prelNo noin
     putStrLn $ "Iesire"
     print noout

ioNumber2 = (readLn :: IO Float) >>= (\noin -> putStrLn ("Intrare\n" ++ (show noin)) >> let noout = prelNo noin in putStrLn "Iesire" >> print noout)

-- 6 
data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN = name

showPersonA :: Person -> String
showPersonA = show . age

{-
showPersonN $ Person "ada" 20
"NAME: ada"
showPersonA $ Person "ada" 20
"AGE: 20"
-}

showPerson :: Person -> String
showPerson = showPersonN >>= (\n -> showPersonA >>= (\a -> return ("NAME: " ++ n ++ " AGE: " ++ a)))

{-
showPerson $ Person "ada" 20
"(NAME: ada, AGE: 20)"
-}

--6.3
newtype Reader env a = Reader { runReader :: env -> a }


instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env



instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance Functor (Reader env) where              
  fmap f ma = pure f <*> ma    

mshowPersonN ::  Reader Person String
mshowPersonN = ("NAME:" ++ ) <$> (Reader name)

mshowPersonA ::  Reader Person String
mshowPersonA = ("AGE:" ++ ) <$> Reader (show . age)

mshowPerson ::  Reader Person String
mshowPerson = mshowPersonN >>= (\n -> mshowPersonA >>= (\a -> return ("NAME: " ++ n ++ " AGE: " ++ a))) 

{-
runReader mshowPersonN  $ Person "ada" 20
"NAME:ada"
runReader mshowPersonA  $ Person "ada" 20
"AGE:20"
runReader mshowPerson  $ Person "ada" 20
"(NAME:ada,AGE:20)"
-}