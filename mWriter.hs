
--- Monada Writer

newtype WriterS a = Writer { runWriter :: (a, String) } 


instance  Monad WriterS where
  return va = Writer (va, "")
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterS () 
tell log = Writer ((), log)

--5.1.1 Definiti functiile logIncrement si logIncrement2 din curs si testati functionarea lor.

logIncrement :: Int -> WriterS Int
logIncrement x = do 
  tell ("incrementing " ++ show x ++ "\n")
  return (x + 1)

--runWriter(logIncrement 13)
--(14,"incrementing 13\n")

logIncrement2 :: Int -> WriterS Int
logIncrement2 x = do 
  tell ("incrementing " ++ show x ++ "\n")
  tell ("incrementing " ++ show x ++ "\n")
  return (x + 1)

--runWriter(logIncrement2 13)
--(14,"incrementing 13\nincrementing 13\n")

--5.1.2 Definit, i funct, ia logIncrementN, care generalizează logIncrement2

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n =  do
  tell ("incrementing " ++ show x ++ " " ++ show n ++ " times\n")
  return (x + n)

--runWriter(logIncrementN 2 6)
--(8,"incrementing 2 6 times\n")               

