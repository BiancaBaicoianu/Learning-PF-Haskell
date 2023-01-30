-- 5.2. Modificati definitia monadei WriterS astfel încât să producă lista mesajelor logate si nu concatenarea lor. 
--Definiti functia logIncrementN în acest context.

--- Monada Writer

newtype WriterLS a = WriterLS { runWriter :: (a, [String]) }
instance Monad WriterLS where
  return x = Writer (x, [])
  m >>= k = let (x, w) = runWriter m
                (y, w') = runWriter (k x)
            in WriterLS (y, w ++ w')

instance  Applicative WriterLS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterLS where              
  fmap f ma = pure f <*> ma     

tell :: [String] -> WriterLS () 
tell log = Writer ((), log)  

logIncrement :: Int -> WriterLS Int
logIncrement x = Writer (x + 1, ["Incremented " ++ show x])

logIncrementN :: Int -> Int -> WriterLS Int
logIncrementN x n =  do
  | n==1 = logIncrement x 
  | otherwise = do
    tell ["Incrementing " ++ show x ++ " " ++ show n ++ " times"]
    logIncrementN (x+1) (n-1)