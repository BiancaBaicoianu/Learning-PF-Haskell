import Data.List

myInt = 5555555555555555555555555555555555555555555555555555555555555555555555555555555555555

double :: Integer -> Integer
double x = x+x

--triple :: Integer -> Integer
--triple x = x+x+x

triple :: Integer -> Integer
triple x = 3*x


maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y)
               then x
          else y


max3 x y z = let
             u = maxim x y
             in (maxim  u z)


functia1 :: Integer -> Integer -> Integer
functia1 a b  = a*a+b*b

functia2 :: Integer -> String
functia2 a = if(a `mod` 2==1)
			then "impar"
		else
			"par"

functia3 :: Integer -> Integer
functia3 n = if(n==0 ||n==1) then 1
		else n*functia3(n-1)

functia4 :: Integer -> Integer -> Bool
functia4 a b = if( a>2*b)
			then True
		else False




