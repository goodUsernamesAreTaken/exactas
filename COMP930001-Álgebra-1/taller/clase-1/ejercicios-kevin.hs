```haskell

f x y = x*x+y*y      -- ewfhkllfjeql.

g x y z= x+y+z*z   --sdgñlsdgdsdsgvwg

doble x = x*2

suma x y = x+y

normaVectorial x y = sqrt(x^2+y^2)

constante8 x = 8

sumatoria x= div((x)*(x+1)) 2

piecewise x | x==0 = 0
            | x>0 = 1
            |otherwise = -1
            
k 0 = 1
k n = 0


collatz :: Int -> Int
collatz 1 = 1
collatz x | mod x 2 == 0 = (div x 2)
          | otherwise = (3*x + 1)

collatzSequence :: Int -> [Int]
collatzSequence = terminate . iterate collatz
          where
            terminate (1:_) = [1]
            terminate (x:xs) = x: terminate xs
cantidad b c | d>0 =2
             | d == 0 = 1
             | otherwise = 0
             where d = b^2 - 4*c
             
maximo :: Int -> Int -> Int
maximo x y | x >= y = x
           | otherwise = y
           
absoluto :: Int -> Int
absoluto 0 = 0
absoluto x | x > 0 = x
           | otherwise = -x
           
maximoabsoluto :: Int -> Int -> Int
maximoabsoluto x y = maximo (absoluto x) (absoluto y)

maximo3:: Int -> Int -> Int-> Int
maximo3 x y z = maximo (maximo x y) z

algunoes0 :: Int -> Int -> Bool
algunoes0 x y | (x == 0 || y == 0) = True
              | otherwise = False

algunoes0v2 :: Int -> Int -> Bool
algunoes0v2 0 _ = True
algunoes0v2 _ 0 = True
algunoes0v2 _ _ = False

ambosson0 :: Int -> Int -> Bool
ambosson0 x y | (x==0 && y==0) =True
              | otherwise = False

ambosson0v2 :: Int -> Int -> Bool
ambosson0v2 0 0 = True
ambosson0v2 _ _ = False

esMultiplode :: Int -> Int -> Bool
esMultiplode x y | (mod x y) == 0 = True
                 | otherwise = False

digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int
digitoDecenas x = div (mod x 100) 10

```
