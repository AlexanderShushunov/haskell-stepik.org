module Stepik where

import Data.Char

helloWorld = putStrLn "Hello, world!"

lenVec3 x y z = sqrt (z ^ 2 + (sqrt (x ^ 2 + y ^ 2)) ^ 2) 

sign x = if (x < 0) then (-1) else (if (x > 0) then 1 else 0)

infixl 6 *+*

a *+* b = a ^ 2 + b ^ 2

(/^) a b = logBase b a

a |-| b = if a - b > 0 then a - b else b - a 

twoDigits2Int x y = if isDigit x && isDigit y 
    then (digitToInt x) * 10 +  digitToInt y 
    else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2 

doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact (n - 2)

fibonacciR :: Integer -> Integer
fibonacciR n | n == 0    = 0
             | n == 1    = 1
             | n > 0     = fibonacciR (n - 1) + fibonacciR (n - 2)
             | otherwise = fibonacciR (n + 2) - fibonacciR (n + 1)

fibonacci :: Integer -> Integer
fibonacci n = helper (0, 1) n where
    helper (r2, r1) n | n == 0 = r2
                      | n == 1 = r1
                      | n > 1  = helper (r1, r2 + r1) (n - 1) 
                      | n < 0  = helper (r1 - r2, r2) (n + 1) 

seqA :: Integer -> Integer
seqA n = let
        helper (r3, r2, r1) n | n == 0      = r3
                              | n == 1      = r2
                              | n == 2      = r1
                              | otherwise   = helper (r2, r1, r1 + r2 - 2 * r3) (n - 1)
    in helper (1, 2, 3) n

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x < 0       = sum'n'count (-x)
              | x < 10      = (x, 1)
              | otherwise   = 
                    let 
                        (lastDigit, rest) = split(x)
                        (restSum, restCount) = sum'n'count(rest)
                    in 
                        (restSum + lastDigit, restCount + 1)
            where split x = (x `mod` 10, x `div` 10)


trapezoidArea base1 base2 height = (base1 + base2) * height / 2 

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = integrationByPanels f a b step defaulPanelCount where
    defaulPanelCount = 1000
    step = (b - a) / defaulPanelCount
    integrationByPanels _ _ _ _ 0              = 0 
    integrationByPanels f a b step panelsCount = trapezoidArea (f a) (f (a + step)) step + 
                                                    integrationByPanels f (a + step) b step (panelsCount - 1)
                                              
                                                




