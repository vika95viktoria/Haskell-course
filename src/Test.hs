module Test where

import Data.Char

sayHello = putStrLn "Hello Vika from module Test!"

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2  
                      
doubleSmallNumber' x = (if x > 105 then x else x*2) + 1        

greeting s = "It's a-me, "   ++ s ++ "!"

lenVec3 x y z = sqrt (x^2 + y^2 + z^2)

sign x = if x == 0 then 0 else if x > 0 then 1 else -1


twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then digitToInt x * 10 + digitToInt y else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt ((fst p2 - fst p1) ^ 2 + (snd p2 - snd p1) ^ 2)

fibonacci n 
            | n == 0 = 0
            | n == 1 = 1
            | n > 0 = fibonacci(n - 1) + fibonacci(n - 2)
            | n < 0 = (fibonacci(n + 2)) - (fibonacci(n + 1))


helperPositive accPP accP 0 = accP
helperPositive accPP accP n = helperPositive accP (accPP + accP) (n - 1)

helperNegative accPP accP 0 = accP
helperNegative accPP accP n = helperNegative accP (accPP - accP) (n + 1)

helper accPP accP 0 = accP
helper accPP accP n 
                    | n < 0 = helper accP (accPP - accP) (n + 1)
                    | n > 0 = helper accP (accPP + accP) (n - 1)

fibonacciLinear n
                  | n < 0  = helper 1 0 n
                  | n == 0 = 0
                  | otherwise = helper 0 1 (n - 1)


