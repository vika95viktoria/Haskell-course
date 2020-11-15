module Reccursion where

import Data.Char
import Data.Function


seqA n 
       | n > 2 = let 
           helper accPPP accPP accP 0 = accPPP
           helper accPPP accPP accP n = helper accPP accP (accP + accPP - 2*accPPP) (n-1)
         in helper 1 2 3 n
       | n < 0 = error "invalid argument"
       | otherwise = n + 1                


sum'n'count x = (sum [digitToInt a | a <- show (abs x)],  length $ show (abs x))

digitCount n = sum [digitToInt x | x <- n]  


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = ((b - a) / 1000) * (((f a + f b) / 2) + sum [f $ calcX i | i <- [1..999]] )
                     where calcX i = a + (b - a) * i / 1000


integration' f a b = h * ((f a + f b) / 2 + sumFi 0 1 999 )
                     where calcX i = a + (b - a) * i / 1000
                           h = (b - a) / 1000
                           sumFi sum i 0 = sum
                           sumFi sum i n = sumFi (sum + f (calcX i)) (i + 1) (n - 1)                    

getSecondFrom a b c = b


-- multSecond = g `on` h

-- g a b = a * b 

-- h = snd


on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

doItYourself = f . g . h

