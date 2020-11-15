module Writer where

import Data.Monoid
import Control.Monad.Writer

-- type Shopping = Writer (Sum Integer) ()

shopping1 :: Shopping
shopping1 = do
    purchase "Jeans"   19200
    purchase "Water"     180
    purchase "Lettuce"   328
    

-- purchase :: String -> Integer -> Shopping
-- purchase item cost = writer ((), Sum cost)
    
-- total :: Shopping -> Integer
-- total w = getSum $ snd $ runWriter w


type Shopping = Writer ([String], Sum Integer) ()

purchase :: String -> Integer -> Shopping
purchase item cost = writer((), ([item], Sum cost))

total :: Shopping -> Integer
total w = getSum $ snd $ snd $ runWriter w

items :: Shopping -> [String]
items w = fst $ snd $ runWriter w