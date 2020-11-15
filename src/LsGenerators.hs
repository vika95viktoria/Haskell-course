module LsGenerators where

import Data.List  

data Odd = Odd Integer 
  deriving (Eq, Show)

addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"  


instance Enum Odd where
    succ (Odd n) = Odd (n + 2)
    pred (Odd n) = Odd (n - 2)
    toEnum number = Odd (toInteger number)
    fromEnum (Odd n) = fromInteger n
    enumFrom (Odd n) = map Odd [n, (n+2)..]
    enumFromThen (Odd n) (Odd m) = map Odd [n, m..]
    enumFromTo (Odd n) (Odd m) = map Odd [n, (n + 2) .. m]
    enumFromThenTo (Odd n) (Odd m) (Odd p) = map Odd [n, m .. p]



coins :: Num a => [a]
coins = [2, 3, 7]

change :: (Ord a, Num a) => a -> [[a]]
change a = nub $ map (filter (>0)) (filter (all (>=0)) (changeHelp a))

changeHelp :: (Ord a, Num a) => a -> [[a]]
changeHelp a 
         | a < 0 = [[-1]]
         | a == 0 = [[]]
         | otherwise = [[x, y] | x <- 0:coins, y <- coins, x + y == a] ++ concatMap (\x -> map (x:) (change (a - x)) ) coins



change' :: (Ord a, Num a) => a -> [[a]]
change' n | n < 0     = []
          | n == 0    = [[]]
          | otherwise = [ x : xs | x <- coins, xs <- change (n - x) ]



concatList :: [[a]] -> [a]
concatList = foldr (++) []

lengthList :: [a] -> Int
lengthList = foldr (\x s -> 1 + s) 0

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0

meanList :: [Double] -> Double
meanList ls = foldr (\x s -> x / n + s) 0 ls  where n = fromRational (fromIntegral $ length ls)

-- evenOnly :: [a] -> [a]
-- evenOnly =  snd . help
--     where help = foldl (\(pos, ls) x  -> if pos `mod` 2 == 0 then (pos + 1, ls ++ [x]) else (pos + 1, ls)) (1, [])

-- evenOnly :: [a] -> [a]
-- evenOnly  = evenOnly' 1

-- evenOnly' _ []        = [] 
-- evenOnly' pos (x:xs)  = (if pos `mod` 2 == 0 then [x] else []) ++ evenOnly' (pos + 1) xs

-- -- construct (x:xs) = (foldr (:) [] [x]) ++ construct xs

-- construct (x:xs) = (foldr (:) [x] (construct xs)) 

lastElem :: [a] -> a
lastElem = foldl1 (curry snd)

revRange :: (Char,Char) -> [Char]
revRange (a,b) = unfoldr g b
  where g x 
            | x >= a && x <= b = Just (x, pred x)
            | otherwise = Nothing