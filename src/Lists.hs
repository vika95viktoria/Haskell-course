module Lists where

import Data.Char

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a b ls = a : b : ls

nTimes:: a -> Int -> [a]
nTimes a n = helper [] a n
              where helper ls a 0 = ls
                    helper ls a n = helper (a:ls) a (n - 1)


oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs) = if x `mod` 2 /= 0 then x : oddsOnly xs else oddsOnly xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: (Num a) => [a] -> a
product'[] = 1
product' (x:xs) = x * product' xs

minimum' :: (Ord a) => [a] -> a
minimum' [x] = x
minimum' (x : xs) = let a = minimum' xs in if a < x then a else x

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [x] = True
isPalindrome x = x == reversedX where
      reversedX = rev x []
      rev [] a = a
      rev (x:xs) a = rev xs (x:a)

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []   
sum3 a b c = let helper (x:xs) = (x, xs)
                 helper [] = (0, []) 
                 aT = helper a
                 bT = helper b
                 cT = helper c
            in (fst aT + fst bT + fst cT) : sum3 (snd aT) (snd bT) (snd cT)

                    

groupElems :: Eq a => [a] -> [[a]]
groupElems a = grHelper [] a
                where grHelper acc [] = acc
                      grHelper acc (a:as) = let same = span (== a) (a:as)
                                            in grHelper (acc ++ [(fst same)]) (snd same)





elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys  

readDigits :: String -> (String, String)
readDigits x = (takeWhile isDigit x, dropWhile isDigit x)

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x:xs) 
                       | p1 x = x : (filterDisj p1 p2 xs)
                       | p2 x = x : (filterDisj p1 p2 xs)
                       | otherwise = filterDisj p1 p2 xs


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort ls@(x:xs) = let smaller = filter (<x) ls
                      equal = filter(==x) ls
                      bigger = filter (>x) ls
                      in (qsort smaller) ++ equal ++ (qsort bigger)    

-- squares'n'cubes :: Num a => [a] -> [a]
-- squares'n'cubes = concatMap (\x -> [x^2, x^3])

-- perms :: [a] -> [[a]]
-- perms [x] = [[x]]
-- perms [] = [[]]
-- perms xs = perm3 [] xs



perm3 acc [] = []
perm3 [] [x] = [[x]]
perm3 acc [x] = map (x:) (perm3 [] acc)
perm3 acc (x:xs) = (map (x:) (perm3 [] (acc ++ xs))) ++ perm3 (acc ++ [x]) xs


           
-- perms :: [a] -> [[a]]
-- perms [] = [[]]
-- perms [x] = [[x]]
-- perms (x:xs) = concatMap (insertElem x) (perms xs) where
-- 			insertElem x [] = [[x]]
--                   insertElem x yss@(y:ys) = (x:yss) : map (y:) (insertElem x ys)
                  


delAllUpper :: String -> String
delAllUpper  = unwords . (filter (\x -> any (`elem` ['a'..'z']) x)) . words 

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 (x:xs) (y:ys) (z:zs) = (max x $ max y z) : max3 xs ys zs      
max3 _ _ _ = []


fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)


repeat' = iterate repeatHelper
repeatHelper = id