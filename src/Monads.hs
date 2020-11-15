module Monads where

import Data.Char

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken x = if all isDigit x then Just (Number (strToInt x))
                         else Nothing

strToInt s = helper 0 (map digitToInt s) where
             helper acc [] = acc
             helper acc (x:xs) = helper (acc*10 + x) xs                         

tokenize :: String -> Maybe [Token]
tokenize input = sequence $ map asToken $ words input


pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do 
                         c <- [1..x]
                         b <- [1..x]
                         a <- [1..x]
                         if a^2 + b^2 == c^2 && a < b then "1" else []
                         return (a, b, c)
