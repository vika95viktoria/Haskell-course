module OwnTypes where

import Data.List

data Color = Red | Green | Blue

instance Show Color where
    show Red = "Red"
    show Blue = "Blue"
    show Green = "Green"


stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Blue" = Blue
stringToColor "Green" = Green


data LogLevel = Error | Warning | Info 
cmp :: LogLevel -> LogLevel -> Ordering
cmp a b = compare (logToInt a) (logToInt b)
           where logToInt Error = 3
                 logToInt Warning = 2
                 logToInt Info = 1


data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2)= sqrt ((x2 - x1)^2 + (y2 - y1)^2)

data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = 2 * pi * (r^2)
area (Rectangle a b) = a * b

data SomeData = SomeData Int

data Result = Fail | Success

doSomeWork :: SomeData -> (Result,Int)
doSomeWork (SomeData a) = if a > 10 then (Success, 0) else (Fail, a)


processData :: SomeData -> String
processData sm = case doSomeWork sm of
         (Success, _ )  -> "Success"
         (_      , n)   -> "Fail: " ++ show n


data Result' = Success' Result | Fail' Int

instance Show Result' where
    show (Success' _) = "Success"
    show (Fail' code) = "Fail: " ++ show code
         
doSomeWork' :: SomeData -> Result'
doSomeWork' a = case doSomeWork a of
                (Success, x) -> (Success' Success)
                (Fail, code) -> (Fail' code)



square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle x y) = x == y
isSquare (Circle x) = False 


data Bit = Zero | One deriving (Eq, Show)    
data Sign = Minus | Plus deriving Show
data Z = Z Sign [Bit] deriving Show

add :: Z -> Z -> Z
add (Z sign1 bit1) (Z sign2 bit2) = Z sign1 (addBitArr [] Zero bit1 bit2)

addBitArr result prev (x:xs) (y:ys) = let sumRes = addBit prev x y in (addBitArr (result ++ [fst sumRes]) (snd sumRes) xs ys) 
addBitArr result prev [] [] = if prev == One then result ++ [prev] else result
addBitArr result prev [] (x:xs) = let sumRes = addBit prev x Zero in (addBitArr (result ++ [fst sumRes]) (snd sumRes) [] xs)
addBitArr result prev (x:xs) [] = let sumRes = addBit prev x Zero in (addBitArr (result ++ [fst sumRes]) (snd sumRes) xs [])

addBit prev a b = let (toWrite, toTransfer) = addBitRule a b
                      (toWrite2, toTransfer2) = addBitRule toWrite prev
                      (toTransferFinally, _) = addBitRule toTransfer toTransfer2
                      in (toWrite2, toTransferFinally)

addBitRule Zero Zero = (Zero, Zero)
addBitRule Zero One = (One, Zero)
addBitRule One Zero = (One, Zero)
addBitRule One One = (Zero, One)

-- mul :: Z -> Z -> Z
-- mul = undefined