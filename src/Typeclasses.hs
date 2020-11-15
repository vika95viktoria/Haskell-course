import Data.Char

class Printable a where
    toString :: a -> [Char]

instance Printable Bool where
    toString x = if x then "true" else "false"

instance Printable () where
    toString x = "unit type" 
    
instance (Printable a, Printable b) => Printable(a, b) where
    toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"  

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool
    
class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool
    
class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a 
                | doesEnrageGork a && doesEnrageMork a = stomp $ stab a
                | doesEnrageGork a = stab a
                | doesEnrageMork a = stomp a
                | otherwise = a        


-- a = 127.22
-- b = 4.12
-- c = 0.1
-- d = 2
-- ip = show a ++ show b ++ show c ++ show d




class (Enum a, Eq a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a
        | maxBound == a = minBound
        | otherwise = succ a

  spred :: a -> a
  spred a
        | a == minBound = maxBound
        | otherwise = pred a


instance SafeEnum Bool        


avg :: Int -> Int -> Int -> Double
avg a b c =  ((fromInteger $ toInteger a) + (fromInteger $ toInteger b) + (fromInteger $ toInteger c)) / 3 