data Bit = Zero | One deriving (Eq, Show)    
data Sign = Minus | Plus deriving (Show, Eq)
data Z = Z Sign [Bit] deriving Show

add :: Z -> Z -> Z
add a b = if all (==Zero) bit then Z Plus [] else Z sign (removeLeadingZeroes (reverse bit))
    where (Z sign bit) = addHelper a b 

removeLeadingZeroes arr@(x:xs) =  if x == Zero then removeLeadingZeroes xs else reverse arr 

addHelper (Z sign1 bit1) (Z sign2 bit2) = if sign1 == sign2 
                                       then Z sign1 (addBitArr [] Zero bit1 bit2) 
                                    else if compareNumbersInBinary bit1 bit2 == GT 
                                            then Z sign1 (subBitArr [] Zero bit1 bit2)
                                         else Z sign2 (subBitArr [] Zero bit2 bit1)      

compareNumbersInBinary :: [Bit] -> [Bit] -> Ordering
compareNumbersInBinary a b = if a == b 
                                then EQ 
                            else if (length a) > (length b)
                                    then GT
                                 else if (length b) > (length a)
                                          then LT
                                      else compareBitArr (reverse a) (reverse b)
                                               
compareBitArr (x:xs) (y:ys) = if compareHead == EQ then compareBitArr xs ys else compareHead
                                  where compareHead = compareBit x y

compareBit Zero One = LT
compareBit One Zero = GT
compareBit _ _      = EQ

subBitArr result toSubstract (x:xs) (y:ys)  = let (res, newToSub) = subBit toSubstract x y in subBitArr (result ++ [res]) newToSub xs ys
subBitArr result toSubstract (x:xs) []  = let (res, newToSub) = subBit toSubstract x Zero in subBitArr (result ++ [res]) newToSub xs []
subBitArr result toSubstract [] [] = result

subBit toSubstract a b 
                       | a == Zero && toSubstract == One = (fst (subBitRule One b), toSubstract)
                       | a == One && toSubstract == One = subBitRule Zero b
                       | otherwise = subBitRule a b

subBitRule Zero Zero = (Zero, Zero)
subBitRule One One   = (Zero, Zero)
subBitRule One Zero  = (One, Zero)
subBitRule Zero One = (One, One)


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

mul :: Z -> Z -> Z
mul a b = if result == [] then Z Plus [] else Z sign result
    where (Z sign result) = mulHelper a b

mulHelper (Z sign1 bit1) (Z sign2 bit2) = Z (resSign) (multBits bit1 bit2 )
                                       where resSign = if sign1 == sign2 then Plus else Minus   

mulBitRule One One = One
mulBitRule _ _     = Zero

multBitArray x ys = map (\y -> mulBitRule x y) ys

multBits a b = foldl (addBitArr [] Zero) [] (addZeroesArr 0 (multArr a b))

multArr [] ys = []
multArr (x:xs) ys = (multBitArray x ys) : multArr xs ys

addZeroesArr ind []     = []
addZeroesArr ind (x:xs) = (addZeroes ind x) : addZeroesArr (ind + 1) xs
                           where addZeroes ind arr = (replicate ind Zero) ++ arr



