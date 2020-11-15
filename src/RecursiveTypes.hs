data List a = Nil | Cons a (List a) deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a ls)= a : fromList ls

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)



data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat 0 = Zero
toNat n = Suc (toNat (n - 1))

add :: Nat -> Nat -> Nat
add a b = toNat (fromNat a + fromNat b)

mul :: Nat -> Nat -> Nat
mul a b = toNat((fromNat a) * (fromNat b))

fac :: Nat -> Nat
fac = toNat . factorial . fromNat

factorial 0 = 1
factorial n = n * factorial (n - 1)


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show


height :: Tree a -> Int
height (Leaf a) = 0
height (Node a b) = 1 + max (height a) (height b)

size :: Tree a -> Int
size (Leaf a) = 1
size (Node a b) = 1 + size a + size b


avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf a) = (1, a)
    go (Node a b) = (cLeft + cRight, sumLeft + sumRight)
                    where (cLeft, sumLeft)   = go a
                          (cRight, sumRight) = go b
