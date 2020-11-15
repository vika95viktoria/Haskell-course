module State where

import Control.Monad

newtype State s a = State {runState:: s -> (a, s)}

instance Monad (State s) where
    return a = State $ \st -> (a, st)
    m >>= k = State $ \st ->
        let (a, st') = runState m st
            m' = k a
            in runState m' st'

get = State $ \st -> (st, st)            

put st = State $ \_ -> ((), st)        

execState m s = snd(runState m s)

fibStep :: State (Integer, Integer) ()
fibStep = State $ \(a, b) -> ((), (b, a + b))

execStateN :: Int -> State s a -> s -> s
execStateN n m s = execState (replicateM n m) s

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show

-- numberTree :: Tree () -> Tree Integer
-- numberTree (Leaf ()) = Leaf (1)
-- numberTree tree = Leaf (2)

tick = do
    n <- get
    put (n+1)
    return n

--  State Integer Tree  

-- put :: s -> State s ()
-- put x s = ((),x)


-- numTree :: Tree () -> State (Tree Integer) Integer 
-- numTree (Leaf ()) = do
--     currentTree <- get
    
-- func :: Integer -> State (Tree Integer) Integer
-- func current = State $ \st -> case st of 
--                                   (Leaf x ) -> (current + 1, Leaf (current))
--                                   (Fork (x) (z) (y)) -> (current + 3, (Fork (Leaf (current)) (current + 1) (Leaf (current + 2))))


-- numTreeSimple :: Tree Integer -> Integer -> (Tree Integer, Integer)
-- numTreeSimple (Leaf x) current = (Leaf (current), current + 1)
-- numTreeSimple (Fork (x) (z) (y)) current = let (leftTree, counter) = numTreeSimple x current
--                                                middle = counter
--                                                (rightTree, final) = numTreeSimple y (counter + 1)
--                                                in (Fork leftTree middle rightTree, final)                               

-- numZeros :: Tree () -> Tree Integer
-- numZeros (Leaf () ) = Leaf (0)
-- numZeros ((Fork (x) () (y))) = Fork (numZeros x) 0 (numZeros y)


-- numberTree :: Tree () -> Tree Integer
-- numberTree tree = fst $ numTreeSimple (numZeros tree) 

evalState m s = fst (runState m s)


numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (numberTree' tree) 1

numberTree' (Leaf _) = tick >>= return . Leaf
numberTree' (Fork l _ r) = do 
    l' <- numberTree' l
    t  <- tick
    r' <- numberTree' r
    return $ Fork l' t r'
