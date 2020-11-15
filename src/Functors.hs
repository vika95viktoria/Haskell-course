module Functors where

import Data.Functor
import Data.Char


data Point3D a = Point3D a a a deriving Show


instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)



data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a) deriving Show

instance Functor GeomPrimitive where
    fmap f (Point p) = Point (fmap f p)
    fmap f (LineSegment p1 p2) = LineSegment (fmap f p1) (fmap f p2)



data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
    fmap _ (Leaf Nothing) = Leaf Nothing
    fmap f (Leaf (Just x)) = Leaf (Just (f x))
    fmap f (Branch lt Nothing rt) = Branch (fmap f lt) Nothing (fmap f rt)
    fmap f (Branch lt (Just x) rt) = Branch (fmap f lt) (Just (f x)) (fmap f rt)




data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show



instance Functor (Entry k1 k2) where
    fmap f (Entry (x, y) value) = Entry (x,y) (f value)

instance Functor (Map k1 k2) where
    fmap f (Map []) = Map []
    fmap f (Map (xs)) = Map (map (fmap f) xs)



data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = Log [msg] . f  


execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (fmsg ++ smsg) res2
                    where (Log fmsg res) = f x
                          (Log smsg res2) = g res


returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msg r) f = let (Log logs res) = f r in Log (msg ++ logs) res

instance Monad Log where
    return = returnLog
    (>>=) = bindLog


execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList param (x:xs) = helper (x param) xs
                              where helper log [] = log
                                    helper log (x:xs) = helper (log >>= x) xs