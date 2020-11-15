infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr

expand e = if expandHelper e == e then e else expand $ (expandHelper e)

expandHelper ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
expandHelper (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
expandHelper (e1 :+: e2) = expand e1 :+: expand e2
expandHelper (e1 :*: e2) = expand e1 :*: expand e2
expandHelper e = e

type Endo a = a -> a 

func :: Endo (Endo Int) -> Int
func x = x (1+) 1
test = func (\x y -> x y)


newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

class Monoid a where
    mempty :: a
    mappend :: a -> a -> a    

instance Monoid Xor where
    mempty = Xor False
    mappend (Xor a) (Xor b) = if a == b then Xor False else Xor True




newtype Maybe' a = Maybe' { getMaybe :: Maybe a } deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)
    mappend mempty (Maybe' Nothing) = (Maybe' Nothing)
    mappend (Maybe' Nothing) mempty = (Maybe' Nothing)
    mappend (Maybe' (Just a)) (Maybe' (Just b)) = Maybe' (Just (a `mappend` b) )


