module Reader where

data Reader r a = Reader { runReader :: (r -> a) }

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ \e -> runReader m (f e)

ask :: Reader r r
ask = Reader id

type User = String
type Password = String
type UsersTable = [(User, Password)]

pwds :: UsersTable

pwds = [("Bill", "123"), ("Mary", "qwerty"), ("John", "4434")]

firstUser :: Reader UsersTable User
firstUser = do
    e <- ask
    return $ fst (head e)


usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = do
    e <- Reader id
    return $ map fst (filter (\x -> snd x == "123456") e)