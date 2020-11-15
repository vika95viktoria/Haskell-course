module ParametrizedTypes where

import Data.Char(isDigit)
import Data.List.Split

data Coord a = Coord a a deriving Show

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1 ) (Coord  x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1 ) (Coord  x2 y2 )= abs (x2 - x1) + abs(y2 - y1) 


getCenter :: Double -> Coord Int -> Coord Double
getCenter width (Coord x y) = Coord ((fromInteger $ toInteger x) * width + halfW) ((fromInteger $ toInteger y) * width + halfW)
                                      where halfW = (width / 2.0) 

getCell :: Double -> Coord Double -> Coord Int
getCell width (Coord x y) = Coord (if xCoord >= 0 then xCoord else xCoord - 1) (if yCoord >= 0 then yCoord else yCoord - 1)
                                    where xCoord = fromIntegral $ truncate $ x / width
                                          yCoord = fromIntegral $ truncate $ y / width


                                          

findDigit :: [Char] -> Maybe Char
findDigit str = if length digitPart == 0 then Nothing else Just (digitPart !! 0)
                where digitPart = filter isDigit str

findDigitOrX :: [Char] -> Char
findDigitOrX s= case findDigit s of
                  Nothing -> 'X'
                  (Just a) -> a



maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x



data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

parsePerson :: String -> Either Error Person
parsePerson str = if not ((any (=='\n')) str) || (not (strIsCorrect str))
                     then Left ParsingError
                  else let pairs = getPairs str
                           initialPerson = Person "" "" 0
                           age = getAge pairs
                           firstName = getFirstName pairs
                           lastName = getLastName pairs
                           in setAge (setLastName (setFirstName (Right initialPerson) firstName) lastName) age

getPairs str = map (\x -> (x !! 0, x !! 1)) (filter (\x -> length x == 2) (map (\x -> splitOn " = " x) (splitOn "\n" str)))

strIsCorrect str = all (\x -> length x == 2) (map (\x -> splitOn " = " x) (splitOn "\n" str))

getAge arr = if length ageInfo == 0 || snd (head ageInfo) == ""
                then Left IncompleteDataError 
            else if (not (all isDigit (snd  (head  ageInfo)))) 
                  then Left (IncorrectDataError (snd  (head  ageInfo)))
            else Right (read (snd (head  ageInfo)) :: Int)    
            where ageInfo = filter (\(a,b) -> a == "age") arr

getFirstName :: [(String, String)] -> Either Error String
getFirstName = getName "firstName"

getLastName :: [(String, String)] -> Either Error String
getLastName = getName "lastName"
                   
getName fieldName arr  = if length firstNameInfo == 0 
                           then Left IncompleteDataError  
                        else Right (snd (head firstNameInfo))     
                        where firstNameInfo = filter (\(a,b) -> a == fieldName) arr 

setFirstName :: Either Error Person -> Either Error String -> Either Error Person 
setFirstName (Left e) _ = Left e                       
setFirstName _ (Left e) = Left e
setFirstName (Right p) (Right name) = Right (p{firstName = name}) 

setLastName :: Either Error Person -> Either Error String -> Either Error Person 
setLastName (Left e) _ = Left e                       
setLastName _ (Left e) = Left e
setLastName (Right p) (Right name) = Right (p{lastName = name})   

setAge :: Either Error Person -> Either Error Int -> Either Error Person
setAge (Left e) _ = Left e
setAge _ (Left e) = Left e
setAge (Right p) (Right personAge) = Right (p{age = personAge})   