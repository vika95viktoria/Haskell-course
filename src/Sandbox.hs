import Data.List
import Data.Char

countPandig n = sum . nub $ map (\(x, y, z) -> z) correct
                where pandigNum = ['1'..(intToDigit n)]
                      allPerms = permutations pandigNum
                      divSchemas = divRazr n
                      allThrees = concatMap (\p -> map (\sch -> arrToThreeNums sch p) divSchemas) allPerms
                      correct = filter (\(x, y, z) -> x * y == z) allThrees
                      

divRazr :: Int -> [(Int, Int, Int)]
divRazr n = [(x, y, z) | x <- [1..(round $ ((fromIntegral n) / 2))], y <- [1..(round $ ((fromIntegral n) / 2))], 
              z <- [1..(round $ ((fromIntegral n) / 2))], x + y + z == n, z >= x, z >= y, not (x == y && y == z) ]



arrToThreeNums (x, y, z) arr = (read firstDigit::Int, read secondDigit::Int, read thirdDigit::Int)
                               where firstDigit = take x arr
                                     secondDigit = take y (drop x arr)
                                     thirdDigit =  drop (x+y) arr       
                                     
coins :: Num a => [a]
coins = [1, 2, 3, 4, 10, 20, 50, 100, 200]

change :: (Ord a, Num a) => a -> [[a]]
change n | n < 0     = []
          | n == 0    = [[]]
          | otherwise = [ x : xs | x <- coins, xs <- change (n - x) ]   
          
countMod arr = (length arr) `mod` 1000000007      

-- main = do
--    val1 <- readLn
--    let sum = countPandig val1
--    print sum                                    

-- main = do 
--     n <- readLn :: IO Int 
--     inputdata <- getContents 
--     let 
--         numbers = map read (lines inputdata) :: [Int] 
--     putStrLn . unlines $ (map show . (count $ change n)) numbers    
