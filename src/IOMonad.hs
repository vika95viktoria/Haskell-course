module IOMonad where

main' :: IO ()
main' = do
          putStrLn "What is your name?"
          putStr "Name: "
          name <- getLine
          if name == "" then main' else  putStrLn $ "Hi, " ++ name ++ "!"

factorial :: Int -> Int -> Int
factorial 0 acc = acc
factorial 1 acc = acc
factorial n acc = factorial (n - 1) (acc * n)

compute :: Double -> Double -> Int -> Double
compute acc x 0 = acc
compute acc x n = compute (acc + (x^n)/ (fromRational $ fromIntegral (factorial n 1))) x (n - 1)

compE x = compute 1 x 9
