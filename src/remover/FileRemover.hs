module FileRemover where

import System.Directory
import Data.List

main' :: IO ()
main' = do
         putStr "Substring: "
         x <- getLine
         if null x
             then putStrLn "Canceled"
         else do
             files <- getDirectoryContents "."
             let correct = filter (isInfixOf x) files
             mapM_ removeFile correct
             mapM_ putStrLn (map ("Removing file: " ++ ) correct)

