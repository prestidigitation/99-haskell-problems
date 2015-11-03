--Write a program that prints out the numbers 1 to 100 (inclusive).
--If the number is divisible by 3, print Crackle instead of the number.
--If it's divisible by 5, print Pop.
--If it's divisible by both 3 and 5, print CracklePop.
--You can use any language.

module Main where

main :: String -> ()
main = printValues (map cracklePop [1..100])
       where
       printValues [] = return ()
       printValues (x:xs) = putStrLn >> printValues xs

cracklePop :: Integer -> String
cracklePop x
    | x `mod` 3 == 0 && x `mod` 5 == 0 = "CracklePop"
    | x `mod` 3 == 0 = "Crackle"
    | x `mod` 5 == 0 = "Pop"
    | otherwise      = show x


