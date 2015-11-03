-- Problem 1
-- Find the last element of a list.

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs


-- Problem 2
-- Find the last but one element of a list.

myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast (x:xs)
    | length xs == 1 = x
    | otherwise = repeat
    where repeat = myButLast xs

myButLast' :: [a] -> a
myButLast' = head . tail . reverse


-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list"
elementAt xs n = head (drop (n - 1) xs)

elementAt' :: [a] -> Int -> a
elementAt' xs n = xs !! (n - 1)


-- Problem 4
-- Find the number of elements of a list.
myLength :: [a] -> Int
myLength []     = 0
myLength (x:xs) = 1 + myLength xs


