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

-- Interesting alternative solution using map.
-- Changes all values in the list to 1 and then totals them.
myLength' :: [a] -> Int
myLength' xs = sum (map (\x -> 1) xs)


-- Problem 5
-- Reverse a list.
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' xs = [last xs] ++ myReverse' (init xs)


-- Problem 6
-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = reverse xs == xs


-- Problem 7
-- 



-- Problem 8
-- Eliminate consecutive duplicates of list elements.
compress :: [a] -> [a]
compress (x:xs) = if x /= (head xs) then (compress xs) else (compress (drop 1 xs))
-- [compressed | compressed <- all, x /= (\x -> head x) xs]


