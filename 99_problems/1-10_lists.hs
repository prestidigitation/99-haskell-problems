-- Problem 1
-- Find the last element of a list.
myLast :: [a] -> a
myLast []     = error "empty list"
myLast [x]    = x
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
-- Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).



-- Problem 8
-- Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress []     = []
compress (x:xs) = if [x] == take 1 xs 
                  then compress (x : drop 1 xs) 
                  else x : compress xs

compress' :: Eq a => [a] -> [a]
compress' []     = []
compress' (x:xs) = x : (compress $ dropWhile (== x) xs)


-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

pack :: Eq a => [a] -> [a]
pack []     = error "empty list"
pack (x:xs) = if [x] == take 1 xs
              then x ++ pack xs
              else x : pack xs


