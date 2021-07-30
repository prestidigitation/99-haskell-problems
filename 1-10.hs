-- 1. Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "No last element for empty list!"
myLast [x] = x
myLast (_:xs) = myLast xs

-- 2. Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "No second to last element for empty list!"
myButLast [x] = error "No second to last element for singleton list!"
myButLast (x:xs)
  | length xs == 1 = x
  | otherwise = myButLast xs

-- 3. Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Integer -> a
elementAt [] k = error "No elements in an empty list!"
elementAt (x:xs) k
  | k == 1 = x
  | otherwise = elementAt xs (k - 1)
