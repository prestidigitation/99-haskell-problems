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
