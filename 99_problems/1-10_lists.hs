myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast (x:xs)
    | length xs == 1 = x
    | otherwise = repeat
    where repeat = myButLast xs

myButLast' :: [a] -> a
myButLast' = head . tail . reverse


