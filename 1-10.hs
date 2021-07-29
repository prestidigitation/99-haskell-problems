-- Find the last element of a list
myLast :: [a] -> a
myLast [] = error "List is empty!"
myLast [x] = x
myLast (x:xs) = myLast xs
