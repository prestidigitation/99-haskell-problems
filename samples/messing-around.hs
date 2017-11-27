import Data.List
import Data.Maybe

lastList :: [a] -> a
lastList [x] = x
lastList (_:xs) = lastList xs
lastList [] = error "Can't do last of an empty list!"

firstList :: [a] -> a
firstList (x:_) = x
firstList [] = error "Can't do first of an empty list!"

nextLast :: [a] -> a
nextLast [] = error "Can't do next to last of an empty list!"
nextLast [x,_] = x
nextLast (x:xs) = nextLast xs

copyFirstElement :: [a] -> [a]
copyFirstElement [] = error "Can't run copyFirstElement on an empty list!"
copyFirstElement (x:xs) = x:x:xs

-- reversextimes :: Int n -> ([a] -> [a]) -> a -> a
-- reversextimes 0 x = x
-- reversextimes n x = reversextimes (n - 1) $ reverse x

getIndexOfMinimum :: Ord a => [a] -> Int
getIndexOfMinimum [] = error "Can't get index of smallest from empty list!"
getIndexOfMinimum xs = fromJust $ (elemIndex . minimum) xs xs

removeSmallest :: [Int] -> [Int]
removeSmallest [] = []
removeSmallest [x] = []
removeSmallest xs = fst (splitAt i xs) ++ tail (snd $ splitAt i xs)
  where
    i = fromJust $ (elemIndex . minimum) xs xs

smash :: [String] -> String
smash [] = ""
smash [x] = x
smash (x:xs) = x ++ " " ++ smash xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _     = error "Empty list"
elementAt (x:xs) n = if n == 1 then x else elementAt xs (n - 1)

-- Problem 4
myLength :: [a] -> Int
myLength []     = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = if xs == reverse xs then True else False

-- Problem 7
-- data NestedList a = Elem a | List [NestedList a]
-- myFlatten :: [Elem | List] -> [a]
-- myFlatten []     = []
-- myFlatten (x:xs) = myFlatten x + myFlatten xs

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress []     = []
compress (x:xs) = if [x] == take 1 xs then compress xs else x : compress xs
