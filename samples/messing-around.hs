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
