import Data.Char

toAlternatingCase :: String -> String
toAlternatingCase [] = []
toAlternatingCase (x:xs)
  | isLower x = toUpper x : toAlternatingCase xs
  | otherwise = toLower x : toAlternatingCase xs
