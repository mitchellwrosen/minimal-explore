module Data.Char (isDigit, strToInt, toDigit) where

strToInt :: String -> Int
strToInt = undefined

isDigit :: Char -> Bool
isDigit = (`elem` ['0'..'9'])

toDigit :: Char -> Int
toDigit '0' = 0
toDigit '1' = 1
toDigit '2' = 2
toDigit '3' = 3
toDigit '4' = 4
toDigit '5' = 5
toDigit '6' = 6
toDigit '7' = 7
toDigit '8' = 8
toDigit '9' = 9
toDigit _ = error "not digit"
