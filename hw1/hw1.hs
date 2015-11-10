import Data.List

lastDigit :: (Integral b) => b -> b
lastDigit x = rem x 10

dropLastDigit :: (Integral b) => b -> b
dropLastDigit x = x `div` 10

toRevDigits :: (Integral b) => b -> [b]
toRevDigits x | x <= 0 = []
              | otherwise = (lastDigit x) : (toRevDigits $ dropLastDigit x)

doubleEveryOther :: (Integral b) => [b] -> [b]
doubleEveryOther [] = []
doubleEveryOther (x:xs) | xs == [] = x:(doubleEveryOther [])
                        | otherwise = x:(head xs * 2):(doubleEveryOther $ tail xs)

sumDigits :: (Integral b) => [b] -> b
sumDigits [] = 0
sumDigits (x:xs) = (sum $ toRevDigits x) + (sumDigits xs)

luhn :: (Integral b) => b -> Bool
luhn x = (rem (sumDigits $ doubleEveryOther $ toRevDigits x) 10) == 0