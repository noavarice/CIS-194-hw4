module Lib
    ( fun1
    , fun2
    ) where

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 list = foldr (*) 1 $ map (subtract 2) $ filter even list

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 num | odd num = fun2 (num * 3 + 1)
         | otherwise = sum evens + fun2 firstOdd
  where
    evens = takeWhile even $ iterate (\i -> div i 2) num
    firstOdd = last evens `div` 2
