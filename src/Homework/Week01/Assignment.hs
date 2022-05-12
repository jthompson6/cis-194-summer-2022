module Homework.Week01.Assignment where

-- #1a
biggestPow10Divisor :: Integer -> Integer
biggestPow10Divisor n
  | n <= 0    = 0
  | n < 10    = 1
  | otherwise = 10 * biggestPow10Divisor (n `div` 10)

toDigitsHelper :: Integer -> Integer -> [Integer]
toDigitsHelper n d
  | d < 10 = [n]
  | otherwise      = (n `div` d) : toDigitsHelper (n `mod` d) (d `div` 10)

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise  = toDigitsHelper n (biggestPow10Divisor n)

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise  = (n `mod` 10) : toDigitsRev (n `div` 10)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:zs)) =  if (length zs) `mod` 2 == 0 then (2*x) : (y : doubleEveryOther(zs)) else (x) : (2*y : doubleEveryOther(zs)) 


-- #3
sumDigits :: [Integer] -> Integer
sumDigits lst = sum(map (sum) (map (toDigitsRev) lst))

-- #4
validate :: Integer -> Bool
validate n = sumDigits(doubleEveryOther(toDigits(n))) `mod` 10 == 0

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
