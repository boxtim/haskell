module Math
( isPrime
, isFactor
, getSomePrimes
, getTheFirstPrimes
) where

isFactor :: Int -> Int -> Bool
isFactor n k = n `mod` k == 0

isPrime :: Int -> Bool
isPrime n = (length ns) == 2
  where ns = [k | k <- [1..n], isFactor n k]

getSomePrimes :: Int -> [Int]
getSomePrimes n = [k | k <- [1..n], isPrime k]

getTheFirstPrimes :: Int -> [Int]
getTheFirstPrimes n = (getSomePrimes . head) ns
  where ns = [k | k <- [n..], n == (length . getSomePrimes) k]