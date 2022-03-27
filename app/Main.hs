module Main where

import Lib (Results, ProblemResult, printStr)
import Text.Printf
import Data.List.Ordered (minus, unionAll)
import Data.List (inits)


-- Problem 1
below1000 = [1..999]
isMultipleOf3or5 :: Int -> Bool
isMultipleOf3or5 x = x `mod` 3 == 0 || x `mod` 5 ==0

multiplesOf3and5 = filter isMultipleOf3or5 below1000
result1 = sum multiplesOf3and5


-- Problem 2
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibsUnder :: Integer -> [Integer]
fibsUnder x = takeWhile (<=x) fibs

result2 = sum $ filter even $ fibsUnder 4000000 -- <3

-- Problem 3
isDivisibleBy :: Integer -> Integer -> Bool 
isDivisibleBy x y = x `mod` y == 0

primeFactors :: Integer -> [Integer]
primeFactors n =
  case factors of
    [] -> [n]
    _  -> factors ++ primeFactors rest
  where
  limit = ceiling $ sqrt $ fromIntegral n
  potentialFactors = [2 .. limit]
  factors = take 1 $ filter (isDivisibleBy n) potentialFactors
  rest = n `div` head factors

result3 = maximum $ primeFactors 600851475143

-- Problem 4

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

isIntPalindrome :: Integer -> Bool
isIntPalindrome x = isPalindrome $ show x

threeDigitNums = reverse [100..999]

multiplyAll :: [Integer] -> [Integer]
multiplyAll [] = []
multiplyAll (x:xs) = map (*x) xs ++ multiplyAll xs

result4 = maximum $ take 100 $ filter isIntPalindrome $ multiplyAll threeDigitNums

-- Problem 5

isDivisibleBy1to20 :: Integer -> Bool
isDivisibleBy1to20 x = all (isDivisibleBy x) [11,12,13,14,15,16,17,18,19,20]

findFirstDivisibleBy1to20 = head . filter isDivisibleBy1to20
result5 = findFirstDivisibleBy1to20 [2520, 5040 ..]

-- Problem 6
squareOfSumOf100Numbers = sum [1..100] ^ 2 
sumOfSquaresOf100Numbers = sum $ map (^2) [1..100]
result6 = squareOfSumOf100Numbers - sumOfSquaresOf100Numbers

-- Problem 7
-- TODO: Understand what the fuck is happening here!!!
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

isMember n [] = False
isMember n (x:xs)
    | n == x = True
    | otherwise = isMember n xs

isPrime x = x `elem` takeWhile (<=x) primes

result7 = primes !! 10000
-- All results
results :: Results
results = 
    [(1, show result1),
     (2, show result2),
     (3, show result3),
     (4, show result4),
     (5, show result5),
     (6, show result6),
     (7, show result7)
    ]

formatResult :: ProblemResult -> String
formatResult (index,result) = printf "Problem %d: " index ++ result

printResults :: Results -> IO ()
printResults [] = return ()
printResults [r] = printStr $ formatResult r
printResults (r:rs) = do
  printStr $ formatResult r
  printResults rs

main :: IO ()
main = printResults results
