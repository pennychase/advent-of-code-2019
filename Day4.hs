module Day4 where

import Data.List ( groupBy )

toDigits :: Int -> [Int]
toDigits n =  go n []
    where
        go 0 ds = ds
        go n ds =
            let (q, r) = quotRem n 10
            in go q (r:ds)

adjacentDigits :: [Int] -> Bool
adjacentDigits ds =
    any (\(i,j) -> i == j) $ zip ds (tail ds) 

adjacentPairDigits :: [Int] -> Bool
adjacentPairDigits ds =
    any (\x -> length x == 2) . filter (\x -> length x <= 2) $ groupBy (==) ds

nonDecreasing :: [Int] -> Bool
nonDecreasing [] = True
nonDecreasing [_] = True
nonDecreasing (x:y:ys) = x <= y && nonDecreasing (y:ys)

meetCriteria :: ([Int] -> Bool) -> Int -> Int -> [Int]
meetCriteria adjTest lower upper  =
    filter meetAll [lower .. upper]
    where
        meetAll n = adjTest ds && nonDecreasing ds
            where ds = toDigits n

day4 :: Int
day4 = length $ meetCriteria adjacentDigits 254032 789860 

day4_part2 :: Int
day4_part2 = length $ meetCriteria adjacentPairDigits 254032 789860 