module Day1 where

readInt :: String -> Int
readInt = read 

computeMass :: Int -> Int
computeMass n = n `quot` 3 - 2

computeFuel :: Int -> Int
computeFuel m = go m 0
    where
        go n acc = 
            let fuel = computeMass n
            in if fuel <= 0 then acc else go fuel (acc + fuel)

-- Have to drop 1 since the module weight is the element in the list created by iterate
computeFuel' n = sum $ drop 1 . takeWhile (>= 0) . iterate computeMass $ n

main :: IO ()
main = do
    contents <- readFile "day-1-input.txt"
    let modules = map readInt . words $ contents
    let totalMass = sum $ map computeMass modules
    putStrLn $ "The total mass is " ++ show totalMass
    let totalFuel = sum $ map computeFuel' modules
    putStrLn $ "The total fuel required is " ++ show totalFuel