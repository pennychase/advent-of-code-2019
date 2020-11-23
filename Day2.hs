module Day2 where

import qualified Data.Map as M

readInt :: String -> Int
readInt = read 

splitComma :: String -> [String]
splitComma xs = words $ map (\x -> if x == ',' then ' ' else x) xs

data IntcodeComputer = IntcodeComputer
    { instrPtr :: Int
    , memory :: M.Map Int Int
    }
    deriving (Show)

data Opcode = Halt | Add | Mult
    deriving (Show, Eq, Enum)

data Instruction = Instruction
    { op :: Opcode
    , arg1 :: Int
    , arg2 :: Int
    , resLoc :: Int
    }
    deriving (Show)

mkComputer :: Int -> [Int] -> IntcodeComputer
mkComputer pos instrs = IntcodeComputer pos (M.fromList (zip [0..] instrs))

initComputer :: IntcodeComputer -> Int -> Int -> IntcodeComputer
initComputer computer noun verb =
    computer { memory = M.insert 2 verb $ memory (computer { memory = M.insert 1 noun (memory computer)})}


getOpcode :: IntcodeComputer -> Maybe Opcode
getOpcode computer =
    case M.lookup (instrPtr computer) (memory computer) of
        Nothing -> Nothing
        Just 99 -> Just Halt
        Just 1  -> Just Add
        Just 2  -> Just Mult
        Just _  -> Nothing

relativeAddr :: IntcodeComputer -> Int -> Maybe Int
relativeAddr computer offset = M.lookup (instrPtr computer + offset) (memory computer)

directAddr :: IntcodeComputer -> Int -> Maybe Int
directAddr computer addr = M.lookup addr (memory computer)

-- Instruction args:
-- arg1 and arg2 are indirect addresses: arg is relative address to instruction pointer and the contents
-- of that address is the address where the actual parameter is located
-- arg3 is a relative address and the contents is the address for the result
makeInstruction :: IntcodeComputer -> Maybe Instruction
makeInstruction computer = do
    Instruction <$> getOpcode computer 
                <*> (relativeAddr computer 1 >>= directAddr computer)
                <*> (relativeAddr computer 2 >>= directAddr computer)
                <*> relativeAddr computer 3

execInstruction :: IntcodeComputer -> Maybe Instruction -> Maybe (Opcode, IntcodeComputer)
execInstruction computer instr =
    case instr of
        Nothing -> Nothing
        Just i ->
            case op i of
                Halt -> Just (Halt, computer)
                Add  -> Just (Add, computer { memory = M.insert (resLoc i) (arg1 i + arg2 i) (memory computer) })
                Mult -> Just (Mult, computer { memory = M.insert (resLoc i) (arg1 i * arg2 i) (memory computer) })

runCode :: IntcodeComputer -> Maybe IntcodeComputer
runCode computer = do
    case execInstruction computer (makeInstruction computer) of
        Nothing -> Nothing
        Just (Halt, computer') -> Just computer'
        Just (_, computer') -> runCode computer' { instrPtr = instrPtr computer' + 4 }

checkForValue :: [Int] -> Int -> [(Int, Int)] -> Maybe (Int, Int)
checkForValue _ _ [] = Nothing
checkForValue ops val ((n,v):xs) = 
    case runCode $ initComputer (mkComputer 0 ops) n v of
        Nothing -> checkForValue ops val xs
        Just computer' -> compareResult computer'
    where
        compareResult c = if directAddr c 0 == Just val then Just (n, v) else checkForValue ops val xs

main :: IO ()
main = do
    contents <- readFile "day-2-input.txt"
    let ops = map readInt . splitComma $ contents
    let computer = initComputer (mkComputer 0 ops) 12 2
    let c = runCode computer
    case c of
        Nothing -> putStrLn "Bad input"
        Just c' -> putStrLn $ "Result is " ++ show (M.lookup 0 (memory c'))

main2 :: IO ()
main2 = do
    contents <- readFile "day-2-input.txt"
    let ops = map readInt . splitComma $ contents
    let inputs = [(n,v) | n <- [0..99], v <- [0..99]]
    case checkForValue ops 19690720 inputs of
        Nothing -> putStrLn "Not found"
        Just (noun, verb) -> putStrLn $ "100 * noun + verb = " ++ show (100 * noun + verb)








                    

