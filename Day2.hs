module Day2 where

import qualified Data.Map as M

readInt :: String -> Int
readInt = read 

splitComma :: String -> [String]
splitComma xs = words $ map (\x -> if x == ',' then ' ' else x) xs

data IntcodeComputer = IntcodeComputer
    { current :: Int
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

getOpcode :: IntcodeComputer -> Maybe Opcode
getOpcode computer =
    case M.lookup (current computer) (memory computer) of
        Nothing -> Nothing
        Just 99 -> Just Halt
        Just 1  -> Just Add
        Just 2  -> Just Mult
        Just _  -> Nothing


getMemContents :: IntcodeComputer -> Int -> Maybe Int
getMemContents computer n = 
    case M.lookup (current computer + n) (memory computer) of
        Nothing -> Nothing
        (Just addr) -> M.lookup addr (memory computer)

makeInstruction :: IntcodeComputer -> Maybe Instruction
makeInstruction computer = do
    opcode <- getOpcode computer
    if opcode == Halt
        then return $ Instruction Halt (-1) (-1) (-1)
        else do
            arg1 <- getMemContents computer 1
            arg2 <- getMemContents computer 2
            res <-  M.lookup (current computer + 3) (memory computer)
            return $ Instruction opcode arg1 arg2 res

execInstruction :: IntcodeComputer -> Maybe Instruction -> Maybe (Opcode, IntcodeComputer)
execInstruction computer instr =
    case instr of
        Nothing -> Nothing
        Just i ->
            case op i of
                Halt -> Just (Halt, computer)
                Add  -> Just (Add, computer { memory = M.insert (resLoc i) ((arg1 i) + (arg2 i)) (memory computer) })
                Mult -> Just (Mult, computer { memory = M.insert (resLoc i) ((arg1 i) * (arg2 i)) (memory computer) })

runCode :: IntcodeComputer -> Maybe IntcodeComputer
runCode computer = do
    let result = execInstruction computer (makeInstruction computer)
    case result of
        Nothing -> Nothing
        Just (Halt, computer') -> Just computer'
        Just (_, computer') -> runCode computer' { current = current computer' + 4 }

main :: IO ()
main = do
    contents <- readFile "day-2-input.txt"
    let ops = map readInt . splitComma $ contents
    let computer = mkComputer 0 ops
    let computer' = computer { memory = M.insert 1 12 (memory computer) }
    let computer'' = computer' { memory = M.insert 2 2 (memory computer') }
    let c = runCode computer''
    case c of
        Nothing -> putStrLn "Bad input"
        Just c' -> putStrLn $ "Result is " ++ show (M.lookup 0 (memory c'))







                    

