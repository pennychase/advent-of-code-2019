module Day3 where

import Data.Maybe ( catMaybes )
import Data.List ( minimumBy )

data Node = Node Int Int
    deriving (Show, Eq, Ord)

data Edge = Edge Node Node
    deriving (Show, Eq, Ord)

readInt :: String -> Int
readInt = read 

splitComma :: String -> [String]
splitComma xs = words $ map (\x -> if x == ',' then ' ' else x) xs

parseDirection :: String -> (Char, Int)
parseDirection str = (head str, readInt (tail str))

mkEdge :: Node -> (Char, Int) -> Edge
mkEdge current direction = Edge current (mkNode current direction)
    where
        mkNode (Node x y) (d, n) = 
            case d of
                'R' -> Node (x + n) y
                'L' -> Node (x - n) y
                'U' -> Node x (y + n)
                'D' -> Node x (y - n)

mkPath :: Node ->  [(Char, Int)] -> [Edge]
mkPath _ [] = []
mkPath node (d:ds) =  edge : mkPath n2 ds
    where
        edge@(Edge _ n2) = mkEdge node d

horizontal :: Edge -> Bool
horizontal (Edge (Node x1 _) (Node x2 _)) = x1 == x2

vertical :: Edge -> Bool
vertical (Edge (Node _ y1) (Node _ y2)) = y1 == y2

-- Finds the point where a horizontal edge and a vertical edge intersect (if they intersect)
intercept :: Edge -> Edge -> Maybe Node
intercept hEdge vEdge = 
    Node <$> xIntercept hEdge vEdge <*> yIntercept hEdge vEdge
    where
        xIntercept :: Edge -> Edge -> Maybe Int
        xIntercept (Edge (Node x _) _) (Edge (Node x1 _) (Node x2 _))
            | x >= min x1 x2 && x <= max x1 x2 = Just x
            | otherwise = Nothing
        yIntercept :: Edge -> Edge -> Maybe Int
        yIntercept (Edge (Node _ y1) (Node _ y2)) (Edge (Node _ y) _)
            | y >= min y1 y2 && y <= max y1 y2 = Just y
            | otherwise = Nothing

edgeIntersect :: Edge -> Edge -> Maybe Node
edgeIntersect edge1 edge2 
    | horizontal edge1 && vertical edge2 = intercept edge1 edge2
    | vertical edge1 && horizontal edge2 = intercept edge2 edge1
    | otherwise = Nothing

pathsIntersects :: [Edge] -> [Edge] -> [Maybe Node]
pathsIntersects p = 
    foldr (\x xs -> map (edgeIntersect x) p ++ xs) []

manhattanDistance :: Node -> Node -> Int
manhattanDistance (Node x1 y1) (Node x2 y2) = abs (x1 - x2) + abs (y1 - y2)

closestIntersection :: [Maybe Node] -> (Node, Int)
closestIntersection maybeIntersections =
    if null intersections 
        then error "No intersection"
        else minimumBy myCompare $ distances intersections
    where
        intersections = filter (/= Node 1 1) $ catMaybes maybeIntersections
        distances xs = zip xs (map (manhattanDistance (Node 1 1)) xs)
        myCompare p1 p2 = compare (snd p1) (snd p2)

main :: IO ()
main = do
    contents <- readFile "day-3-input.txt"
    let [d1, d2] = map  (map parseDirection . splitComma) (lines contents)
    let p1 = mkPath (Node 1 1) d1
    let p2 = mkPath (Node 1 1) d2
    print $ closestIntersection (pathsIntersects p1 p2)

    







