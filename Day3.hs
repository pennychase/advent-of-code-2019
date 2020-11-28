module Day3 where

import Data.Maybe ( catMaybes )
import Data.List ( minimumBy )

data Point = Point Int Int
    deriving (Show, Eq, Ord)

data Edge = Edge Point Point
    deriving (Show, Eq, Ord)

--
-- For parsing the Directions
--

readInt :: String -> Int
readInt = read 

splitComma :: String -> [String]
splitComma xs = words $ map (\x -> if x == ',' then ' ' else x) xs

parseDirection :: String -> (Char, Int)
parseDirection str = (head str, readInt (tail str))

--
-- For creating the paths as a list of Edges and converting to a list of Points
--

-- Given a Point and a direction, construct the edge by adding/subtracting the
-- length to the coordinate indicated by the direction
mkEdge :: Point -> (Char, Int) -> Edge
mkEdge current direction = Edge current (mkPoint current direction)
    where
        mkPoint (Point x y) (d, n) = 
            case d of
                'R' -> Point (x + n) y
                'L' -> Point (x - n) y
                'U' -> Point x (y + n)
                'D' -> Point x (y - n)

mkEdges :: Point ->  [(Char, Int)] -> [Edge]
mkEdges _ [] = []
mkEdges point (d:ds) =  edge : mkEdges n2 ds
    where
        edge@(Edge _ n2) = mkEdge point d

pathFromEdges :: [Edge] -> [Point]
pathFromEdges edges = origin : map getPoint edges
    where
        Edge origin _ = head edges
        getPoint (Edge _ point) = point

--
-- Utilities for edges
--

-- Determine if an edge is horizontal
horizontal :: Edge -> Bool
horizontal (Edge (Point x1 _) (Point x2 _)) = x1 == x2

-- Determine if an edge is vertical
vertical :: Edge -> Bool
vertical (Edge (Point _ y1) (Point _ y2)) = y1 == y2

-- Determine if an x or y coordinate is between the x or y coordinates of an edge
coordBetween :: Int -> Int -> Int -> Bool
coordBetween c c1 c2 = c >= min c1 c2 && c <= max c1 c2

--
-- Functions to find all the intersections between two paths
--

-- Find all the points where the two paths (defined by their edges) intersect
pathsIntersects :: [Edge] -> [Edge] -> [Maybe Point]
pathsIntersects p = 
    foldr (\x xs -> map (edgeIntersect x) p ++ xs) []

-- Find the Point at which two edges intersect
-- Determine which edge is horizontal and which is vertical, and pass to
-- intercept in that order
edgeIntersect :: Edge -> Edge -> Maybe Point
edgeIntersect edge1 edge2 
    | horizontal edge1 && vertical edge2 = intercept edge1 edge2
    | vertical edge1 && horizontal edge2 = intercept edge2 edge1
    | otherwise = Nothing

-- Finds the point where a horizontal edge and a vertical edge intersect (if they intersect)
intercept :: Edge -> Edge -> Maybe Point
intercept hEdge vEdge = 
    Point <$> xIntercept hEdge vEdge <*> yIntercept hEdge vEdge
    where
        xIntercept :: Edge -> Edge -> Maybe Int
        xIntercept (Edge (Point x _) _) (Edge (Point x1 _) (Point x2 _))
            | coordBetween x x1 x2 = Just x
            | otherwise = Nothing
        yIntercept :: Edge -> Edge -> Maybe Int
        yIntercept (Edge (Point _ y1) (Point _ y2)) (Edge (Point _ y) _)
            | coordBetween y y1 y2 = Just y
            | otherwise = Nothing

--
-- Distance functions
--

-- Distance between two points uses the Manhattan distance since we're on a grid
manhattanDistance :: Point -> Point -> Int
manhattanDistance (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

pathLength :: [Point] -> Int
pathLength path = sum $ zipWith manhattanDistance path (tail path)

-- Part 1 Problem
-- Find the intersection point that is closest to the origin (Node 1 1) using Manhattan distance
-- Note that paths must intersect at the origin, so we have to exclude the origin
closestIntersection :: [Maybe Point] -> (Point, Int)
closestIntersection maybeIntersections =
    if null intersections 
        then error "No intersection"
        else minimumBy myCompare $ distances intersections
    where
        intersections = filter (/= Point 1 1) $ catMaybes maybeIntersections
        distances xs = zip xs (map (manhattanDistance (Point 1 1)) xs)
        myCompare p1 p2 = compare (snd p1) (snd p2)

-- Part 2 problem
-- Find the intersection point that is closest to the origin (Node 1 1) using path length
-- Note that paths must intersect at the origin, so we have to exclude the origin
closestIntersection' :: [Edge] -> [Edge] -> [Maybe Point] -> (Point, Int)
closestIntersection' path1 path2 maybeIntersections =
    if null intersections 
        then error "No intersection"
        else minimumBy myCompare $ distances intersections
    where
        intersections = filter (/= Point 1 1) $ catMaybes maybeIntersections
        distances xs = zip xs (map (combinedPathLength path1 path2) xs)
        myCompare p1 p2 = compare (snd p1) (snd p2)

combinedPathLength :: [Edge] -> [Edge] -> Point -> Int
combinedPathLength p1 p2 pt = 
    onePathLength p1 pt + onePathLength p2 pt
    where
        onePathLength p pt = pathLength . pathFromEdges $ pathToIntercept p pt
        
pathToIntercept :: [Edge] -> Point -> [Edge]
pathToIntercept path point = go path point
    where
        onEdge (Point x y) edge@(Edge (Point x1 y1) (Point x2 y2))
            | horizontal edge   = x == x1 && coordBetween y y1 y2
            | vertical edge     = y == y1 && coordBetween x x1 x2
            | otherwise         = False
        go [] _ = []
        go (e@(Edge p1 _):es) pt =
            if onEdge pt e 
                then [Edge p1 pt]
                else e : go es pt

main :: IO ()
main = do
    contents <- readFile "day-3-input.txt"
    let [d1, d2] = map  (map parseDirection . splitComma) (lines contents)
    let p1 = mkEdges (Point 1 1) d1
    let p2 = mkEdges (Point 1 1) d2
    let intersects = pathsIntersects p1 p2
    print $ closestIntersection intersects
    print $ closestIntersection' p1 p2 intersects
    

    







