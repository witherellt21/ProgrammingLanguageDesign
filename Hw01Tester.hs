{--
  CSCI 312 Homework #1

  Adpated from https://cs.pomona.edu/~michael/courses/csci131s18/hw/Hw01.html
--}

module Hw01 where

import qualified Data.Map as Map
import qualified Data.Set as Set
--import Data.Empty
--import Data.IntTree
import Data.Maybe

type Node = String
type DAG = Map.Map Node (Set.Set Node)

a = "a"
b = "b"
c = "c"
d = "d"
e = "e"

g = Map.fromList [(a, Set.fromList [b,c]),
                  (b, Set.fromList [d]),
                  (c, Set.fromList [d]),
                  (d, Set.fromList []),
                  (e, Set.fromList [a, c])]

data IntTree = Empty | Node IntTree Int IntTree deriving (Eq,Show)

-- Put your functions here --------------------
sumUp ::  [Int] -> Int
sumUp [] = 0
sumUp (x:xs) = x + sumUp(xs)

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs) = if x `mod` 2 == 0 then x:evens xs else evens xs

incAll :: [Int] -> [Int]
incAll (x:xs) = [k + 1 | k <- x:xs]

incBy :: Int -> [Int] -> [Int]
incBy n (x:xs) = [k + n | k <- x:xs]

append :: [Int] -> [Int] -> [Int]
append [] [] = []
append [] (y:ys) = y:ys
append (x:xs) [] = x:xs
append (x:xs) (y:ys) = x:(append xs (y:ys))

isLeaf :: IntTree -> Bool
isLeaf Empty = True
isLeaf (Node l x r) = l == Empty && r == Empty

sumTree :: IntTree -> Int
sumTree Empty = 0
sumTree (Node l x r) = sumTree (l) + x + sumTree(r)

fringe :: IntTree -> [Int]
fringe Empty = []
fringe (Node l x r) = if isLeaf (Node l x r) then [x] else append (fringe l) (fringe r)

maybeBounded :: Maybe Int -> Maybe Int -> Int -> Bool
maybeBounded Nothing Nothing x = True
maybeBounded Nothing (Just upper) x = x < upper
maybeBounded (Just lower) Nothing x = lower < x
maybeBounded (Just lower) (Just upper) x = lower < x && x < upper

getNode :: IntTree -> Maybe Int
getNode Empty = Nothing
getNode (Node l x r) = Just x

isBST :: IntTree -> Bool
isBST Empty = True
isBST (Node l x r) = isBST l && isBST r && maybeBounded (getNode l) (getNode r) x

sumUp' :: [Int] -> Int
sumUp' (x:xs) = x + foldr (+) 0 xs

evens' :: [Int] -> [Int]
evens' (x:xs) = filter (\n -> n `mod` 2 == 0) (x:xs)

incAll' :: [Int] -> [Int]
incAll' (x:xs) = map (\n -> n + 1) (x:xs)

incBy' :: Int -> [Int] -> [Int]
incBy' l (x:xs) = map (\n -> n + l) (x:xs)

map1 :: (a -> b) -> [a] -> [b]
map1 f [] = []
map1 f (x:xs) = foldr (\y ys -> (f y):ys) [] (x:xs)

filter1 :: (a -> Bool) -> [a] -> [a]
--filter1 f [] = []
--filter1 f (x:xs) = foldr () [] (x:xs)
filter1 f [] = []
filter1 f (x:xs) = if f x then x:filter1 f xs else filter1 f xs

sqrt' :: Float -> Maybe Float
sqrt' x = if x < 0 then Nothing else Just (sqrt x)

div' :: Float -> Float -> Either String Float
div' x y = if y == 0 then Left "Division by zero error." else Right (x/y)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

pairUp :: [a] -> [b] -> [(a,b)]
pairUp [] [] = []
--pairUp (x:xs) [] = []
--pairUp [] (y:ys) = []
pairUp (x:xs) (y:ys) = (x,y):pairUp xs ys

splitUp :: [(a,b)] -> ([a],[b])
--splitUp [] = []
--splitUp ((x,y):xs) = (x:splitUp xs,  y:splitUp xs)
splitUp xs = (map fst xs, map snd xs)

sumAndLength :: [Int] -> (Int,Int)
sumAndLength [] = (0,0)
sumAndLength l = (sumUp l, foldr (const (1+)) 0 l)

neighbors :: DAG -> Node -> Set.Set Node
neighbors dag node = dag Map.! node

hasPath :: DAG -> Node -> Node -> Bool
hasPath dag parent children = elem children (neighbors dag parent) || any (\x -> hasPath dag x children) (neighbors dag parent)

-- Tests ----------------------------------------

main = do

    putStrLn "Problem 1: natural recursion -----------------------------------\n"

    putStr "Should be 6: "
    print $ sumUp [1,2,3]

    putStr "Should be [2,4,6,8]: "
    print $ evens [1,2,3,4,5,6,7,8,9]

    putStr "Should be [2,3,4,5,6,7,8,9,10]: "
    print $ incAll [1,2,3,4,5,6,7,8,9]

    putStr "Should be [3,4,5,6,7,8,9,10,11]: "
    print $ incBy 2 [1,2,3,4,5,6,7,8,9]

    putStr "Should be [1,2,3]: "
    print $ append [] [1,2,3]

    putStr "Should be [1,2,3]: "
    print $ append [1,2,3] []

    putStr "Should be [1,2,3,4,5,6]: "
    print $ append [1,2,3] [4,5,6]

    putStrLn "\nProblem 2: data types -----------------------------------------\n"

    putStr "Should be True: "
    print $ isLeaf Empty

    putStr "Should be True: "
    print $ isLeaf (Node Empty 3 Empty)

    putStr "Should be False: "
    print $ isLeaf (Node (Node Empty 1 Empty) 2 Empty)

    putStr "Should be 10: "
    print $ sumTree (Node (Node Empty 1 Empty) 3 (Node Empty 2 (Node Empty 4 Empty)))

    putStr "Should be [2,7]: "
    print $ fringe (Node (Node Empty 1 (Node Empty 2 Empty)) 5 (Node (Node Empty 7 Empty) 10 Empty))


    putStrLn "\nProblem 3: binary search trees --------------------------------\n"

    putStr "Should be True: "
    print $ isBST (Node (Node Empty 2 Empty)  4 (Node Empty 5 Empty))

    putStr "Should be False: "
    print $ isBST (Node (Node Empty 5 Empty)  4 (Node Empty 2 Empty))

    putStrLn "\nProblem 4: map and filter -------------------------------------\n"

    putStr "Should be 6: "
    print $ sumUp' [1,2,3]

    putStr "Should be [2,4,6,8]: "
    print $ evens' [1,2,3,4,5,6,7,8,9]

    putStr "Should be [2,3,4,5,6,7,8,9,10]: "
    print $ incAll' [1,2,3,4,5,6,7,8,9]

    putStr "Should be [3,4,5,6,7,8,9,10,11]: "
    print $ incBy' 2 [1,2,3,4,5,6,7,8,9]

    putStrLn "\nProblem 5: defining higher-order functions --------------------\n"

    putStr "Should be [1,4,9,16,25]: "
    print $ map1 (\x -> x * x) [1,2,3,4,5]

    putStr "Should be [1,3,5,7,9]: "
    print $ filter1 odd [0,1,2,3,4,5,6,7,8,9]

    putStrLn "\nProblem 6: Maybe and Either ------------------------------------\n"

    putStr "Should be [0.0,1.0,2.0,3.0]: "
    print $ mapMaybe sqrt' [0,-1,1,-4,4,9,-9]


    putStrLn "\nProblem 7: Creating polymorphic data types ---------------------\n"

    putStr "Should be (\"hello\", 3): "
    print $ swap (3, "hello")

    putStr "Should be [(0,1),(2,3),(4,5),(6,7),(8,9)]: "
    print $ pairUp [0,2,4,6,8] [1,3,5,7,9]
    --print $ pairUp [0,2,4,6,8] [1,3,5,7]

    putStr "Should be ([0,2,4,6,8],[1,3,5,7,9]): "
    print $ splitUp [(0,1),(2,3),(4,5),(6,7),(8,9)]

    putStr "Should be (15, 5): "
    print $ sumAndLength [1,2,3,4,5]

    case div' 1 0 of
      Right val -> print $ val
      Left  msg -> putStrLn msg

    case div' 1 2 of
      Right val -> print $ val
      Left  msg -> putStrLn msg

    putStrLn "\nProblem 8: maps and sets --------------------------------------\n"

    putStr "Should be True: "
    print $ hasPath g a d

    putStr "Should be False: "
    print $ hasPath g a e

    