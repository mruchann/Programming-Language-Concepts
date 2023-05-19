module PE2 where

-- PE2: Dungeon Crawler
-- Dungeon map is :: Tree Chamber [Encounter]
-- Each encounter is either a fight or a treasure
-- Fights deal you damage (reduce HP) but enemies drop some gold (add
-- gold)
-- Tresures just give gold, or potions (which give hp)
-- Nodes hold encounters, when you visit a node you go through all of them in order
-- You start with a certain amount of HP and 0 gold.
-- You lose HP and accumulate gold as you descend the tree and go through encounters

-- Polymorphic tree structure
data Tree a b = EmptyTree | Leaf a b | Node a b [Tree a b] deriving (Show, Eq)

-- Every location in the tree is of some Chamber type.
data Chamber = Cavern | NarrowPassage | UndergroundRiver | SlipperyRocks deriving (Show, Eq)

-- An enemy has a name, an amount of damage that it deals
-- and an amount of gold that it drops (in that order).
data Enemy = Enemy String Integer Integer deriving (Show, Eq)

-- Gold n gives n amount of gold
-- Potion n heals n hp
data Loot = Gold Integer | Potion Integer deriving (Show, Eq)

-- An encounter is either a Fight with an Enemy, or a treasure where
-- you find Loot
data Encounter = Fight Enemy | Treasure Loot deriving (Show, Eq)

-- This is a type synonym for how we will represents our dungeons
type Dungeon = Tree Chamber [Encounter]

-- First argument is starting HP
-- Second argument is the dungeon map
-- Third argument is the path (each integer in the list shows what child
-- you descend into)
-- Calculate how much HP you have left and how much gold you've
-- accumulated after traversing the given path
traversePath :: Integer -> Dungeon -> [Int] -> (Integer, Integer)
traversePath hp dungeon path = 
    case (dungeon) of
        EmptyTree -> (hp, 0)
        Leaf _ encounters -> (hp - damage, gold)
            where
                damage = calculateDamageInEncounters encounters
                gold = calculateGoldInEncounters encounters
        Node _ encounters subtrees -> sumTuples (-damage, gold) (traversePath (hp) (subtrees !! (head path)) (excludeFirst path))
            where
                damage = calculateDamageInEncounters encounters
                gold = calculateGoldInEncounters encounters
                sumTuples (a, b) (c, d) = (a + c, b + d)
                excludeFirst [] = []
                excludeFirst (_:xs) = xs

calculateDamageInEncounters :: [Encounter] -> Integer
calculateDamageInEncounters encounters = 
    case encounters of 
    [] -> 0
    (Fight (Enemy _ damage _): rest) -> damage + (calculateDamageInEncounters rest)
    (Treasure (Potion potion): rest) -> -potion + (calculateDamageInEncounters rest)
    (Treasure (Gold _): rest) -> calculateDamageInEncounters rest
     
calculateGoldInEncounters :: [Encounter] -> Integer
calculateGoldInEncounters encounters = 
    case encounters of 
    [] -> 0
    (Fight (Enemy _ _ gold): rest) -> gold + (calculateGoldInEncounters rest)
    (Treasure (Gold gold): rest) -> gold + (calculateGoldInEncounters rest)  
    (Treasure (Potion _): rest) -> calculateGoldInEncounters rest


-- First argument is starting HP
-- Second argument is dungeon map
-- Find which path down the tree yields the most gold for you
-- You cannot turn back, i.e. you'll find a non-branching path
-- You do not need to reach the bottom of the tree
-- Return how much gold you've accumulated
findMaximumGain :: Integer -> Dungeon -> Integer
findMaximumGain hp dungeon = case dungeon of 
    EmptyTree -> 0
    Leaf _ encounters -> if (canEnterEncounters hp encounters) then calculateGoldInEncounters encounters else 0
    Node _ encounters subtrees -> if (canEnterEncounters hp encounters) then 
        (calculateGoldInEncounters encounters) + maximum (map (findMaximumGain (hp - (calculateDamageInEncounters encounters))) subtrees)
        else 0

canEnterEncounters :: Integer -> [Encounter] -> Bool
canEnterEncounters hp encounters = if hp <= 0 then False else
    case encounters of
    [] -> True
    (Fight (Enemy _ damage _): rest) -> canEnterEncounters (hp - damage) rest
    (Treasure (Potion potion) : rest) -> canEnterEncounters (hp + potion) rest
    (Treasure (Gold _): rest) -> canEnterEncounters hp rest

-- First argument is starting HP
-- Second argument is the dungeon map
-- Remove paths that you cannot go thorugh with your starting HP. (By
-- removing nodes from tree).
-- Some internal nodes may become leafs during this process, make the
-- necessary changes in such a case.

deleteEmptyTrees :: [Dungeon] -> [Dungeon]
deleteEmptyTrees [] = [] 
deleteEmptyTrees (x:xs) 
    | x == EmptyTree = deleteEmptyTrees xs
    | otherwise = x: deleteEmptyTrees xs

cleanUp :: Dungeon -> Dungeon
cleanUp dungeon = case dungeon of
    EmptyTree -> EmptyTree
    Leaf chamber encounters -> Leaf chamber encounters
    Node chamber encounters subtrees -> if (deleteEmptyTrees subtrees) == [] then (Leaf chamber encounters) 
                                        else (Node chamber encounters (deleteEmptyTrees (map cleanUp (deleteEmptyTrees subtrees))))

findViablePaths :: Integer -> Dungeon -> Dungeon
findViablePaths hp dungeon = case dungeon of 
    EmptyTree -> EmptyTree

    Leaf chamber encounters -> if (canEnterEncounters hp encounters) then Leaf chamber encounters else EmptyTree

    Node chamber encounters subtrees -> if (canEnterEncounters hp encounters) then 
        cleanUp (Node chamber encounters (map (findViablePaths (hp - (calculateDamageInEncounters encounters))) subtrees))
        else EmptyTree

height :: Dungeon -> Integer
height dungeon = case dungeon of 
    EmptyTree -> 0
    Leaf chamber encounters -> 1
    Node chamber encounters subtrees -> 1 + maximum (map (height) subtrees)

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a < x]
                      ++ [x] ++
               qsort [b | b <- xs, b >= x]

-- First argument is starting HP
-- Second Argument is dungeon map
-- Find, among the viable paths in the tree (so the nodes you cannot
-- visit is already removed) the two most distant nodes, i.e. the two
-- nodes that are furthest awat from each other.
mostDistantPair :: Integer -> Dungeon -> (Integer, Dungeon)
mostDistantPair hp dungeon = case dungeon of 
    EmptyTree -> (0, EmptyTree)
    Leaf chamber encounters -> if (canEnterEncounters hp encounters) then (1, Leaf chamber encounters) else (0, EmptyTree)
    Node chamber encounters subtrees -> if (canEnterEncounters hp encounters) then 
        (1 + maximum (map (fst) (map (mostDistantPair (hp - (calculateDamageInEncounters encounters))) subtrees)), 
        Node chamber encounters (map (snd) (map (mostDistantPair (hp - (calculateDamageInEncounters encounters))) subtrees)))
        else (0, EmptyTree)

-- Find the subtree that has the highest total gold/damage ratio
-- Simply divide the total gold in the subtree by the total damage
-- in the subtree. You only take whole subtrees (i.e you can take a new
-- node as the root of your subtree, but you cannot remove nodes
-- below it). Note that the answer may be the whole tree.
mostEfficientSubtree :: Dungeon -> Dungeon
mostEfficientSubtree dungeon = case dungeon of 
    EmptyTree -> EmptyTree
    Leaf chamber encounters -> Leaf chamber encounters
    Node chamber encounters subtrees -> findMaxRatioSubtree (findMaxRatio dungeon) dungeon
    where 
        calculateDamageInSubtree :: Dungeon -> Integer
        calculateDamageInSubtree dungeon = case dungeon of
            EmptyTree -> 0
            Leaf _ encounters -> calculateDamageInEncounters encounters
            Node _ encounters subtrees -> calculateDamageInEncounters encounters + sum (map calculateDamageInSubtree subtrees)

        calculateGoldInSubtree :: Dungeon -> Integer
        calculateGoldInSubtree dungeon = case dungeon of
            EmptyTree -> 0
            Leaf _ encounters -> calculateGoldInEncounters encounters
            Node _ encounters subtrees -> calculateGoldInEncounters encounters + sum (map calculateGoldInSubtree subtrees)

        calculateRatio :: Dungeon -> Double
        calculateRatio dungeon = case dungeon of
            EmptyTree -> 0
            Leaf _ encounters -> if calculateDamageInEncounters encounters <= 0 then 1000000000
                else (fromIntegral (calculateGoldInEncounters encounters)) / (fromIntegral (calculateDamageInEncounters encounters))
            Node _ encounters subtrees -> if calculateDamageInSubtree dungeon <= 0 then 1000000000
                else (fromIntegral (calculateGoldInSubtree dungeon)) / (fromIntegral (calculateDamageInSubtree dungeon))

        findMaxRatio :: Dungeon -> Double
        findMaxRatio dungeon = case dungeon of
            EmptyTree -> 0
            Leaf _ encounters -> calculateRatio dungeon
            Node _ encounters subtrees -> if calculateDamageInSubtree dungeon <= 0 then 1000000000 else
                max (calculateRatio dungeon) (maximum (map findMaxRatio subtrees))

        findMaxRatioSubtree :: Double -> Dungeon -> Dungeon
        findMaxRatioSubtree maxRatio dungeon = case dungeon of
            EmptyTree -> EmptyTree
            Leaf _ encounters -> if (calculateRatio dungeon) == (maxRatio) then dungeon else EmptyTree
            Node _ encounters subtrees -> if (calculateRatio dungeon) == (maxRatio) then dungeon 
            else if length(deleteEmptyTrees ((map (findMaxRatioSubtree maxRatio) subtrees))) == 0 then EmptyTree
            else head (deleteEmptyTrees ((map (findMaxRatioSubtree maxRatio) subtrees)))