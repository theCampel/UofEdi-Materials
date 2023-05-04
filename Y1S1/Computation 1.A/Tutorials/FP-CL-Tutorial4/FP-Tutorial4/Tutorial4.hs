module Tutorial4 where

import Data.Char
import Data.List
import Test.QuickCheck


-- ** Optional material

-- 1. doubles
-- a.
doublesComp :: [Int] -> [Int]
doublesComp xs =  [x*2 | x <- xs]

-- b.
doublesRec :: [Int] -> [Int]
doublesRec [] = []
doublesRec (x:xs) = x*2 : doublesRec xs

-- c.
doublesHO :: [Int] -> [Int]
doublesHO =  map (*2)

-- d.
prop_doubles :: [Int] -> Bool
prop_doubles xs =  (doublesComp xs == doublesRec xs) == (doublesHO xs == doublesRec xs)

-- 2. aboves
-- a.
abovesComp :: Int -> [Int] -> [Int]
abovesComp n ms =  [m | m <- ms, m>n ]

-- b.
abovesRec :: Int -> [Int] -> [Int]
abovesRec n [] = []
abovesRec n (m:ms)  | m > n = m: abovesRec n ms
                    | otherwise = abovesRec n ms
 
-- c.
abovesHO :: Int -> [Int] -> [Int]
abovesHO n =  filter (>n)

-- d.
prop_aboves :: Int -> [Int] -> Bool
prop_aboves n ms =  (abovesComp n ms == abovesRec n ms) == (abovesComp n ms == abovesHO n ms)

-- 3. parity
-- a.
xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (not a && b)

-- b.
parityRec :: [Bool] -> Bool
parityRec [] = True
parityRec (b:bools) | not b = parityRec bools
                    | otherwise = b `xor` (parityRec bools)

-- c.
parityHO :: [Bool] -> Bool
parityHO bools =  foldr xor True bools

-- d.
prop_parity :: [Bool] -> Bool
prop_parity bools =  parityRec bools == parityHO bools

-- 4. allcaps
-- a.
allcapsComp :: String -> Bool
allcapsComp word = and [isUpper ch | ch <- word, isAlpha ch]

-- b.
allcapsRec :: String -> Bool
allcapsRec "" = True
allcapsRec (w:word) | isAlpha w = isUpper w && allcapsRec word  
                    | otherwise = allcapsRec word

-- c.
allcapsHO :: String -> Bool
allcapsHO word = foldr (&&) True (map isUpper (filter isAlpha word))

-- d.
prop_allcaps :: String -> Bool
prop_allcaps word = allcapsRec word == allcapsHO word


-- ** Optional material
-- Matrix manipulation

type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform nums = all (==head nums) nums

-- b.
checkLengths :: Matrix -> Bool
checkLengths matri = uniform (map length matri)

checkMinShape :: Matrix -> Bool
checkMinShape matri = (length matri > 1) && (length (head matri) > 1)

valid :: Matrix -> Bool
valid matri = checkLengths matri && checkMinShape matri


-- 6.
width :: Matrix -> Int
width m = length (head m)

height :: Matrix -> Int
height m = length m

plusRow :: [Rational] -> [Rational] -> [Rational]
plusRow rowNofA rowNofB = zipWith (+) rowNofA rowNofB

testSize :: Matrix -> [Rational]
testSize mat = mat !! 0

plusM :: Matrix -> Matrix -> Matrix
plusM matA matB | not ((valid matA && valid matB) && (width matA == width matB) && (height matA == height matB)) = error "Matrices incorrect Size"
                | otherwise = [plusRow (matA !! n) (matB !! n) | n <- [0..length matA-1]]


-- 7.
dotProdOfRowAndMatrix :: [Rational] -> Matrix -> [Rational]
dotProdOfRowAndMatrix r1 m2 = [sum (zipWith (*) r1 v2) | v2 <- transpose m2]


timesM :: Matrix -> Matrix -> Matrix
timesM m1 m2    | not ((width m1 == height m2) && (valid m1 && valid m2)) = error "Matrices Incorrect Size"
                | otherwise = [dotProdOfRowAndMatrix vn m2 | vn <- m1]


-- ** Challenge

-- 8.
-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]     
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined
        
determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined        
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse2 :: Rational -> Rational -> Rational 
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)
        
prop_inverse3 :: Triple Rational -> 
                 Triple Rational -> 
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined
