-- Informatics 1 - Functional Programming 
-- Class Test 2022

module ClassExam where

import Data.Char
import Test.QuickCheck

-- Problem 1

-- a

f :: String -> Int
f word = sum [ ord x | x <- word, isAlpha x]

-- b

g :: String -> Int
g [] = 0
g (w:word)  | not (isAlpha w) = g word
            | otherwise = ord w + g word

-- c

h :: String -> Int
h word = sum (map ord (filter isAlpha word)) -- == sum . map ord . filter isAlpha word

-- d

prop_fgh :: String -> Bool
prop_fgh word = (f word == g word) && (g word == h word)

-- Problem 2

-- a
filterNonAlpha :: String -> String -> [(Char, Char)]
filterNonAlpha word1 word2 = filter (\(x, y) -> isAlpha x && isAlpha y) (zip word1 word2)

c :: String -> String -> Bool 
c word1 word2 = and [fst x == snd x | x <- filterNonAlpha word1 word2]

-- OORRRR
c1 xs ys = [xs !! i == ys !! i | i <- [0..min(length xs) (length ys) -1], isAlpha (xs !! i) && isAlpha (ys !! i)]


-- b

d :: String -> String -> Bool
d word1 [] = True
d [] word2 = True
d (w1:word1) (w2:word2) | not (isAlpha w1 && isAlpha w2) =  d word1 word2
                        | otherwise = (w1 == w2) && d word1 word2

-- c

prop_cd :: String -> String -> Bool
prop_cd word1 word2 = c word1 word2 == d word1 word2

--addstrs :: [Either Int String] -> String
--addstrs 