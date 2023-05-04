module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)

prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- ** Caesar Cipher Exercises

-- 1.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp ch tupes 
      | [snd tupe | tupe <- tupes, ch == fst tupe] == "" = ch
      | otherwise = head [snd tupe | tupe <- tupes, ch == fst tupe]

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec ch [] = ch
lookUpRec ch (t:tupes) 
            | ch == fst t = snd t
            | otherwise = lookUpRec ch tupes
                  

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp ch tupes = lookUp ch tupes == lookUpRec ch tupes


-- 2.
encipher :: Int -> Char -> Char
encipher shift original = lookUp original (makeKey shift)

-- 3.
normalise :: String -> String
normalise word = [toUpper x | x <- word, isAlpha x]

normaliseRec :: String -> String
normaliseRec [] = []
normaliseRec (w:word)
                  | isAlpha w = toUpper w : normaliseRec word
                  | otherwise = normaliseRec word 


prop_normalise :: String -> Bool
prop_normalise word = normalise word == normaliseRec word 

-- 4.
enciphers :: Int -> String -> String
enciphers shift word = map (encipher shift) (normalise word)

-- 5.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey keys = [(seco, firs) | (firs, seco) <- keys]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec (k:keys) = (snd k, fst k) : reverseKeyRec keys 

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey keys = reverseKey keys == reverseKeyRec keys 

-- 6.

-- map (lookUpRec char (reverseKey (makeKey shift))) (normalise word)

decipher :: Int -> Char -> Char
decipher shift ch = lookUpRec ch (reverseKey (makeKey shift)) 

decipherStr :: Int -> String -> String
decipherStr shift [] = []
decipherStr shift (e:encrypted) = decipher shift e : decipherStr shift encrypted 

-- ** Optional Material

-- 7.
-- 
genPossibilities :: String -> [String]
genPossibilities word =  [decipherStr shift word | shift <- [1..26]] 

--final :: String -> [(Int, String)]
--final word = zip [1..26] map (filter ("THE" `isInfixOf` (genPossibilities word)))
--

matches :: String -> [(Int, String)]
matches word = zip [1..26] (genPossibilities word)

--candidates :: String -> [(Int, String)]
--candidates word = [cands | cands <- matches word, "THE" `isInfixOf` (snd (matches word))]


splitEachFive :: String -> [String]
splitEachFive xs | length xs > 5 = take 5 xs : splitEachFive (drop 5 xs)
                 | otherwise     = [ fillToFive xs ]

fillToFive :: String -> String
fillToFive xs = xs ++ replicate (5 - length xs) 'X'

-- An alternative solution demonstrating 'repeat'
fillToFive' :: String -> String
fillToFive' xs = take 5 (xs ++ repeat 'X')

-- The following example shows why 'transpose' is not
--  invertible in general. The transpose function
--  takes the 'columns' of a list of lists, and makes
--  them the 'rows' of a new list of lists. 
--
-- [[o n e],           [[o t t f f],       [[o n e e e],
--  [t w o],            [n w h o i],        [t w o r],  
--  [t h r e e],   -->  [e o r u v],   -->  [t h r e],  
--  [f o u r],          [e r e],            [f o u], 
--  [f i v e]   ]       [e],        ]       [f i v]     ]   

-- 8.
encrypt :: Int -> String -> String
encrypt = undefined

-- 9.
decrypt :: Int -> String -> String
decrypt = undefined

--------------------
prop_a :: [Int] -> Bool
prop_a xs = maximum xs == foldr max 0 xs

prop_b :: [Int] -> Bool
prop_b xs 
      | xs /= [] = tail xs == drop 1 xs

--prop_c :: [Int] -> Bool
--prop_c xs 
--     | xs /= [] = head xs == take 1 xs

prop_d :: [Int] -> Bool
prop_d xs = drop (length xs) xs == []

prop_e :: [Int] -> Bool
prop_e xs = take (length xs) xs == xs

--testNeg :: [Int] -> [Int]
--testNeg h  =  foldr (*) 1 . (map cube) . (filter neg)
--            where
--            cube x  =  x * x * x
--            neg x   =  x < 0