module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck
import Text.Read (Lexeme(Char))


-- 1. inRange

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x >= lo, x<=hi]


-- 2. multDigits
-- NB: Neg Numbers don't really count here.  
multDigits :: String -> Int
multDigits str  | null [digitToInt digs | digs <- str, isDigit digs] = 1 -- This is unnecessary as produt will return one for an empty list.
                | otherwise = product [digitToInt digs | digs <- str, isDigit digs]

countDigits :: String -> Int
countDigits str = length [digitToInt digs | digs <- str, isDigit digs] 

prop_multDigits :: String -> Bool
prop_multDigits str = multDigits str <= 9^(countDigits str)


-- 3. capitalise and title


lowerAll :: String -> String
lowerAll str = [toLower cars | cars <- str]

capitalise :: String -> String
capitalise (c:cs) = toUpper c: lowerAll cs


title :: [String] -> [String]
title (w:words) = (capitalise w):[if length words >= 4 then capitalise words else lowerAll words | words <- words ]

-- 4. score and totalScore


isVowel :: Char -> Bool
isVowel v = toLower v `elem` "aeiou"

score :: Char -> Int
score x     | isUpper x && isVowel x = 3
            | not (isUpper x) && isVowel x = 2
            | isUpper x && not (isVowel x) = 2
            | not (isUpper x) && not (isVowel x) && isLetter x = 1
            | otherwise = 0

removeZeros :: [Int] -> [Int]
removeZeros scores = [n | n <- scores, n>0]

totalScore :: String -> Int
totalScore xs =  product (removeZeros (map score xs))


prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs >= 1 



-- ** Optional Material

-- 5. crosswordFind

getWordsOfSameLength :: [String] -> Int -> [String]
getWordsOfSameLength words len = [x | x <- words, length x == len]

isLInPosP :: Char -> Int -> String -> Bool
isLInPosP letter pos word = pos `elem` (elemIndices letter (word) )  


crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [x | x <- getWordsOfSameLength words len, isLInPosP letter pos x ]

-- orrrr:
-- crosswordFind :: Char -> Int -> Int -> [String] -> [String]
-- crosswordFind letter pos len words = 

-- 6. search

makeIndices :: String -> [Int]
makeIndices word = [0.. (length word)-1]

findChars :: String -> Char -> [Int]
findChars word letter = [if letter == x then 1 else 0 | x <- word]

mergeIndicesAndChars :: [Int] -> [Int] -> [(Int, Int)]
mergeIndicesAndChars indices chars = zip indices chars  

getIndicesOfChars :: [(Int, Int)] -> [Int]
getIndicesOfChars indicesAndChars = [x | (x,y) <- indicesAndChars, y>=1]

search :: String -> Char -> [Int]
search word letter = getIndicesOfChars(mergeIndicesAndChars (makeIndices word) (findChars word letter ))



-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = length (search str goal) <= length str

