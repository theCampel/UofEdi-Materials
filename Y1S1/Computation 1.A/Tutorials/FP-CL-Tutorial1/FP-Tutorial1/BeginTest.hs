import Data.Time.Format.ISO8601 (yearFormat)
import Data.Time (DayOfWeek(Wednesday, Thursday, Friday, Saturday, Sunday))
import Text.Printf
import Data.Char

--module BeginTest where

double :: Int -> Int    -- Declaring variable
double xy = xy + xy        -- Assining variable to value. 
                        -- (Variables can't really be changed on the fly here - I think)

equ :: Int -> Int -> Bool
equ x y = x==y

test :: Int -> Int -> String
test x y = if x > y then printf "%x is bigger" x else "Y is bigger"

test2 :: Int -> Int -> String
test2 x y
    | x > y = printf "%x is bigger" x
    | x < y = printf "%y is bigger" y -- Error for some reason
    | otherwise = "They're the same number" 

data Weekdays = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

capitalise :: String -> String 
capitalise "" = ""
capitalise (c:cs) = (toUpper c) : c : c : cs

--cartMat :: [Int] -> [Int] -> [[Int]]
--cartMat a b = [a] * [b]