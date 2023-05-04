module Tutorial7 where

import LSystem
import Test.QuickCheck

pathExample = Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30

-- 1a. copy
copy :: Int -> Command -> Command --Alternatively: foldr (:#:) Sit (replicate reps com)
copy reps com   | reps == 0 = Sit
                | otherwise = com :#: copy (reps-1) com


-- 1b. polygon
polygon :: Distance -> Int -> Command
polygon dist numSides =  copy numSides (Go dist :#: Turn (360.0/fromIntegral numSides))

-- 2. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n
    where 
        f 0 = Go 10
        f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: p :#: f (x-1)
        p = Turn (-60)
        n = Turn 60

-- 3. sierpinski
sierpinski :: Int -> Command
sierpinski x =  f x
    where 
        f 0 = Go 10
        f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
        g 0 = Go 10
        g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f (x-1)
        p = Turn (-60)
        n = Turn 60
     
-- 4. hilbert
hilbert :: Int -> Command
hilbert =  undefined

-- 5. dragon
dragon :: Int -> Command
dragon =  undefined

-- ** Optional Material

removeChars :: Command -> String
removeChars comm = filter (`notElem` ":#()") (show comm) 

removeSit :: Command -> [String]
removeSit comm = [coms | coms <- words (removeChars comm), coms /= "Sit"]

-- 6a. split
split :: Command -> [Command] -- Change back to [Command]
split Sit = []
split (Turn x) = [Turn x]
split (Go x) = [Go x]
split (a :#: b) = split a ++ split b

-- 6b. join
join :: [Command] -> Command
join comms = head comms :#: join (tail comms) -- Dont forget to remove Sit

-- 6c. equivalent
equivalent :: Command -> Command -> Bool
equivalent comm1 comm2 =  split comm1 == split comm2

-- 6d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = join (split c) == c

prop_split :: Command -> Bool
prop_split comm1 =  not ((Sit `elem` (split comm1)) || ":#" `elem` [show ( split comm1)])
-- OR:          and (map f (split comm1))

--                  where
--                      f :: Command -> Bool
--                      f (x :#: y) = false
--                      f (Sit) = True
--                      f _ = True



-- 7. optimise
optimise :: Command -> Command
optimise = undefined
