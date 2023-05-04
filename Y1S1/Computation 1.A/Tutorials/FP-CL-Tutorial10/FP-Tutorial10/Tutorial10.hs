module Tutorial10 where

import Test.QuickCheck
import Control.Monad
import Data.Char

-- Question 1

-- 1a

ok :: String -> Bool
ok str = (and [isLower x && isAlpha x | x <- str]) && (length str < 6 )

-- 1b

f :: [String] -> String
f words = if null wordsOk 
            then "zzzzz" 
            else minimum wordsOk
          where
            wordsOk = [word | word <- words, ok word]
  
-- 1c



g :: [String] -> String
g (w:words) | length words == 1 = 
                  if ok w && (w < head words)
                    then w
                  else if ok w && (w > head words) 
                    then head words
                  else "zzzzz"
            | ok w && (w < head words) = g (words ++ [w])
            | otherwise = g words


-- 1d

h :: [String] -> String
h words = foldr min "zzzzz" (filter ok words)
 
-- Question 2

-- 2a

i :: [a] -> [a] -> [a]
i a b = tail a ++ [head b]

-- 2b

j :: [[a]] -> [[a]]
j words = undefined -- [ [i w (head words) | (w:words) <- ogList]  | ogList <- [words ++ [head words]]]

-- 2c

k :: [[a]] -> [[a]]
k (w:o:words)  = k' (w:o:words) w
                where
                  k' (w:[]) a = [i w a]
                  k' (w:o:words) a = i w o : k' (o:words) a


-- Question 3

data Prop = X
          | Y
          | T
          | F
          | Not Prop
          | Prop :&&: Prop
          | Prop :||: Prop
          | Prop :->: Prop
  deriving (Eq, Show)

instance Arbitrary Prop where
  arbitrary = sized gen
    where
    gen 0 =
      oneof [ return X,
              return Y,
              return T,
              return F ]
    gen n | n>0 =
      oneof [ return X,
              return Y,
              return T,
              return F,
              liftM Not prop,
              liftM2 (:&&:) prop prop,
              liftM2 (:||:) prop prop,
              liftM2 (:->:) prop prop]
      where
      prop = gen (n `div` 2)

-- 3a

eval :: Bool -> Bool -> Prop -> Bool
eval p q X = p
eval p q Y = q
eval p q (Not a) = not (eval p q a)
eval p q (a :&&: b) = (eval p q a) && (eval p q b)
eval p q (a :||: b) = (eval p q a) || (eval p q b)
eval p q (a :->: b) = (eval p q a) <= (eval p q b)


-- 3b
simple :: Prop -> Bool
simple X = True
simple Y = True
simple (Not a) = False
simple (a :&&: b) | ((a == T) || (a == F)) || ((b == T) || (b == F)) = False
                  | otherwise = simple a && simple b
simple (a :||: b) | ((a == T) || (a == F)) || ((b == T) || (b == F)) = False
                  | otherwise = simple a && simple b
simple (a :->: b) | ((a == T) || (a == F)) || ((b == T) || (b == F)) = False
                  | otherwise = simple a && simple b
simple c = True


-- 3c

simplify :: Prop -> Prop
simplify T = T
simplify F = F
simplify X = X
simplify Y = Y
simplify (Not T) = F
simplify (Not F) = T
simplify (F :&&: p) = F
simplify (p :&&: F) = F
simplify (T :&&: p) = simplify p
simplify (p :&&: T) = simplify p
simplify (p :&&: q) = simplify p :&&: simplify q
simplify (F :||: p) = simplify p
simplify (p :||: F) = simplify p
simplify (T :||: p) = T
simplify (p :||: T) = T
simplify (p :||: q) = simplify p :||: simplify q
simplify (F :->: p) = T
simplify (p :->: F) = Not (simplify p)
simplify (T :->: p) = simplify p
simplify (p :->: T) = T
simplify (p :->: q) = simplify p :->: simplify q