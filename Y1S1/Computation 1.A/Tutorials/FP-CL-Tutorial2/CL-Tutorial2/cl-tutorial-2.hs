-- Exercise 1:
-- Several could be the odd one out. Initially I believed B as it had no border around it. Then I believed D as it was the only Blue one. Then I thought C was another option as it was the only round object. Alternatively E as it's the only smaller shape.




data Thing = A | B | C | D | E
    deriving (Eq, Show)
things :: [Thing]
things = [A,B,C,D,E]

data Colour = Orange | Blue
    deriving (Eq, Show)
data Shape  = Square | Circle
    deriving (Eq, Show)
data Border = Thick | Thin
    deriving (Eq, Show)
data Size   = Big | Small
    deriving (Eq, Show)

colour :: Thing -> Colour
colour A = Orange
colour B = Orange
colour C = Orange
colour D = Blue
colour E = Orange

shape :: Thing -> Shape 
shape A = Square
shape B = Square
shape C = Circle
shape D = Square
shape E = Square

border :: Thing -> Border
border A = Thick
border B = Thin
border C = Thick
border D = Thick
border E = Thick

size :: Thing -> Size
size A = Big
size B = Big
size C = Big
size D = Big
size E = Small

type Predicate u = u -> Bool
isOrange :: Predicate Thing 
isOrange x = x `elem` [A, B, C, E]
isBlue :: Predicate Thing
isBlue x = x `elem` [D]
isSquare :: Predicate Thing
isSquare x = x `elem` [A,B,D,E]
isCircle :: Predicate Thing 
isCircle x = x `elem` [C]
isThick :: Predicate Thing 
isThick x = x `elem` [A,C,D,E]
isThin :: Predicate Thing 
isThin x = x `elem` [B]
isBig :: Predicate Thing 
isBig x = x `elem` [A,B,C,D]
isSmall :: Predicate Thing 
isSmall x = x `elem` [E]



--Exercise 4:
--1. > and [isThick n | n <- things, isBlue n]
--True
--2. > or [isBig n | n <- things, isOrange n]

--Exercise 5:
--1. It is not the case that some Square is Blue
-- >not (or [isBlue n | n <- things, isSquare n])
--False
--2. Every Square is not Blue
-- >and [not (isBlue n )| n <- things, isSquare n]
-- False