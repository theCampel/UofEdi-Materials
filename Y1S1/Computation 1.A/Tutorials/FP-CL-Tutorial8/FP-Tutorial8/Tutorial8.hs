module Tutorial8 where  

import System.Random
import Test.QuickCheck

-- Importing the keymap module

import KeymapTree
import Language.Haskell.TH (Type(TupleT))
-- import KeymapTreeSol

-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item

-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]

-- Exercise 1
convertMaybeToWhole :: Maybe a -> a
convertMaybeToWhole (Just a) = a
convertMaybeToWhole Nothing = undefined

getItems :: [Barcode] -> Catalogue -> [Item]
getItems bars testDB = [convertMaybeToWhole (get bar testDB) | bar <- bars, (get bar testDB) /= Nothing]

-- Exercise 2

{-
*Tutorial8> db <- readDB
Done
(1.77 secs, 2,271,786,896 bytes)
*Tutorial8> size db
104651
(0.03 secs, 75,944 bytes)
*Tutorial8> ks <- samples 1000 db
(0.18 secs, 9,106,088 bytes)
*Tutorial8> force (getItems ks db)
()
(11.62 secs, 584,224 bytes)

If the database was two times bigger,
how would you expect the time to change?
For the getItems question, it would be 4 times as long

-}

-- for Exercises 3--6 check KeymapTree.hs 

-- Exercise 7

{-
*Tutorial8> db <- readDB
Done
(??? secs)
*Tutorial8> size db
???
(??? secs)
*Tutorial8> depth db
???
(??? secs)
*Tutorial8> ks <- loadKeys
(??? secs)
*Tutorial8> force (getItems ks db)
()
(??? secs)

If the database was two times bigger,
how would you expect the time to change?
-}

-- for Exercises 8--10 check KeymapTree.hs 

-- ** Input-output

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine (lines dbl))
            putStrLn (force (show db) `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

samples :: Int -> Catalogue -> IO [Barcode]
samples n db =
  do g <- newStdGen
     let allKeys = [ key | (key,item) <- toList db ]
     let indices = randomRs (0, length allKeys - 1) g
     let keys = take n [ allKeys !! i | i <- indices ]
     saveKeys keys
     return (force keys `seq` keys)

saveKeys :: [Barcode] -> IO ()
saveKeys = writeFile "keys.cache" . show

loadKeys :: IO [Barcode]
loadKeys = do
  keys <- read <$> readFile "keys.cache"
  return (force keys `seq` keys)

force :: [a] -> ()
force = foldr seq ()
