
-- An example of Recursion (it calls itself inside of itself)
enumFromToo :: Int -> Int -> [Int]
enumFromToo m n | m <= n     = []
                | m > n    = m : enumFromToo (m-1) n

