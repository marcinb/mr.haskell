map' f (x:xs) = (f x) : (map f xs)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith f xs ys)

quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) =
    let smallerSorted = filter (< x) xs
        biggerSorted = filter (>= x) xs
    in (quickSort' smallerSorted) ++ [x] ++ (quickSort' biggerSorted)

largestDivisable :: (Integral a) => a
largestDivisable = let p x = (mod x 3829) == 0
                   in head (filter p [100000, 99999..])


chain :: (Integral a) => a -> [a]
chain 1 = []
chain a
  | even a = a : chain (a `div` 2)
  | odd a = a : chain (a * 3 +1)


isLong :: [a] -> Bool
isLong xs = (length xs) > 15

chainsLongerThan15 :: (Integral a) => [a] -> [[a]]
chainsLongerThan15 [] = []
chainsLongerThan15 xs = filter isLong (map chain xs)

phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ]  

data Person = Person { 
            firstName :: String
            , lastName :: String
            , age :: Int
            } deriving (Eq, Show)
