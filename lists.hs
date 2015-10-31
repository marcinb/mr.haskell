infixr 7 :-:

data List a = EmptyList | a :-: (List a) deriving (Show, Read, Eq, Ord)

exampleList :: List Int
exampleList = foldr (:-:) EmptyList [1,2,3,4,5]

infixr 5 .++
(.++) :: List a -> List a -> List a
EmptyList .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

listElem :: (Eq a) => a -> List a -> Bool
listElem _ EmptyList = False
listElem x (y :-: ys)
    | x == y = True
    | otherwise = listElem x ys
