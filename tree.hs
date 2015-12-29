data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

exampleTree = foldr treeInsert EmptyTree [8,6,4,1,7,3,5]

singleton :: (Ord a) => a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node y left right)
    | x == y = Node y left right
    | x < y = Node y (treeInsert x left) right
    | x > y = Node y left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node y left right)
    | x == y = True
    | x < y = treeElem x left
    | x > y = treeElem x right

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node a left right) =
      Node (f a) (fmap f left) (fmap f right)

instance Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node a left right) = 
      foldMap f left `mappend`
      f a `mappend`
      foldMap f right
