data Tree = Leaf | Node Int Tree Tree
    deriving Show

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ left right) = 
    1 + max (treeDepth left) (treeDepth right)

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node value left right) =
    value + (treeSum left) + (treeSum right)

isTreeSorted :: Tree -> Int -> Int -> Bool
isTreeSorted Leaf _ _ = True
isTreeSorted (Node value left right) minBoundary maxBoundary =
    minBoundary < value &&
    value < maxBoundary &&
    isTreeSorted left minBoundary value &&
    isTreeSorted right value maxBoundary

addNewMax :: Tree -> Tree
addNewMax Leaf = (Node 0 Leaf Leaf)
addNewMax (Node value left Leaf) = (Node value left (Node (value + 1) Leaf Leaf))
addNewMax (Node value left right) = (Node value left (addNewMax right))

insert :: Tree -> Int -> Tree
insert Leaf toInsert    = Node toInsert Leaf Leaf
insert (Node value left right) toInsert
    | value < toInsert  = (Node value (insert left toInsert) right)
    | otherwise         = (Node value left (insert right toInsert))

toList :: Tree -> [Int]
toList Leaf = []
toList (Node value left right) = 
    value: ((toList left) ++ (toList right))


