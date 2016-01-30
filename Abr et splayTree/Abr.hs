module Abr where 

data BT a = E | Node a (BT a) (BT a) 
       deriving (Show, Eq)


length' E = 0
length' (Node _ l r) = 1+max (length' l) (length' r)

pre E = []
pre (Node x l r) =  (pre l) ++ [x] ++  (pre r)


add::(Ord a)=> a -> BT a -> BT a
add x E = Node x E E
add x t@ (Node y l r) | x == y = t
                      | x < y =  Node y (add x l) r
                      | otherwise = Node y l (add x r)


-- constuction d'un arbre à partir d'une liste
build::(Ord a) => [a] -> BT a
build xs = foldr add E  xs


-- Minimum d'un arbre 
minTree (Node x E _) = x
minTree (Node _ l _) = minTree l

-- Max d'un arbre
maxTree (Node x _ E) = x 
maxTree (Node _ _ r) = maxTree r


-- suppression d'un élément d'un arbre
delete x (Node y E r) | x == y = r
delete x (Node y l E) | x == y = l
delete x (Node y l r) | x < y  = Node y (delete x l) r
                      | x > y  = Node y l (delete x r)
                      | x == y = Node k l (delete k r)
                      where 
                        k = minTree r



