module LeftistHeap where 

data HP a = E | Node Int a (HP a) (HP a) 
     deriving (Show)



rang::HP a -> Int
rang E = 0
rang (Node r _ _ _) = r


size::HP a -> Int 
size E = 0
size (Node x _ l r) = 1 + size l + size r


findH::(Ord a) => HP a -> a
findH E  = error "Empty heap"
findH (Node _ el _ _) = el


merge::(Ord a) => HP a -> HP a -> HP a
merge E h = h
merge h E = h
merge h1@(Node _ x a1 b1) h2@(Node _ y a2 b2) | x <= y = make x a1 (merge b1 h2)
                                              | otherwise = make y a2 (merge h1 b2)

make::(Ord a) => a -> HP a -> HP a -> HP a
make x a b | rang a >= rang b = Node (rang b + 1) x a b
           | otherwise = Node (rang a + 1) x b a



insert x heap = merge (Node 1 x E E) heap


deleteH::(Ord a) => HP a -> HP a
deleteH E = error "Empty heap"
deleteH (Node _ _ a b) = merge a b 


deuxMin::(Ord a) => HP a -> (a, a, HP a)
deuxMin heap = (m1, m2, p')
        where 
            m1 = findH heap
            q  = deleteH heap
            m2 = findH q
            p' = deleteH q 
        


























