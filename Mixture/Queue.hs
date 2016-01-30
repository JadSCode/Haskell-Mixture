module Queue where


newtype Queue a = Q [a] deriving Show


enqueue::a -> Queue a -> Queue a
enqueue x (Q q) = Q (q ++ [x])


dequeue::Queue a -> Queue a
dequeue (Q []) = error "dequeue : file d'attente vide "
dequeue (Q (_:xs)) = Q xs


front::Queue a -> a
front (Q []) = error "front: file d'attente vide"
front (Q (x:_)) = x


queueEmpty::Queue a -> Bool
queueEmpty (Q []) = True
queueEmpty (Q _)  = False

emptyQueue::Queue a
emptyQueue  = Q []




