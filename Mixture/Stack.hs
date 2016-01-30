module Stack where


data Stack a = E | Stk a (Stack a)

push::a-> Stack a -> Stack a
push x s = Stk x s


pop::Stack a -> Stack a
pop E = error "Erreur pop d'une pile vide"
pop (Stk _ s) = s


top::Stack a -> a
top E = error "Erreur top d'une pile vide" 
top (Stk x _) = x 

emptyStack::Stack a
emptyStack = E


stackEmpty::Stack a -> Bool
stackEmpty E = True
stackEmpty _ = False
