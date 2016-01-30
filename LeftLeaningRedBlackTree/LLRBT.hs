module LLRBT where


data Color = Red | Black  deriving (Show)
data RBTree a = E | N Color (RBTree a) a (RBTree a)  deriving (Show)




insert :: Ord a => a -> RBTree a -> RBTree a
insert el t = toBlack $ ins el t
    where 
        ins::Ord a => a -> RBTree a -> RBTree a
        ins el E = N Red E el E
        ins el t@(N clr l y r) = flipCouleur $ rotationR $ rotationL $ case compare el y of 
            LT -> N clr (ins el l) y r
            EQ -> t
            GT -> N clr l  y (ins el r)
             

-- changer la couleur du noeud en noir
toBlack:: Ord a => RBTree a -> RBTree a
toBlack E = E
toBlack (N _ l y r) = N Black l y r


--flipCouleur : (rouge) <-- (noir) --> (rouge) =====> (noir) <-- (rouge) --> (noir)

flipCouleur :: Ord a => RBTree a -> RBTree a
flipCouleur (N Black l@(N Red _ _ _) y r@(N Red _ _ _)) = N Red (toBlack l) y (toBlack r)
flipCouleur t = t



-- Rotation à droite , dans le cas où 2 noeud rouges sont adjacents 
rotationR :: Ord a => RBTree a -> RBTree a
rotationR (N clr (N Red l'@(N Red _ _ _) y r') x r) = N clr l' y (N Red r' x r)
rotationR t = t


--Rotatoin à gauche dans le cas d'un noeud rouge à droite
rotationL :: Ord a => RBTree a -> RBTree a
rotationL (N clr l x (N Red l' y r') ) = N clr (N Red l x l') y r'
rotationL t = t




fromList :: Ord a => [a] -> RBTree a
fromList = foldr insert E



pre E = []
pre (N _ l x r) =  (pre l) ++ [x] ++  (pre r)



