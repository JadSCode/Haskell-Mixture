module SplayTree where

import qualified Abr as Abr

------- LES ARBRES AUTO-AJUSTABLES splay --------

-- l'arbre est dèja ajusté 
splay x (Abr.Node y l r) | x == y = Abr.Node x l r
-- x est un fils du noeud courant (une simple rotation zig ou zag)
---- Zig : 
splay x (Abr.Node p (Abr.Node y l1 l2) r) | x == y = Abr.Node x l1 (Abr.Node p l2 r)
---- Zag : 
splay x (Abr.Node p l (Abr.Node y r1 r2))  | x == y = Abr.Node x (Abr.Node p l r1) r2
-- 

-- x est le fils gauche du fils gauche du noeud courant // zig zig 
splay x (Abr.Node g (Abr.Node p (Abr.Node y ll1 ll2 ) lr) r) | x == y = (Abr.Node x ll1 (Abr.Node p ll2 (Abr.Node g lr r)))
-- x est le fils droit du fils droit du noeud courant // zag zag
splay x (Abr.Node g l (Abr.Node p rl (Abr.Node y rr1 rr2))) | x == y = (Abr.Node x (Abr.Node p (Abr.Node g l rl) rr1) rr2)

-- Zig zag 

splay x (Abr.Node g (Abr.Node p ll (Abr.Node y lr1 lr2)) r) | x == y = Abr.Node x (Abr.Node p ll lr1) (Abr.Node g lr2 r)

-- Zag Zig 

splay x (Abr.Node g l (Abr.Node p (Abr.Node y rl1 rl2) rr)) | x == y = Abr.Node x (Abr.Node g l rl1) (Abr.Node p rl2 rr)


-- lorsque x n'est fils ni petit fils 

splay x Abr.E = Abr.E
splay x (Abr.Node y l r)    | x < y = splay x (Abr.Node y (splay x l) r)
                        | x > y = splay x (Abr.Node y l (splay x r))

-- insertion dans un arbre splay
inserer el arbre = splay el (Abr.add el arbre) --


-- access à un element dans l'arbre où bien le dernier élément qui mène vers el
acc el t@(Abr.Node y l r)    |el < y = if l == Abr.E then y else acc el l    
                         |el > y = if r == Abr.E then y else acc el r
                         |otherwise = el 

-- access + splay
access el t = splay (acc el t) t 



-- join (a, b) : fusioner deux arbres (en tenant en compte que a < b

join t Abr.E  = t
join Abr.E t  = t
join t1 t2 = Abr.Node elemax t1' t2 
             where 
                --Abr.Node elemax t1' E = access (maxTree t1) t1
                t1'     = Abr.delete elemax $ access elemax t1
                elemax  = Abr.maxTree t1
                     
    
-- membre : vérifier l'existence d'un élément dans l'arbre 
membre el t = valeur (access el t) == el



-- valeur d'un noeud 
valeur::Abr.BT a -> a
valeur (Abr.Node x _ _) = x


left Abr.E = Abr.E
left (Abr.Node _ l _) = l

right Abr.E = Abr.E 
right (Abr.Node _ _ r) = r


split i Abr.E = error "Erreur!"
split i t = (Abr.Node y lsplayed Abr.E, t2)
          where 
          lsplayed = left splayed
          t2 = right splayed
          y = valeur splayed
          splayed  = splay i t


delete i Abr.E = error "Erreur !"
delete i t |membre i tacc = join t1 t2
           |otherwise     = t
            where 
                t1 = left tacc
                t2 = right tacc
                tacc = access i t
           



abr  = Abr.build [5,1,8,9,2,6,3,4]
abr2 = Abr.build [15,11,18,19,12,16,13,14]
