import Data.List
import qualified Data.List.Key as K
import Data.Map ((!), fromList, fromListWith, adjust, keys, Map)
 

-- Construire le graphe à partir d'une liste (noeud1, noeud2, coût)
buildGraph :: Ord a => [(a, a, Float)] -> Map a [(a, Float)]
buildGraph g = fromListWith (++) $ g >>=
               \(a,b,d) -> [(a,[(b,d)]), (b,[(a,d)])]



-- Trouver le chemin le plus proche entre deux noeux (from et to)  dans un graphe (graph)
shortestPath :: Ord a => a -> a -> Map a [(a, Float)] -> [a]
shortestPath from to graph = reverse $ f to where -- (retour en arrière après avoir trouvé le chemin vers la destination)
    f x = x : maybe [] f (snd $ dijkstra from graph ! x)
        where 
            dijkstra :: Ord a => a -> Map a [(a, Float)] -> Map a (Float, Maybe a)
            dijkstra source graph =
                f (fromList [(v, (if v == source then 0 else 1/0, Nothing)) -- on marque le noeud source par zero , et les autres par l'infini 
                            | v <- keys graph]) (keys graph) where
                f ds [] = ds
                f ds q  = f (foldr relax ds $ graph ! m) (delete m q) where -- la phase de relaxation & le marquage des noeuds visités 
                          m = K.minimum (fst . (ds !)) q -- on prend le minimum des valeurs pour pour le prochain calcul
                          relax (e,d) = adjust (min (fst (ds ! m) + d, Just m)) e -- la fonction qui se charge de la relaxation 
 


main = do let g = buildGraph [('a','c',2), ('a','d',6), ('b','a',3),
                              ('b','d',8), ('c','d',7), ('c','e',5),
                              ('d','e',10)]
          print $ shortestPath 'a' 'e' g == "ace"

-- Il est à signaler que cette implémentation n'est pas la mienne 
-- https://bonsaicode.wordpress.com/2011/01/04/programming-praxis-dijkstra%E2%80%99s-algorithm/
-- Il faut installer aussi ce package http://hackage.haskell.org/package/utility-ht pour que la fonction K.minimum marche 
