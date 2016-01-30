module ParcoursDFS where

import Graph 
import Stack


-- DepthFirst sans concaténation

depthFirst noeudInit g = reverse $ dfs [noeudInit] []
  where
   dfs [] visited     = visited
   dfs (c:cs) visited 
      | elem c visited = dfs cs visited
      | otherwise      = dfs ((adjacent g c)++cs) (c:visited)


-- DepthFirst sans concaténation & en utilisant une pile 

depthFirst' noeudInit g = reverse $ dfs (push noeudInit emptyStack) []
 where
    dfs pile visited |stackEmpty pile         = visited
                     |elem (top pile) visited = dfs (pop pile) visited
                     |otherwise               = dfs (foldr push (pop pile) (adjacent g c)) (c:visited) 
                     where c = top pile



