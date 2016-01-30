module ParcoursDFS where

import Graph 
import Queue


-- BreadthFrst sans concaténation en utilisant une File d'attente

breadthFirst start g = reverse $ bfs (enqueue start emptyQueue) []
 where
  bfs q visited | queueEmpty q = visited
                | elem (front q) visited = bfs (dequeue q) visited
                | otherwise  = bfs (foldr enqueue (dequeue q) (adjacent g c)) (c:visited)
                where c = front q

