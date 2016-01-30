module GraphMatrix where

import Data.Array 
import Data.Maybe


emptyMatrix vinf vsup  = 
            array ((vinf,vinf), (vsup,vsup)) [((x1,x2), Nothing) | x1 <- range (vinf,vsup), x2 <- range (vinf,vsup)] 

mkGraph dir (vinf,vsup) es = 
                (emptyMatrix vinf vsup) // ( [((x1, x2),Just v)| (x1,x2,v) <- es] ++ 
                                         if dir then [] else [((x2,x1),Just w)| (x1,x2,w) <- es, x1/=x2])


nodes g = range (vinf, vsup) where  ((vinf,_), (_,vsup)) = bounds g

edgeIn g (x,y)= (g!(x,y)) /= Nothing

adjacent g u = [v| v<-nodes g, edgeIn g (u,v)] 






