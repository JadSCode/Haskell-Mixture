module Huffman where

import qualified Data.Map as M
import qualified LeftistHeap as T
import Data.Maybe

data HF = Leaf Char Int | Node Int (HF) (HF) deriving (Show)

instance Eq HF where 
      a == b = frq a == frq b

instance Ord HF where 
      compare a b = compare (frq a) (frq b)


frq::HF -> Int
frq (Leaf _ f) = f
frq (Node f _ _ ) = f
 
 
freqs :: String -> [(Char, Int)]
freqs = M.toList . M.fromListWith (+) . map (flip (,) 1)


cn :: (Char, Int) -> HF
cn (x,i) = Leaf x i


tas::[(Char, Int)]->T.HP HF
tas = build.map(cn)
    where 
        build = foldr T.insert T.E



htree t | T.size t == 2 = Node ((frq min1) + (frq min2)) min1 min2
        | otherwise = htree $ T.insert noeud reste
            where 
            (min1,min2,reste) = T.deuxMin t
            noeud = Node ((frq min1) + (frq min2)) min1 min2




cListe (Leaf c _ )  = [(c,"")]
cListe (Node _ l r) = map (add '0') (cListe l) ++ map (add '1') (cListe r)
                    where 
                        add c (x,y) = (x,c:y)
             


codeHuffman = htree. tas . freqs 


cMap = M.fromList . cListe . codeHuffman 


encoder str = concat $ map (m M.!) str
            where m = cMap str


decoder t = dc t 
        where 
            dc (Leaf c _ ) [] = [c] 
            dc (Leaf c _ ) bs = c:dc t bs
            dc (Node _ l r) (b:bs) = dc (if b == '0' then l else r) bs        



-- Teste

chaineTest = "Bonjour" 
huffCodeTest = codeHuffman chaineTest --
chaineEncodee = encoder chaineTest  -- 100011101010100111

-- décodage réussi?
check | decoder huffCodeTest chaineEncodee == chaineTest = True
      | otherwise = False

      










