sortBySelec[] = [];
sortBySelec xs = x:sortBySelec(remove x xs)
                where x = minimum xs
                      remove _ [] = [];
                      remove el (x:xs) | x == el = xs
                                       | otherwise = x:remove el xs;
                
                
