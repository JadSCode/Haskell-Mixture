import Data.Char

words' s          =  case dropWhile isSpace s of
                      "" -> []
                      s' -> w : words s''
                            where (w, s'') = break isSpace s';

quicksort []     = [];
quicksort (x:xs) = quicksort (filter (<x) xs ) ++ [x] ++ quicksort (filter (>=x) xs );


sortO []     = [];
sortO (x:xs) = sortO (filter (\(_,a) -> a >= snd(x) ) xs ) ++ [x] ++ sortO (filter (\(_,a)-> a < snd(x) ) xs );


countw [] = [];
countw (x:xs) = (x,length(takeWhile(==x) (x:xs))): countw (dropWhile (==x) (xs));


insertSpace n = [' ' | x<-[1..n]]; 


show' (a,b) =  "| " ++ a ++ insertSpace (24 - length a) ++ "|   " ++ show b ++ insertSpace (23 - length (show b)) ++ "|\n";

mpf n s =   concat . map show' . take n . sortO . countw . quicksort . words' $ map toLower s;

main = do 
    putStrLn "-----------------------------------------------------"
    putStrLn "Donner le nom du fichier contenant le texte à traiter"
    putStrLn "-----------------------------------------------------"
    fichier  <- getLine
    contents <- readFile fichier
    putStrLn "-----------------------------------------------------"
    putStrLn "Donner le nombre de mots à afficher "
    putStrLn "-----------------------------------------------------"
    n <- getLine
    let x =  read n :: Int
    let {lesMots =  "------------------------------------------------------\n"
                 ++ "| Le mot                  | Nombre d'occurences      | \n"
                 ++ "------------------------------------------------------\n"
                 ++ mpf x contents 
                 ++ "------------------------------------------------------\n"
        }
    putStrLn lesMots
    writeFile "output.txt" lesMots

    
