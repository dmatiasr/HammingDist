concatenation :: String -> String -> String
concatenation x (y:ys) = x++(drop (length(x)) (y:ys)) 

wordGen :: String -> String -> [String]
wordGen [] (y:ys) = [] 
wordGen (x:xs) (y:ys) = [concatenation (x:xs) (y:ys)] ++ (wordGen (init(x:xs)) (y:ys) )

aux :: [[Char]] -> String
aux (x:xs) = x

hammi :: String -> String -> [String]
hammi w1 w2 = h1++h2++h3++h4
			where 	h1 = wordGen w1 w2;
					h2 = wordGen w2 w1;
					h3 = wordGen (aux(drop (length(w1)-1) (h1))) (aux(drop (length(w2)-1) (h2)));
					h4 = wordGen (aux(drop (length(w2)-1) (h2))) (aux(drop (length(w1)-1) (h1)));
