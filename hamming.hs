--CONCATENA 2 PALABRAS DECREMENTANDO UN INDICE
concatenation :: String -> String -> String
concatenation x (y:ys) = x++(drop (length(x)) (y:ys)) 

--GENERA PERMUTACIONES POR INDICE ENTRE DOS PALABRAS
wordGen :: String -> String -> [String]
wordGen [] (y:ys) = [] 
wordGen (x:xs) (y:ys) = [concatenation (x:xs) (y:ys)] ++ (wordGen (init(x:xs)) (y:ys) )

--GENERA PERMUTACIONES POR INDICE DESDE UNA LISTA DE STRINGS Y UN STRINGS 
hammiList::[String]->String->[String]
hammiList [] s = []
hammiList (x:xs) s = wordGen x s ++ hammiList xs s

--RETORNA TODAS LAS PERMUTACIONES POR INDICE ENTRE DOS PALABRAS
hammi :: String -> String -> [String]
hammi [] [] = []
hammi w1 w2 = rm(h1++h2++hammiList h1 w1++hammiList h2 w2)
			where 	h1 = wordGen w1 w2;
					h2 = wordGen w2 w1;

--ELIMINA REPETIDOS DE UNA LISTA
rm::[String]->[String]
rm [] = []
rm (x:xs) | any (==x) (xs) = rm xs
		  | otherwise = [x] ++ rm xs

--RETORNA DISTANCIA DE HAMMING ENTRE DOS PALABRAS
wordDistHamming::String->String->Int
wordDistHamming [] [] = 0
wordDistHamming (x:xs) (y:ys) | x==y = 0+wordDistHamming xs ys
							  |	otherwise = 1+wordDistHamming xs ys

--RETORNA UNA LISTA CON TODAS LAS DISTANCIAS DE HAMMING DE UNA LISTA DE STRINGS Y UNA STRING
distHamming::[String]->String->[Int]
distHamming [] s = []
distHamming (x:xs) s = [wordDistHamming x s]++distHamming xs s


--UNA LISTA DE STRING Y CALCULAR SUS PERMUTACIONES POR INDICE PASANDO UN STRING
permutaStrings :: String->[String]->[String]
permutaStrings a [] = []
permutaStrings a (x:xs)= hammi a x ++ permutaStrings a xs

--PERMUTA CADA PALABRA DE LA LISTA CON EL RESTO DE ELLA
permutaAll :: [String]->[String]
permutaAll [] = []
permutaAll (x:xs)= permutaStrings x xs ++ permutaAll xs

--LIMPIAR REPETIDOS DE PERMUTA ALL

