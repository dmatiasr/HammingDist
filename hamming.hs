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




--PERMUTA UNA LISTA DE STRING BASADO EN UN STRING
permutaStrings :: String->[String]->[String]
permutaStrings a [] = []
permutaStrings a (x:xs)= hammi a x ++ permutaStrings a xs

--PERMUTA CADA PALABRA DE LA LISTA CON EL RESTO DE ELLA
permutaAll :: [String]->[String]
permutaAll [] = []
permutaAll (x:xs)= permutaStrings x xs ++ permutaAll xs

--LIMPIAR REPETIDOS DE PERMUTA ALL
--COMPARAR CADA PALABRA CON TODAS LAS PERMUTACIONES DE TODAS LAS PALABRAS.
-- Y SACAR LAS DISTANCIAS
--VER CASOS BASE

--COMPARAR UNA PERMUTACIION CON TODAS LAS PALABRAS. LUEGO PROCEDER CON LA SEGUNDA PERMUTACION
--Y COMPARAR CON TODAS LAS PALABRAS
				--PALABRAS XS --PERMUTACIONES XS --
compareStrings :: [String]->[String]->[[Int]]
compareStrings [] [] = [[]] 
compareStrings xs [] = [[]]
compareStrings [] ys = [[]]
compareStrings (x:xs)(y:ys)= [ distHamming (x:xs) y ]++ compareStrings (x:xs) ys
							--palabras con 1er permut

--ARMA UNA LISTA CON TODAS LAS DISTANCIAS ENTRE CADA PALABRA Y LAS PERMUTACIONES DE TODAS
calcAllDistance :: [String]->[[Int]]
calcAllDistance [] = [[0]]
calcAllDistance (x:xs)= compareStrings (x:xs) (rm (permutaAll (x:xs)) ) 


--COMPARA UN K Y UNA LISTA DE DISTANCIA DE UNA PERMUTACION. 
--ALGUN INT MENOR QUE EL K

compareK :: Int->[Int]-> Bool
compareK  k [] = True
compareK  k (x:xs)| k == 0 = error "K=0: No se puede resolver" 
				  | k >= x = True && compareK k xs
				  | otherwise = False && compareK k xs


--COMPARA EL K CON TODAS LAS LISTAS DE DISTANCIAS DE CADA PERMUTACION
compareKAllPerms:: Int ->[[Int]]->Bool
compareKAllPerms k [[]] = False
compareKAllPerms k (xs:xss) = compareK k xs || compareKAllPerms k xss

--VERIFICAR QUE TODAS LAS LISTAS TIENEN EL MISMO TAMAÑO
--PARA UNA LISTA VERIFICAR CON EL RESTO (SOlO ES NECESARIO COMPARAR UNA CON EL RESTO)
lengthStrings ::String->[String]->Bool
lengthStrings as [] = True
lengthStrings as (x:xs)| length as== length x = True && lengthStrings as xs
					   | otherwise = False

--HammingExistMain DADO UN K, ANALIZA SI EXISTE AL MENOS UNA PERMUTACION, QUE SEA MENOR O IGUAL
--EN DISTANCIA HAMMING A K,
--QUE RESULTE DE PERMUTAR CADA DIGITO DE CADA PALABRA CON LAS DEMAS.
hammingExistMain :: Int->[String]->Bool
hammingExistMain k (x:xs) | lengthStrings x xs == False = error "Las cadenas tienen diferente tamaño" 
						  | lengthStrings x xs == True = compareKAllPerms k ( calcAllDistance (x:xs))

