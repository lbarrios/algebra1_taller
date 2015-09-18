pertenece :: Integer -> [Integer] -> Bool
-- indica si un elemento aparece en la lista.
pertenece elem lista | length lista == 0 = False
	| elem==head lista = True
	| otherwise = pertenece elem (tail lista)

hayRepetidos :: [Integer] -> Bool
-- indica si una lista tiene elementos repetidos.
hayRepetidos lista | length lista == 0 = False
	| pertenece (head lista) (tail lista) = True
	| otherwise = hayRepetidos (tail lista)

menores :: Integer -> [Integer] -> [Integer]
-- calcula los elementos de la lista que son menores al primer parametro.
menores pivote lista | length lista == 0 = []
	| head lista<pivote = head lista : menores pivote (tail lista)
	| otherwise = menores pivote (tail lista)


--quitar :: Integer -> [Integer] -> [Integer]
-- elimina la primera aparicion del elemento en la lista (de haberla).
--maximo :: [Integer] -> Integer
-- que dada una lista no vac ́ıa calcula el mayor elemento de la misma.

