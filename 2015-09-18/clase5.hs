pertenece :: Integer -> [Integer] -> Bool
<<<<<<< HEAD
-- indica si un elemento aparece en la lista
pertenece elemento [] = False
pertenece elemento lista | head lista == elemento = True
	| otherwise = pertenece elemento (tail lista)

hayRepetidos :: [Integer] -> Bool
-- indica si una lista tiene repetidos
hayRepetidos [] = False
hayRepetidos lista | pertenece (head lista) (tail lista) = True
                   | otherwise = hayRepetidos (tail lista)

menores :: Integer -> [Integer] -> [Integer]
-- calcula los elementos de la lista que son menores al parametro
menores pivote [] = []
menores pivote lista | (head lista) < pivote = (head lista) : (menores pivote (tail lista))
                     | otherwise = menores pivote (tail lista)

quitar :: Integer -> [Integer] -> [Integer]
-- elimina la primera aparicion del elemento en la lista
quitar elem [] = []
quitar elem l | head l == elem = tail l
              | otherwise = head l : (quitar elem (tail l)) 


maximo :: [Integer] -> Integer
-- dada una lista no vacia, calcula el mayor elemento de la misma
maximo l | length l==1 || (head l) > maximo(tail l) = head l
         | otherwise = maximo(tail l)


enBase :: Integer -> Integer -> [Integer]
-- toma un numero y lo transforma a la base del segundo parametro, devuelve en forma de lista
enBase a b | a < b = [a]
           | otherwise = enBase (div a b) b ++ [mod a b]

deBase :: Integer -> [Integer] -> Integer
-- toma una base y un numero en esa base, y devuelve el numero en base 10
deBase base [] = 0
deBase base nlist = (last nlist) + base *(deBase base (init nlist))
=======
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


quitar :: Integer -> [Integer] -> [Integer]
-- elimina la primera aparicion del elemento en la lista (de haberla).
quitar elem lista | length lista==0 = []
	| elem == head lista = tail lista
	| otherwise = head lista : quitar elem (tail lista)

maximo :: [Integer] -> Integer
-- que dada una lista no vac ́ıa calcula el mayor elemento de la misma.
maximo lista | length lista == 1 = head lista
	| otherwise = max (head lista) (maximo (tail lista))


enBase :: Integer -> Integer -> [Integer]
-- toma un numero y lo transforma a la base pasada como segundo parametro
enBase numero base | numero<base = [numero]
	| otherwise = enBase (div numero base) base ++ [mod numero base]

deBase :: Integer -> [Integer] -> Integer
--toma una base y un numero y devuelve el numero en base 10
deBase base numeroLista | length numeroLista == 0 = 0
	| otherwise = last numeroLista + base*(deBase base (init numeroLista))

esCapicua :: [Integer] -> Bool
esCapicua lista | length lista <=1 = True
	| otherwise = head lista == last lista

--capicuaPara :: [Integer] -> [Integer]
--capicuaPara lista | esCapicua lista = lista
--	| otherwise = lista 


cambiaDeBase :: Integer -> Integer -> [Integer] -> [Integer]
cambiaDeBase baseDesde baseHacia numero = enBase (deBase baseDesde numero) baseHacia

listaDecreciente :: [Integer] -> Bool
listaDecreciente lista = length lista == 1 || ( (head lista) > head (tail lista) && listaDecreciente (tail lista) )

listaNoDecreciente :: [Integer] -> Bool
listaNoDecreciente lista = not (listaDecreciente lista)

quitarMenosElUltimo :: Integer -> [Integer] -> [Integer]
quitarMenosElUltimo num lista | pertenece num (quitar num lista) = quitarMenosElUltimo num (quitar num lista)
	| otherwise = lista
>>>>>>> f85e8b7a9961e0e5d1eddae707a8ed013f2a0c10
