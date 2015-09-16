pertenece :: Integer -> [Integer] -> Bool
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
