data Arbol t = Hoja t | Ramificacion (Arbol t) t (Arbol t)

esHoja (Hoja i) = True
esHoja (Ramificacion a b c) = False

sumaNodos (Hoja i) = i
sumaNodos (Ramificacion a b c) = b + (sumaNodos a) + (sumaNodos c)

altura :: Arbol Integer -> Integer
altura (Hoja i) = 1
altura (Ramificacion a b c) = 1  + (max (altura a) (altura c))

pertenece :: Integer -> Arbol Integer -> Bool
pertenece i (Hoja j) = i==j
pertenece i (Ramificacion a b c) = i==b || pertenece i a || pertenece i c


