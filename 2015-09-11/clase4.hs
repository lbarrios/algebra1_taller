potencia :: Float -> Integer -> Float
potencia a 0 = 1
potencia a n = a * potencia a (n-1)

sumaDeImparesCuyoCuadSeaMenorQue :: Integer -> Integer

sumaDeImparesCuyoCuadSeaMenorQue umbral = sumaAuxiliar umbral ...

-- Construimos una funcion que nos facilita todo el trabajo

sumaAuxiliar :: Integer -> Integer -> Integer
sumaAuxiliar umbral num
    | num ^ 2 >= umbral= 0
    | otherwise = num + sumaAuxiliar umbral (num + 2)
