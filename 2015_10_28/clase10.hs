data Polinomio = Mono Float Integer
		| Suma Polinomio Polinomio
		| Producto Polinomio Polinomio

sumaListas :: Num a => [a] -> [a] -> [a]
sumaListas [] l = l
sumaListas l [] = l
sumaListas (x:xs) (y:ys) = (x + y): (sumaListas xs ys)

porListas :: Num a => [a] -> [a] -> [a]
porListas [] l = []
porListas l [] = []
porListas [x] (l:ls) = (x*l):(porListas [x] ls)
porListas (x:xs) l = sumaListas (porListas [x] l) (0:(porListas xs l))

evaluar :: Polinomio -> Float -> Float
evaluar (Mono a n) z = a*(z^n)
evaluar (Suma p q) z = (evaluar p z) + (evaluar q z)
evaluar (Producto p q) z = (evaluar p z) * (evaluar q z)

coeficientes :: Polinomio -> [Float]
coeficientes p = limpiarCeros (coeficientes' p)

coeficientes' (Mono a 0) = [a]
coeficientes' (Mono a n) = 0:(coeficientes' (Mono a (n-1)))
coeficientes' (Suma p q) = sumaListas (coeficientes' p) (coeficientes' q)
coeficientes' (Producto p q) = porListas (coeficientes' p) (coeficientes' q)

sacarCeros :: [Float] -> [Float]
sacarCeros (0:xs) = sacarCeros xs
sacarCeros l = l

limpiarCeros :: [Float] -> [Float]
limpiarCeros l = reverse (sacarCeros (reverse l))

instance Num Polinomio where
	(+) p q = Suma p q
	(*) p q = Producto p q
	negate (Mono a b) = Mono (negate a) b
	negate (Suma a b) = Suma (negate a) (negate b)
	negate (Producto a b) = Producto (negate a) b
	fromInteger n = Mono (fromIntegral n) 0
	abs p = undefined
	signum p = undefined

instance Show Polinomio where
	show p = show (coeficientes p)

x=Mono 1 0
y=Mono 2 0
