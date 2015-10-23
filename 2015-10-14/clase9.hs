data Figura = Rectangulo Float Float Float Float
  | Circulo Float Float Float
  deriving Show

c1 :: Figura
c1 = Circulo 0 0 pi

r1 :: Float -> Figura
r1 n = Rectangulo 0 0 (cos(pi/4)*n) (sin(pi/4)*n)

area :: Figura -> Float
area (Rectangulo x1 y1 x2 y2) = (abs (x1-x2))*(abs (y1-y2))
area (Circulo x1 y1 radio) = pi*radio^2


data Punto = Punto Float Float
  deriving Show
data Figura2 = Rectangulo2 Punto Punto | Circulo2 Punto Float
  deriving Show
area2 :: Figura2 -> Float
area2 (Rectangulo2 (Punto x1 y1) (Punto x2 y2)) = (abs (x1-x2))*(abs (y1-y2))
area2 (Circulo2 (Punto x1 y1) radio) = pi*radio^2


------------

data ProgAritmetica = Vacio | CongruentesA Integer Integer

instance Show ProgAritmetica where
  show Vacio = "{}"
  show (CongruentesA x d) = "{a en Z | a = " ++ (show x) ++ " (mod " ++ (show d) ++ ")}"

esMultiplo :: Integer -> Integer -> Bool
esMultiplo n multiplo = mod n multiplo == 0

pertenece :: Integer ->  ProgAritmetica -> Bool
pertenece n Vacio = False
pertenece n (CongruentesA resto divisor) = esMultiplo (n-resto) divisor

incluido :: ProgAritmetica -> ProgAritmetica -> Bool
incluido Vacio b = True
incluido b Vacio = False
incluido (CongruentesA resto1 divisor1) (CongruentesA resto2 divisor2)
  | esMultiplo divisor1 divisor2 = (mod resto1 divisor2) == (mod resto2 divisor2)
  | otherwise = False

-- En este caso en que se devuelve un booleano,
-- es mejor usar directamente un AND, en vez de guardas
incluido2 (CongruentesA resto1 divisor1) (CongruentesA resto2 divisor2) = 
  esMultiplo divisor1 divisor2 && 
  ((mod resto1 divisor2) == (mod resto2 divisor2))

interseccion :: ProgAritmetica -> ProgAritmetica -> ProgAritmetica

