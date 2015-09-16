import Data.Char

restaPuntos a b = (fst b-fst a, snd b-snd a)
restaPuntos2 (a,b) (c,d) = (c-a, d-b)

pendienteVector a = 

pendiente a b = pendienteVector restaPuntos a b
