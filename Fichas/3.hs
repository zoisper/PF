
data Hora = H Int Int
 deriving Show

type Etapa = (Hora,Hora)

type Viagem = [Etapa]


testahora :: Hora -> Bool
testahora (H a b) = (a >= 0 && a <= 24 && b >= 0 && b <= 60)

testaetapa :: Etapa -> Bool
testaetapa (H a b, H c d) 
                      | a < c = testahora (H a b) && testahora (H c d)
                      | a == c && b <= d = testahora (H a b) && testahora (H c d)
                      | otherwise = False


testaviagem :: Viagem -> Bool
testaviagem [] = True
testaviagem [x] = testaetapa x
testaviagem ((H a b, H c d):(H e f, H g h):xs)
                            | testaetapa (H a b, H c d) && testaetapa (H c d, H e f) = testaviagem ((H e f, H g h ):xs)
                            | otherwise = False


partidachegada :: Viagem -> (Hora,Hora)
partidachegada l | testaviagem l = partidachegadaaux l

partidachegadaaux :: Viagem -> (Hora,Hora)
partidachegadaaux [(H a b, H c d)] = (H a b, H c d)
partidachegadaaux ((H a b, H c d):(H e f, H g h):[]) = (H a b, H g h)   
partidachegadaaux (a:b:c:xs) = partidachegadaaux (a:c:xs)

tempoviagem :: Viagem -> Int
tempoviagem [] = 0
tempoviagem ((a,b):vs) = (difhoras a b) + (tempoviagem vs)

difhoras :: Hora -> Hora -> Int
difhoras (H a b) (H c d) | testahora (H a b ) && testahora (H c d) = (c*60) - (a*60) + d - b    

tempoespera :: Viagem -> Int
tempoespera [] = 0
tempoespera [x] = 0
tempoespera ((a,b):(c,d):vs) = (difhoras b c) + tempoespera ((c,d):vs)

tempototal :: Viagem -> Int
tempototal l = difhoras a b 
        where (a,b) = partidachegada l