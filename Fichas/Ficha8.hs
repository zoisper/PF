data Frac = F Integer Integer
 

mdc :: Integer -> Integer -> Integer
mdc 0 n = n
mdc n 0 = n
mdc n d | n >= d = mdc (n-d) d
        | otherwise = mdc n (d-n)

normaliza :: Frac -> Frac
normaliza (F 0 d) = F 0 0 
normaliza (F n d) | n*d < 0 = F (-(div (abs n) m )) (div (abs d) m) 
                  | otherwise = F (div (abs n) m ) (div (abs d) m)
                    where m = mdc (abs n) (abs d) 

instance Eq Frac where
 f1 == f2 = n1 == n2 && d1 == d2
  where F n1 d1 = normaliza f1 
        F n2 d2 = normaliza f2 


instance Ord Frac where
  compare f1 f2 = compare (n1 * d2) (n2 * d1)
   where F n1 d1 = normaliza f1
         F n2 d2 = normaliza f2

instance Show Frac where
  show (F n d) = (show n) ++ "/" ++ (show d) 

instance Num Frac where
 (F n1 d1) + (F n2 d2) = F ((n1*d2) + (n2*d1)) (n1 * d2)
 (F n1 d1) - (F n2 d2) = F ((n1*d2) - (n2*d1)) (n1 * d2)
 (F n1 d1) * (F n2 d2) = F (n1*n2) (d1*d2)
 negate (F n1 d1) = F  (- n1) d1
 abs (F n1 d1) = F (abs n1) (abs d1)
 signum f1 = F (signum n1) 1 
   where F n1 d1 = normaliza f1
 fromInteger i = F (fromInteger i) 1

twiceBiggerThen :: Frac -> [Frac] -> [Frac]
twiceBiggerThen _ [] = []
twiceBiggerThen n (x:xs) | (x*x) >= n = (x:(twiceBiggerThen n xs))
                         | otherwise = twiceBiggerThen n xs

data Exp a = Const a 
 | Simetrico (Exp a) 
 | Mais (Exp a) (Exp a) 
 | Menos (Exp a) (Exp a) 
 | Mult (Exp a) (Exp a)

calcula :: Num a => Exp a -> a
calcula (Const a) = a
calcula (Simetrico a) = - (calcula a)
calcula (Mais a b) =  (calcula a) + (calcula b)
calcula (Menos a b) = (calcula a) - (calcula b)
calcula (Mult a b) = (calcula a) * (calcula b) 



instance Show a => Show (Exp a) where
  show (Const a) = show a
  show (Simetrico a) = "-" ++ show a
  show (Mais a b) = show a ++ "+" ++ show b
  show (Menos a b) = show a ++ "-" ++ show b
  show (Mult a b) = show a ++ "*" ++ show b

instance (Num a, Eq a) => Eq (Exp a) where
 a == b = (calcula a) == (calcula b) 
  
instance (Num a, Eq a) => Num (Exp a) where
  a + b = Const ((calcula a) + (calcula b))
  a - b = Const ((calcula a) - (calcula b))
  a * b = Const ((calcula a) * (calcula b))
  negate (Const a) = Const (- a)
  negate (Simetrico a) = a
  negate (Mais a b) = Mais (- a) (- b) 
  negate (Menos a b) = Menos b a
  negate (Mult a b) = Mult (-a) (b)
  abs x = Const (abs (calcula x))
  signum x = Const (signum (calcula x))
  fromInteger a = Const (fromInteger a)


l = [x | x <- [1..100]]