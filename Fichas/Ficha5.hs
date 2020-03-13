


myzipwith :: (a->b->c) -> [a] -> [b] -> [c]
myzipwith p l [] = []
myzipwith p  [] l = []
myzipwith p (h1:t1) (h2:t2) = ((p h1 h2):myzipwith p t1 t2)


type Mat a = [[a]]


mydimOK :: Mat a -> Bool
mydimOK [] = True
mydimOK (x:xs) | length x >0 = mydimOKaux (length x) xs
               | otherwise = False 
 where mydimOKaux :: Int ->  Mat a -> Bool
       mydimOKaux n [] = True
       mydimOKaux n (x:xs) | length x == n = mydimOKaux n xs
                           | otherwise = False

dimMat :: Mat a -> (Int,Int) 
dimMat [] = (0,0)
dimMat (x:xs) | mydimOK (x:xs) = (length x, length (x:xs))
              | otherwise = (0,0)
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat a [] = a
addMat [] b = b
addMat (x:xs) (y:ys) = [(somma x y)] ++ (addMat xs ys) 
 where somma :: Num a => [a] -> [a] -> [a]
       somma a [] = a
       somma [] a =  a
       somma (a:as) (b:bs) = ((a + b):(somma as bs))

multMat :: Num a => Mat a -> Mat a -> Mat a
multMat [] _ = []
multMat _ [] = []
multMat (x:xs) m = [multlinha x m] ++ (multMat xs m)   
 where multlinha :: Num a => [a] -> Mat a -> [a]
       multlinha a [] = []
       multlinha a ((b:[]):bs) = [sum (myzipwith (*) a bh)]
        where bh = (map head ((b:[]):bs))
       multlinha a m2 = [sum (myzipwith (*) a h)] ++ multlinha a t 
        where h = (map head m2)
              t = (map tail m2)
myzipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
myzipWMat p [] _ = [] 
myzipWMat p _ [] = []
myzipWMat p (x:xs) (y:ys) = [(myzipwith p x y)] ++ (myzipWMat p xs ys)  

mytranspose :: Mat a -> Mat a 
mytranspose [] = []
mytranspose ((x:[]):xs) = [map (head)  ((x:[]):xs)]
mytranspose l = [(map (head) l )] ++ mytranspose (map (tail) l )



mytriSup :: (Eq a, Num a) => Mat a -> Bool 
mytriSup (x:xs) = triangular 1 xs

triangular :: (Eq a, Num a) => Int -> Mat a -> Bool
triangular _ [] = True
triangular n (x:xs) | testalinha n x = triangular (n+1) xs
                    | otherwise = False

testalinha :: (Eq a, Num a) => Int -> [a] -> Bool
testalinha 0 _ = True
testalinha _ [] =  True
testalinha n (x:xs) | (x == 0) = testalinha (n-1) xs
                    | otherwise = False

