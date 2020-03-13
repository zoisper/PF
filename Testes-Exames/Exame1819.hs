import Data.List
import System.Random
import Data.Char
import Data.Maybe

myisSorted :: (Ord a) => [a] -> Bool 
myisSorted [] = True
myisSorted [x] = True
myisSorted (x:y:ys) | x <= y = myisSorted (y:ys)
                    | otherwise = False

myinits :: [a] -> [[a]] 
myinits [] = [[]]
myinits lista = (myinits (init lista)) ++ [lista]


maximumMB :: (Ord a) => [Maybe a] -> Maybe a
maximumMB = foldr (\a b -> if a `maiorque` b then a else b) Nothing
   where maiorque (Just a) (Just b) = a >b
         maiorque Nothing _ = False
         maiorque  _ Nothing = True  


data LTree a = Tip a | Fork (LTree a) (LTree a)
 

listaLT :: LTree a -> [a] 
listaLT (Tip a) = [a]
listaLT (Fork a b) = (listaLT a) ++ (listaLT b)

instance Show a =>  Show (LTree a) where
  show (Tip a) = (show a)
  show (Fork a b) = (mostra 1 a) ++ (mostra 1 b)


mostra :: Show a => Int -> LTree a -> String
mostra n (Tip a) = (replicate n '.') ++ (show a) ++ "\n"
mostra n (Fork a b) = (mostra (n+1) a) ++ (mostra (n+1) b) 



maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits l]

mymaxSumInit :: (Num a, Ord a) => [a] -> a
mymaxSumInit l = foldl ( \acc i -> max acc (sum i)) (head l) (inits l)

convPF :: (Eq a) => RelP a -> RelF a
convPF x = ((map fst y),f)
    where y = convPL x
          f a = foldl (\acc (b,c) -> if a == b then c else acc) [] y

type RelP a = [(a,a)]
type RelL a = [(a,[a])]
type RelF a = ([a], a->[a])

  


convLP :: RelL a -> RelP a
convLP l = concat (map junta l)
 where junta (x,xs) = map (\y->(x,y)) xs


convPL :: (Eq a) => RelP a -> RelL a
convPL [(a,b)] = [(a,[b])]
convPL (h:t) = insere h (convPL t)
   where insere (a,b) lista | elem a (map fst lista) = map (\(c,d) -> if a == c then (c,b:d) else (c,d)) lista
                            | otherwise = [(a,[b])] ++ lista 


relpteste :: RelP Int
relpteste = [(1,3),(1,4),(2,1),(2,4),(2,5),(3,7),(4,7),(5,7),(6,5),(7,6)]



mcriaRelPint :: Int -> IO (RelP Int)
mcriaRelPint 0 = return []
mcriaRelPint n = do
    putStr "Introduz dois numeros (separados por um espaco): "
    (num1,num2) <- fmap (span (/= ' ')) getLine
    fmap ((read num1,read num2) :) $ mcriaRelPint (n - 1)


convFP :: (Eq a) => RelF a -> RelP a
convFP (a,b) = convLP (map (\y -> (y ,b y )) a)

--ou

convFP2 :: (Eq a) => RelF a -> RelP a
convFP2 ([],a) = []
convFP2 (a,b) = convLP (converte (a,b))

converte :: (Eq a) => RelF a -> RelL a
converte ([],a) = []
converte ((x:xs),b) = (x, b x):(converte (xs, b)) 

