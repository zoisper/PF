import System.Random
import Data.Char



elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices n lista | n == (last lista) = (elemIndices n (init lista)) ++ [(length lista) -1]
                    | otherwise = elemIndices n (init lista) 

myisSubsequenceOf :: Eq a => [a] -> [a] -> Bool 
myisSubsequenceOf [] lista = True
myisSubsequenceOf lista [] = False
myisSubsequenceOf (x:xs) (y:ys) | x == y = myisSubsequenceOf xs ys
                                | otherwise = myisSubsequenceOf (x:xs) ys



data BTree a = Empty | Node a (BTree a) (BTree a)
 deriving Show

mylookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
mylookupAP _ Empty = Nothing
mylookupAP x (Node (a,b) e d) | x == a = Just b
                              | x > a = mylookupAP x d
                              | otherwise = mylookupAP x e


myzipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
myzipWithBT _ Empty _ = Empty
myzipWithBT _ _ Empty = Empty
myzipWithBT f (Node a e d) (Node b l r) = Node (f a b ) (myzipWithBT f e l) (myzipWithBT f d r )  

bt1 :: Num a => BTree a
bt1 = (Node 1 Empty (Node 2 Empty Empty))
bt2 :: Num a => BTree a
bt2 = (Node 3 Empty (Node 6 (Empty) (Node 1 Empty Empty)))

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (x:xs) | isDigit x = (x:a, b)
                  | isAlpha x = (a, x:b)
                  | otherwise = (a,b)
                     where (a,b) = digitAlpha xs


data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)
  
myfirstSeq :: Seq a -> a
myfirstSeq (Cons a x ) = a
myfirstSeq (App Nil x ) = myfirstSeq x
myfirstSeq (App x Nil) = myfirstSeq x 
myfirstSeq (App a b) | (seqvazia a) = myfirstSeq b
                     | otherwise = myfirstSeq a 


seqvazia :: Seq a -> Bool
seqvazia Nil = True
seqvazia (Cons a _) = False
seqvazia (App a b) = (seqvazia a) && (seqvazia b)

dropSeq :: Int -> Seq a -> Seq a
dropSeq 0 x = x
dropSeq _ Nil = Nil
dropSeq n (Cons a x) = dropSeq (n-1) x
dropSeq n (App a b) | (contaseq a) < n = dropSeq (n-(contaseq a)) b
                    | (contaseq a) == n = b
                    | otherwise = App (dropSeq n a ) b  



contaseq :: Seq a -> Int
contaseq Nil = 0
contaseq  (Cons a b) = 1 + (contaseq b)
contaseq (App a b) = (contaseq a) + (contaseq b)


seqteste :: Num a => Seq a
seqteste = (App (App (Cons 7 (Cons 5 Nil)) (Cons 3 Nil)) (Cons 1 Nil))

instance Show a => Show (Seq a) where
    show a = "<<" ++ mostra a ++ ">>"
    

mostra :: Show a => Seq a -> String
mostra Nil = ""
mostra (Cons a Nil) = show a
mostra (Cons a b) = (show a) ++ "," ++  (mostra b)  
mostra (App a b) = (mostra a) ++ "," ++ (mostra b)  

type Mat a = [[a]]

getElem :: Mat a -> IO a
getElem m = do 
           l <- randomRIO (1,length (head m))
           c <- randomRIO (1, (length m))
           let n = (!!) ((!!) m (c-1)) (l-1)
           return n



mymagic :: Mat Int -> Bool
mymagic [] = True
mymagic [x] = True
mymagic m = todosiguais teste
    where teste = (somalinhas m) ++ (somacolunas m) ++ [somadiagonaldir m] ++ [somadiagonalesq m]  


somalinhas :: Mat Int -> [Int]
somalinhas m = map sum m

somacolunas :: Mat Int -> [Int]
somacolunas ((x:[]):xs) = [(sum (map head ((x:[]):xs)))]
somacolunas  m = [(sum (map head m))] ++ (somacolunas (map tail m))

somadiagonalesq :: Mat Int -> Int
somadiagonalesq [] = 0
somadiagonalesq (x:xs) = (head x) + (somadiagonalesq (map tail xs))  

somadiagonaldir :: Mat Int -> Int
somadiagonaldir [] = 0
somadiagonaldir (x:xs) = (last x) + (somadiagonaldir (map init xs))

todosiguais :: [Int] -> Bool
todosiguais [] = True
todosiguais [x] = True
todosiguais (x:y:ys) | x == y = todosiguais (y:ys)
                     | otherwise = False 