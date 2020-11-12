
--funçoes auxiliares

groupaux :: Eq a => a -> [a] -> [a] --forma uma lista com todos os elementos iguais a n
groupaux _ [] = []
groupaux n (x:xs) = if n == x then [x] ++ groupaux n xs 
                    else groupaux n xs

removedogrupo :: Eq a => a -> [a] -> [a] --remove todos elementos da lista iguais a n
removedogrupo _ [] = []
removedogrupo n (x:xs) = if n == x then removedogrupo n xs
                        else [x] ++ removedogrupo n xs 



{- 1. Apresente uma definiçãoao recursiva da função (pré-definida) enumFromTo :: Int -> Int ->
[Int] que constrói a lista dos númmeros inteiros compreendidos entre dois limites.
Por exemplo, enumFromTo 1 5 corresponde à lista [1,2,3,4,5]. -}

myenumFromTo :: Int -> Int -> [Int]
myenumFromTo a b 
             | a <= b = a:(myenumFromTo (a+1) b)
             | otherwise = []


{- 2. Apresente uma definiçãoao recursiva da funçãoao (pré-definida) enumFromThenTo :: Int -> Int
-> Int -> [Int] que constrói a lista dos núumeros inteiros compreendidos entre dois limites
e espaçaados de um valor constante.
Por exemplo, enumFromThenTo 1 3 10 corresponde `a lista [1,3,5,7,9]. -}

myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo a b c
             | a <= c && a < b = a:(myenumFromThenTo b (2*b-a) c)
             | otherwise = []
              

{- 3. Apresente uma definição recursiva da função (pré-definida) (++) :: [a] -> [a] -> [a]
que concatena duas listas.
Por exemplo, (++) [1,2,3] [10,20,30] corresponde `a lista [1,2,3,10,20,30]. -}

concatena :: [a] -> [a] -> [a]
concatena [] a = a
concatena (x:xs) y = x:(concatena xs y)

  
{- 4. Apresente uma definição recursiva da função (pré-definida) (!!) :: [a] -> Int -> a que
dada uma lista e um inteiro, calcula o elemento da lista que se encontra nessa posição (assumese que o primeiro elemento se encontra na posição 0).
Por exemplo, (!!) [10,20,30] 1 corresponde a 20.
Ignore os casos em que a função não se encontra definida (i.e., em que a posição fornecida não
corresponde a nenhuma posição válida da lista). -}

elemlista :: [a] -> Int -> a 
elemlista  (x:xs) 0 = x
elemlista (x:xs) n  = elemlista xs (n-1)   


{- 5. Apresente uma definição recursiva da função (pré-definida) reverse :: [a] -> [a] que
dada uma lista calcula uma lista com os elementos dessa lista pela ordem inversa.
Por exemplo, reverse [10,20,30] corresponde a [30,20,10]. -}

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = (myreverse xs) ++ [x]

--ou (versão otimizada)

myreverse2 :: [a] -> [a]
myreverse2 x = reverse_aux [] x


reverse_aux :: [a] -> [a] -> [a]
reverse_aux x [] = x
reverse_aux x (y:ys) = reverse_aux (y:x) ys


{- 6. Apresente uma definição recursiva da função (pré-definida) take :: Int -> [a] -> [a] que
dado um inteiro n e uma lista l calcula a lista com os (no máximo) n primeiros elementos de
l.
A lista resultado sá terá menos de que n elementos se a lista l tiver menos do que n elementos.
Nesse caso a lista calculada é igual à lista fornecida.
Por exemplo, take 2 [10,20,30] corresponde a [10,20]. -}

mytake :: Int -> [a] -> [a]
mytake n [] = []
mytake n (x:xs) 
                | n > 0 =  x:(mytake (n-1) xs)
                | otherwise = []


{- 7. Apresente uma definição recursiva da função (pré-definida) drop :: Int -> [a] -> [a] que
dado um inteiro n e uma lista l calcula a lista sem os (no máximo) n primeiros elementos de
l.
Se a lista fornecida tiver n elementos ou menos, a lista resultante será vazia.
Por exemplo, drop 2 [10,20,30] corresponde a [30]. -}

mydrop :: Int -> [a] -> [a]
mydrop _ [] =  []
mydrop n (x:xs)
         | n > 0 = mydrop (n-1) xs
         | otherwise = (x:xs)


{- 8. Apresente uma definição recursiva da função (pré-definida) zip :: [a] -> [b] -> [(a,b)]
const´oi uma lista de pares a partir de duas listas.
Por exemplo, zip [1,2,3] [10,20,30,40] corresponde a [(1,10),(2,20),(3,30)]. -}

myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (h1:t1) (h2:t2) = (h1,h2):(myzip t1 t2 )


{- 9. Apresente uma definição recursiva da função (pré-definida) elem :: Eq a => a -> [a] ->
Bool que testa se um elemento ocorre numa lista.
Por exemplo, elem 20 [10,20,30] corresponde a True enquanto que elem 2 [10,20,30]
corresponde a False. -}

myelem :: Eq a => a -> [a] -> Bool
myelem a [] = False
myelem a (x:xs) 
            | a == x = True
            | otherwise = myelem a xs


{- 10. Apresente uma definição recursiva da função (pré-definida) replicate :: Int -> a ->
[a] que dado um inteiro n e um elemento x constói uma lista com n elementos, todos iguais
a x.
Por exemplo, replicate 3 10 corresponde a [10,10,10]. -}

myreplicate :: Int -> a -> [a]
myreplicate n p 
            | n > 0 = p:(myreplicate (n-1) p)
            | otherwise = []


{- 11. Apresente uma defini¸c~ao recursiva da função (pré-definida) intersperse :: a -> [a] ->
[a] que dado um elemento e uma lista, constrói uma lista em que o elemento fornecido é
intercalado entre os elementos da lista fornecida.
Por exemplo, intersperce 1 [10,20,30] corresponde a [10,1,20,1,30]. -}


myintersperse :: a -> [a] -> [a]
myintersperse a [] = []
myintersperse a [x] = [x]
myintersperse a (x:xs) = x:a:(myintersperse a xs)

{- 12. Apresente uma definição recursiva da função (pré-definida) group :: Eq a => [a] -> [[a]] que
agrupa elementos iguais e consecutivos de uma lista.
Por exemplo, group [1,2,2,3,4,4,4,5,4] corresponde a [[1],[2,2],[3],[4,4,4],[5],[4]]. -}

mygroup :: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup [a] = [[a]]
mygroup (x:xs)
         | x == (head xs) = [[x] ++ head (mygroup xs)] ++ tail (mygroup (xs))
         | otherwise = [[x]] ++ (mygroup xs)

         --ou

mygroup2 :: Eq a => [a] -> [[a]]
mygroup2 [] = []
mygroup2 [x] = [[x]]
mygroup2 (x:xs)
             | x == (head xs) = (x:head (mygroup2 xs)):tail(mygroup2 xs)
             | otherwise = [x]:(mygroup2 xs)


{- 13. Apresente uma definição recursiva da função (pré-definida) concat :: [[a]] -> [a] que
concatena as listas de uma lista.
Por exemplo, concat [[1],[2,2],[3],[4,4,4],[5],[4]] corresponde a [1,2,2,3,4,4,4,5,4]. -}

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ (myconcat xs) 


{- 14. Apresente uma definição recursiva da função (pré-definida) inits :: [a] -> [[a]] que
calcula a lista dos prefixos de uma lista.
Por exemplo, inits [11,21,13] corresponde a [[],[11],[11,21],[11,21,13]]. -}

myinits :: [a] -> [[a]]
myinits [] = [[]]
myinits l = myinits (init l) ++ [l] 


{- 15. Apresente uma definição recursiva da função (pré-definida) tails :: [a] -> [[a]] que
calcula a lista dos sufixos de uma lista.
Por exemplo, tails [1,2,3] corresponde a [[1,2,3],[2,3],[3],[]]. -}

mytails :: [a] -> [[a]]
mytails [] = [[]]
mytails l = [l] ++ (mytails (tail l))


{- 16. Apresente uma definição recursiva da funçãao (pré-definida) isPrefixOf :: Eq a => [a]
-> [a] -> Bool que testa se uma lista é prefixo de outra.
Por exemplo, isPrefixOf [10,20] [10,20,30] corresponde a True enquanto que isPrefixOf
[10,30] [10,20,30] corresponde a False. -}

myisPrefixOf :: Eq a => [a] -> [a] -> Bool
myisPrefixOf [] l = True
myisPrefixOf l [] = False
myisPrefixOf (x:xs) (y:ys) 
                | x == y = myisPrefixOf xs ys
                | otherwise = False


{- 17. Apresente uma definição recursiva da função (pré-definida) isSuffixOf :: Eq a => [a]
-> [a] -> Bool que testa se uma lista ´e sufixo de outra.
Por exemplo, isSuffixOf [20,30] [10,20,30] corresponde a True enquanto que isSuffixOf
[10,30] [10,20,30] corresponde a False. -}

myisSuffixOf :: Eq a => [a] -> [a] -> Bool
myisSuffixOf [] l = True
myisSuffixOf l [] = False
myisSuffixOf l1 l2
                | last l1 == last l2 = myisSuffixOf (init l1) (init l2)
                | otherwise = False


{- 18. Apresente uma definição recursiva da função (pré-definida) isSubsequenceOf :: Eq a =>
[a] -> [a] -> Bool que testa se os elementos de uma lista ocorrem noutra pela mesma
ordem relativa.
Por exemplo, isSubsequenceOf [20,40] [10,20,30,40] corresponde a True enquanto que
isSubsequenceOf [40,20] [10,20,30,40] corresponde a False. -}

myisSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myisSubsequenceOf [] l = True
myisSubsequenceOf l [] = False
myisSubsequenceOf (x:xs) (y:ys)
                    | x == y = myisSubsequenceOf xs ys
                    | otherwise = myisSubsequenceOf (x:xs) ys 

{- 19. Apresente uma definição recursiva da função (pré-definida) elemIndices :: Eq a => a ->
[a] -> [Int] que calcula a lista de posições em que um dado elemento ocorre numa lista.
Por exemplo, elemIndices 3 [1,2,3,4,3,2,3,4,5] corresponde a [2,4,6]. -}

myelemIndices :: Eq a => a -> [a] -> [Int]
myelemIndices a [] = []
myelemIndices a l 
             | a == last l = myelemIndices a (init l) ++ [(length l)-1]
             | otherwise = myelemIndices a (init l)

{- 20. Apresente uma definição recursiva da função (pré-definida) nub :: Eq a => [a] -> [a] que
calcula uma lista com os mesmos elementos da recebida, sem repetições.
Por exemplo, nub [1,2,1,2,3,1,2] corresponde a [1,2,3].

-} 

mynub :: Eq a => [a] -> [a] 
mynub [] = []
mynub l
      | elem (last l) (init l) == True = mynub (init l)
      | otherwise = mynub (init l) ++ [last l]


{- 21. Apresente uma definição recursiva da função (pré-definida) delete :: Eq a => a -> [a]
-> [a] que retorna a lista resultante de remover (a primeira ocorrência de) um dado elemento
de uma lista.
Por exemplo, delete 2 [1,2,1,2,3,1,2] corresponde a [1,1,2,3,1,2]. Se não existir nenhuma ocorr^encia a fun¸c~ao dever´a retornar a lista recebida. -}

mydelete :: Eq a => a -> [a] -> [a]
mydelete a [] = []
mydelete a (x:xs)
             | a == x = xs
             | otherwise = x:(mydelete a xs)


{- 22. Apresente uma definiçãoao recursiva da funçãoao (pré-definida) (\\):: Eq a => [a] -> [a]
-> [a] que retorna a lista resultante de remover (as primeiras ocorrencias) dos elementos da
segunda lista da primeira.
Por exemplo, (\\)[1,2,3,4,5,1] [1,5] corresponde a [2,3,4,1]. -}

barrabarra :: Eq a => [a] -> [a] -> [a]
barrabarra [] _ = []
barrabarra l [] = l
barrabarra l (y:ys) = barrabarra (mydelete y l) ys  


{- 23. Apresente uma definição recursiva da função (pré-definida) union :: Eq a => [a] -> [a]
-> [a] que retorna a lista resultante de acrescentar à primeira lista os elementos da segunda
que não ocorrem na primeira.
Por exemplo, union [1,1,2,3,4] [1,5] corresponde a [1,1,2,3,4,5] -}

myunion :: Eq a => [a] -> [a] -> [a]
myunion l [] = l
myunion l (x:xs)
            | elem x l = myunion l xs
            | otherwise = myunion (l ++ [x]) xs

{- 24. Apresente uma definição recursiva da função (pré-definida) intersect :: Eq a => [a] ->
[a] -> [a] que retorna a lista resultante de remover da primeira lista os elementos que não
pertencem à segunda.
Por exemplo, intersect [1,1,2,3,4] [1,3,5] corresponde a [1,1,3].-}
 
myintersect :: Eq a => [a] -> [a] -> [a]
myintersect [] _ = []
myintersect l [] = []
myintersect (x:xs) l 
             | elem x l = [x] ++ myintersect xs l
             | otherwise = myintersect xs l


{- 25. Apresente uma definição recursiva da função (pré-definida) insert :: Ord a => a -> [a]
-> [a] que dado um elemento e uma lista ordenada retorna a lista resultante de inserir
ordenadamente esse elemento na lista.
Por exemplo, insert 25 [1,20,30,40] corresponde a [1,20,25,30,40]. -}

myinsert :: Ord a => a -> [a] -> [a]
myinsert a [] = [a]
myinsert a (x:xs)
          | a >= x = x:(myinsert a xs)
          | otherwise = (a:x:xs)

{- 26. Apresente uma definição recursiva da função (pré-definida) unwords :: [String] -> String que
junta todas as strings da lista numa só, separando-as por um espaço.
Por exemplo, unwords ["Programacao", "Funcional"] corresponde a "Programacao Funcional". -}

myunwords :: [String] -> String
myunwords [] = []
myunwords [x] = x
myunwords (x:xs) = x ++ " " ++ (myunwords xs)


{- 27. Apresente uma definição recursiva da função (pré-definida) unlines :: [String] -> String que
junta todas as strings da lista numa só, separando-as pelo caracter ’\n’.
Por exemplo, unlines ["Prog", "Func"] corresponde a "Prog\nFunc\n".-}

myunlines :: [String] -> String
myunlines [] = ""
myunlines [x] = x ++ "\n"
myunlines (x:xs) = x ++ "\n" ++ myunlines xs


{- 28. Apresente uma definição recursiva da função pMaior :: Ord a => [a] -> Int que dada
uma lista não vazia, retorna a posição onde se encontra o maior elemento da lista. As posições
da lista começam em 0, i.e., a função dever´a retornar 0 se o primeiro elemento da lista for o
maior.-}


mypMaior :: Ord a => [a] -> Int
mypMaior [x] = 0
mypMaior (x:xs) 
              | x >= mymaior xs = 0
              | otherwise = 1 + mypMaior xs

--mymaior e mymaior2 sao duas funçoes que retornam o maior elemento da lista. Posso usar uma ou outra.

mymaior :: Ord a => [a] -> a
mymaior [a] = a
mymaior (x:y:ys)
              | x >= y = mymaior (x:ys)  
              | otherwise = mymaior (y:ys)


mymaior2 :: Ord a => [a] -> a
mymaior2 [a] = a
mymaior2 (x:xs) 
         | x >= (mymaior2 xs) = x
         | otherwise = mymaior2 xs


{- 29. Apresente uma definição recursiva da função temRepetidos :: Eq a => [a] -> Bool que
testa se uma lista tem elementos repetidos.
Por exemplo, temRepetidos [11,21,31,21] corresponde a True enquanto que temRepetidos
[11,2,31,4] corresponde a False. -}

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos [x] = False
temRepetidos (x:xs)
                | elem x xs = True 
                | otherwise = temRepetidos xs 


{-30. Apresente uma definição recursiva da função algarismos :: [Char] -> [Char] que determina a lista dos algarismos de uma dada lista de caracteres.
Por exemplo, algarismos "123xp5" corresponde a "1235". -}

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (x:xs)
              | x >= '0' && x <= '9' = [x] ++ algarismos xs
              | otherwise = algarismos xs 


{- 31. Apresente uma definição recursiva da função posImpares :: [a] -> [a] que determina os
elementos de uma lista que ocorrem em posições ímpares. Considere que o primeiro elemento
da lista ocorre na posição 0 e por isso par.
Por exemplo, posImpares [10,11,7,5] corresponde a [11,5].-}

posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (x:y:ys) = y:(posImpares ys)


{- 32. Apresente uma definição recursiva da função posPares :: [a] -> [a] que determina os
elementos de uma lista que ocorrem em posições pares. Considere que o primeiro elemento da
lista ocorre na posição 0 e por isso par.
Por exemplo, posPares [10,11,7,5] corresponde a [10,7]. -}

posPares :: [a] -> [a] 
posPares [] = []
posPares [x] = [x]
posPares (x:y:ys) = x:(posPares ys)


{- 33. Apresente uma definição recursiva da função isSorted :: Ord a => [a] -> Bool que testa
se uma lista está ordenada por ordem crescente.
Por exemplo, isSorted [1,2,2,3,4,5] corresponde a True, enquanto que isSorted [1,2,4,3,4,5]
corresponde a False. -}

myisSorted :: Ord a => [a] -> Bool
myisSorted [] = True
myisSorted [x] = True
myisSorted (x:y:ys)
          | x <= y = myisSorted (y:ys)
          | otherwise = False 


{- 34. Apresente uma definição recursiva da função iSort :: Ord a => [a] -> [a] que calcula
o resultado de ordenar uma lista. Assuma, se precisar, que existe definida a função insert
:: Ord a => a -> [a] -> [a] que dado um elemento e uma lista ordenada retorna a lista
resultante de inserir ordenadamente esse elemento na lista. -}

myiSort :: Ord a => [a] -> [a]
myiSort [] = []
myiSort [x] = [x]
myiSort (x:xs) = myinsert x (myiSort xs) 


{- 35.Apresente uma definição recursiva da função menor :: String -> String -> Bool que
dadas duas strings, retorna True se e só se a primeira for menor do que a segunda, segundo
a ordem lexicográfica (i.e., do dicionário)
Por exemplo, menor "sai" "saiu" corresponde a True enquanto que menor "programacao"
"funcional" corresponde a False. -}

menor :: String -> String -> Bool
menor [] [] = False
menor l [] = False
menor [] l = True
menor (x:xs) (y:ys)
           | x == y = menor xs ys
           | x < y = True
           | otherwise = False


{- 36. Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
segunda componente seja menor ou igual a zero.
Defina a função elemMSet :: Eq a => a -> [(a,Int)] -> Bool que testa se um elemento
pertence a um multi-conjunto.
Por exemplo, elemMSet ’a’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a True enquanto
que elemMSet ’d’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a False. -}

myelemMSet :: Eq a => a -> [(a,Int)] -> Bool
myelemMSet x [] = False
myelemMSet a ((x,y):z)
           | a == x = True 
           | otherwise = myelemMSet a z 

{- 37. Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
segunda componente seja menor ou igual a zero.
Defina a função lengthMSet :: [(a,Int)] -> Int que calcula o tamanho de um multiconjunto.
Por exemplo, lengthMSet [(’b’,2), (’a’,4), (’c’,1)] corresponde a 7. -}

mylengthMSet :: [(a,Int)] -> Int
mylengthMSet [] = 0
mylengthMSet ((a,i):b) = i + mylengthMSet b 


{- 38. Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
segunda componente seja menor ou igual a zero.
Defina a função converteMSet :: [(a,Int)] -> [a] que converte um multi-conjuto na
lista dos seus elementos
Por exemplo, converteMSet [(’b’,2), (’a’,4), (’c’,1)] corresponde a "bbaaaac". -}

myconvertMSet :: [(a,Int)] -> [a]
myconvertMSet [] = []
myconvertMSet ((a,b):c) 
              | b == 0 = myconvertMSet c
              | otherwise = a:(myconvertMSet ((a,b-1):c)) 

{- 39. Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
segunda componente seja menor ou igual a zero.
Defina a função insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que acrescenta
um elemento a um multi-conjunto.
Por exemplo, insereMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a [(’b’,2),
(’a’,4), (’c’,2)]. -}

myinsereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
myinsereMSet a [] = [(a,1)]
myinsereMSet a ((b,c):d)
               | a == b = ((b,c+1):d)
               | otherwise = (b,c):(myinsereMSet a d)


{- 40. Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
segunda componente seja menor ou igual a zero.
Defina a função removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que remove um
elemento a um multi-conjunto. Se o elemento não existir, deve ser retornado o multi-conjunto
recebido.
Por exemplo, removeMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a [(’b’,2),
(’a’,4)]. -}  

myremoveMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
myremoveMSet a [] = []
myremoveMSet a ((b,c):d)
              | a == b && c == 1 = d
              | a == b && c /= 1 = ((b,c-1):d)
              | otherwise = (b,c):(myremoveMSet a d)

{- 41. Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
segunda componente seja menor ou igual a zero.
Defina a função constroiMSet :: Ord a => [a] -> [(a,Int)] dada uma lista ordenada
por ordem crescente, calcula o multi-conjunto dos seus elementos.
Por exemplo, constroiMSet "aaabccc" corresponde a [(’a’,3), (’b’,1), (’c’,3)]. -}

myconstroiMSet :: Ord a => [a] -> [(a,Int)]
myconstroiMSet [] = []
myconstroiMSet l = myinsereMSet (last l) (myconstroiMSet (init l))


{- 42.Apresente uma definição recursiva da função pré-definida partitionEithers :: [Either
a b] -> ([a],[b]) que divide uma lista de Eithers em duas listas. -}

mypartitionEithers :: [Either a b] -> ([a],[b])
mypartitionEithers [] = ([],[])
mypartitionEithers ((Right x):xs) = (fst(mypartitionEithers xs),x:(snd (mypartitionEithers xs)))
mypartitionEithers ((Left x):xs) = (x:(fst(mypartitionEithers xs)), snd (mypartitionEithers xs))

mypartitioEithers2 :: [Either a b] -> ([a],[b])
mypartitioEithers2 [] = ([],[])
mypartitioEithers2 ((Right x):xs) = (f, x:s)
   where (f,s) = (mypartitioEithers2 xs)
mypartitioEithers2 ((Left x):xs) = (x:f, s)
   where (f,s) = (mypartitioEithers2 xs)

{- 43. Apresente uma definição recursiva da função pré-definida catMaybes :: [Maybe a] -> [a]
que colecciona os elementos do tipo a de uma lista. -}

mycatMaybes :: [Maybe a] -> [a]
mycatMaybes [] = []
mycatMaybes (Just a:xs) = a:(mycatMaybes xs)
mycatMaybes (Nothing:xs) = mycatMaybes xs


{- 44. Considere o seguinte tipo para representar movimentos de um robot.

data Movimento = Norte | Sul | Este | Oeste
deriving Show

Defina a função posicao :: (Int,Int) -> [Movimento] -> (Int,Int) que, dada uma
posição inicial (coordenadas) e uma lista de movimentos, calcula a posição final do robot
depois de efectuar essa sequência de movimentos. -} 

data Movimento = Norte | Sul | Este | Oeste
 deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:ms) = posicao (x,y+1) ms
posicao (x,y) (Sul:ms) = posicao  (x,y-1) ms
posicao (x,y) (Este:ms) = posicao (x+1,y) ms
posicao (x,y) (Oeste:ms) = posicao (x-1,y) ms


{- 45. Considere o seguinte tipo para representar movimentos de um robot.
data Movimento = Norte | Sul | Este | Oeste
deriving Show
Defina a função caminho :: (Int,Int) -> (Int,Int) -> [Movimento] que, dadas as posições
inicial e final (coordenadas) do robot, produz uma lista de movimentos suficientes para que o
robot passe de uma posição para a outra. -}

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2)
                  | x1 > x2 = Oeste:(caminho(x1-1,y1) (x2,y2))
                  | x1 < x2 = Este:(caminho(x1+1,y1) (x2,y2))
                  | y1 > y2 = Sul:(caminho(x1,y1-1) (x2,y2))
                  | x1 < x2 = Norte:(caminho(x1,y1+1) (x2,y2))
                  | otherwise = []

{- 46. Considere o seguinte tipo para representar movimentos de um robot.
data Movimento = Norte | Sul | Este | Oeste
deriving Show

Defina a funçãoo vertical :: [Movimento] -> Bool que, testa se uma lista de movimentos
só é composta por movimentos verticais (Norte ou Sul). -}

vertical :: [Movimento] -> Bool
vertical [Norte] = True
vertical [Sul] = True
vertical (Norte:xs) = vertical xs
vertical (Sul:xs) = vertical xs
vertical _ = False


{- 47. Considere o seguinte tipo para representar a posição de um robot numa grelha.

data Posicao = Pos Int Int
deriving Show

Defina a função maisCentral :: [Posicao] -> Posicao que, dada uma lista não vazia de
posições, determina a que está mais perto da origem (note que as coordenadas de cada ponto
são números inteiros). -}

data Posicao = Pos Int Int
 deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [p] = p
maisCentral ((Pos x1 y1):(Pos x2 y2):ps)
                 | (x1^2 + y1^2) <= (x2^2 + y2^2) = maisCentral ((Pos x1 x2):ps)
                 | otherwise = maisCentral ((Pos x2 y2):ps) 

{- 48. Considere o seguinte tipo para representar a posição de um robot numa grelha.

data Posicao = Pos Int Int
deriving Show

Defina a função vizinhos :: Posicao -> [Posicao] -> [Posicao] que, dada uma posição
e uma lista de posições, selecciona da lista as posições adjacentes à posição dada. -}


vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos p [] = []
vizinhos (Pos x1 y1) ((Pos x2 y2):ps)
              | x2 == x1 && y2 == y1 + 1 = (Pos x2 y2):(vizinhos (Pos x1 y1) ps) 
              | x2 == x1 && y2 == y1 - 1 = (Pos x2 y2):(vizinhos (Pos x1 y1) ps)
              | y2 == y1 && x2 == x1 +1 = (Pos x2 y2):(vizinhos (Pos x1 y1) ps)
              | y2 == y1 && x2 == x1 -1 = (Pos x2 y2):(vizinhos (Pos x1 y1) ps)
              | otherwise = vizinhos (Pos x1 y1) ps


{- 49. Considere o seguinte tipo para representar a posição de um robot numa grelha.

data Posicao = Pos Int Int
deriving Show

Defina a função mesmaOrdenada :: [Posicao] -> Bool que testa se todas as posições de
uma dada lista têm a mesma ordenada. -}


mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada [x] = True
mesmaOrdenada ((Pos x1 y1):(Pos x2 y2):ps)
               | y1 == y2 = mesmaOrdenada ((Pos x2 y2):ps)
               | otherwise = False


{- 50. Considere o seguinte tipo para representar o estado de um semáforo.

data Semaforo = Verde | Amarelo | Vermelho
deriving Show

Defina a função interseccaoOK :: [Semaforo] -> Bool que testa se o estado dos semáforos
de um cruzamento é seguro, i.e., não há mais do que semáforo não vermelho. -}

data Semaforo = Verde | Amarelo | Vermelho
 deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK [_] = True
interseccaoOK (Vermelho:xs) = interseccaoOK xs
interseccaoOK (Amarelo:Vermelho:xs) = interseccaoOK (Amarelo:xs)
interseccaoOK (Verde:Vermelho:xs) = interseccaoOK (Amarelo:xs)
interseccaoOK (_:_:xs) = False

--ou

interseccaoOK2 :: [Semaforo] -> Bool
interseccaoOK2 [x] = True
interseccaoOK2 lista
                  | contaverdeamarelo lista > 1 = False 
                  | otherwise = True


--conta o numero de semaforos verdes e amarelos

contaverdeamarelo :: [Semaforo] -> Int
contaverdeamarelo [] = 0
contaverdeamarelo (Vermelho:xs) = contaverdeamarelo xs
contaverdeamarelo (_:xs) = 1 + contaverdeamarelo xs

