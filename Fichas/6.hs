

data BTree a = Empty | Node a (BTree a) (BTree a)
 deriving Show
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int | Rep | Faltou
 deriving Show
type Turma = BTree Aluno -- ´arvore bin´aria de procura (ordenada por n´umero)

arvore1 = (Node 3 (Node 5 (Node 7 Empty Empty) Empty) (Node 6 Empty Empty))
arvore2 = (Node 2 (Node 3 (Node 1 Empty Empty) Empty) Empty)
turma1 :: BTree Aluno
turma1 = (Node (10,"Ana",TE,Aprov 15) (Node (9,"Pedro", MEL,Aprov 11) (Node (6,"Joana",ORD, Rep) Empty Empty) Empty) (Node (12, "Lina", MEL, Faltou) Empty Empty))
turma2 :: BTree Aluno
turma2 = (Node (10,"Ana",TE,Rep) (Node (9,"Pedro", MEL,Aprov 11) (Node (6,"Joana",ORD, Rep) Empty Empty) Empty) Empty)

percFaltas :: Turma -> Float
percFaltas turma = ((fromIntegral faltaram) / (fromIntegral todos)) * 100
 where (faltaram, todos) = (raciofalta turma)

raciofalta :: Turma -> (Int,Int)
raciofalta Empty = (0,0)
raciofalta (Node (_,_,_,Faltou) e d) = (1 + fe + fd, 1 + te + td)
 where (fe, te) = raciofalta e
       (fd, td) = raciofalta d 	
raciofalta (Node (_,_,_,_) e d)  = (0 + fe + fd, 1 + te + td)
 where (fe, te) = raciofalta e
       (fd, td) = raciofalta d 


mediaAprov :: Turma -> Float
mediaAprov turma = ((fromIntegral positiva) / (fromIntegral todos)) 
 where (positiva, todos) = (raciopositiva turma)

raciopositiva :: Turma -> (Int,Int)
raciopositiva Empty = (0,0)
raciopositiva (Node (_,_,_,Aprov x) e d) = (x + fe + fd, 1 + te + td)
 where (fe, te) = raciopositiva e
       (fd, td) = raciopositiva d 
raciopositiva (Node (_,_,_,_) e d)  = (0 + fe + fd, 0 + te + td)
 where (fe, te) = raciopositiva e
       (fd, td) = raciopositiva d 

aprovAv :: Turma -> Float
aprovAv turma = ((fromIntegral aprovados) / (fromIntegral avaliados) *100)
 where (aprovados, avaliados) = racioaprovados turma

racioaprovados :: Turma -> (Int,Int)
racioaprovados Empty = (0,0)
racioaprovados  (Node (_,_,_,Aprov x) e d) = (1 + apesq + apdir, 1 + avaesq + avadit)
 where (apesq, avaesq) = racioaprovados e
       (apdir, avadit) = racioaprovados d
racioaprovados (Node (_,_,_,Rep) e d) = (0 + apesq + apdir, 1 + avaesq + avadit)
 where (apesq, avaesq) = racioaprovados e
       (apdir, avadit) = racioaprovados d
racioaprovados (Node (_,_,_,_) e d) = (0 + apesq + apdir, 0 + avaesq + avadit)
 where (apesq, avaesq) = racioaprovados e
       (apdir, avadit) = racioaprovados d



