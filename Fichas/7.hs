
import Data.Maybe

data BTree a = Empty | Node a (BTree a) (BTree a)
 deriving Show
data LTree a = Tip a | Fork (LTree a) (LTree a)
 deriving Show

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
 deriving (Show,Eq)


splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty, Tip b)
splitFTree (No a f1 f2) = (Node a b c, Fork d e) 
 where (b,d) = splitFTree f1
       (c,e) = splitFTree f2


joinTrees :: Eq a => BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip a) = Just (Leaf a)
joinTrees (Empty) (Fork a b) = Nothing
joinTrees (Node a b c) (Tip x) = Nothing 
joinTrees (Node a b1 b2) (Fork l1 l2 ) | isNothing(joinTrees b1 l1 ) || isNothing (joinTrees b2 l2) = Nothing 
                                       | otherwise = Just (No a bl1 bl2)
                                      where bl1 = fromJust (joinTrees b1 l1)
                                            bl2 = fromJust (joinTrees b2 l2)


--(No 1 (Leaf 2) (No 2 (Leaf 3) (Leaf 4)))

bt1 :: Num a => BTree a
bt1 = (Node 1 Empty (Node 2 Empty Empty))

lt1 :: Num a => LTree a
lt1 = (Fork (Tip 2) (Fork (Tip 3) (Tip 4))) 

lt2 :: Num a => LTree a
lt2 = (Fork (Fork (Tip 2) (Fork (Tip 3) (Tip 4))) (Tip 2))



