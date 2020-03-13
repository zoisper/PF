
import System.Random
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as Text
import Control.Monad

bingo :: IO ()
bingo = do 
         putStrLn "Benvindo ao Bingo\n"
         putStrLn "Pressione Para Começar\n"
         getChar
         let l = [x | x <- [1..99]]
         sorteio <- baralha l
         mostra sorteio []

baralha :: [Int] -> IO [Int]
baralha [] = return []
baralha (x:xs) = do 
                  b <- randomRIO (0, (length (x:xs)) -1)
                  let (h,c:ts) = splitAt b (x:xs) 
                  t <- baralha (h ++ ts)
                  return (c:t) 


mostra :: [Int] -> [Int] -> IO ()
mostra [] sorteados = do 
                       putStrLn (show sorteados)  
                       putStrLn "Fim Do Sorteio"
mostra lista sorteados = do 
                          putStr ((show (head lista))  ++ "\n" ++ "\n")
                          let sorteadosmaisum = insere (head lista) sorteados
                          putStrLn (show (sorteadosmaisum) ++ "\n")
                          putStrLn "Pressione uma tecla para continua\n"
                          getChar
                          mostra (tail lista) sorteadosmaisum              

insere :: Int -> [Int] -> [Int]
insere x [] = [x]
insere x (h:t) | x >= h = (h:(insere x t))
               | otherwise = (x:h:t)  
-- -----------------------------------------------------------------------------------------------

mastermind :: IO ()
mastermind = do
              putStrLn "Benvindo ao Mastermind"
              segredo <- gerasegredo
              ciclo 15 segredo 

 

gerasegredo :: IO String
gerasegredo = do
                a <- randomRIO ('1','4')
                b <- randomRIO ('1','4')
                c <- randomRIO ('1','4')
                d <- randomRIO ('1','4')
                return (a:b:c:d:[])



gerajogada :: IO String
gerajogada = do
              putStrLn "Digite 4 Digitos\n"
              jogada <- getLine
              if ((length jogada == 4) && testadigitos jogada ) then return jogada
              else do gerajogada 

testadigitos :: String -> Bool
testadigitos [] = True
testadigitos (x:xs) | (isDigit x) && x >= '1' && x <= '4' = testadigitos xs
                    | otherwise = False

ciclo :: Int -> String -> IO ()
ciclo 0 segredo = do 
                putStrLn "Ja Foste\n"
                putStrLn (show segredo)
ciclo n segredo = do
                   jogada <- gerajogada 
                   if (segredo == jogada) then putStrLn "Ganhou"
                   else check segredo jogada
                   ciclo (n-1) segredo 

check :: String -> String -> IO ()
check segredo jogada = do
                        let cc = certascertas segredo jogada
                        let ce = ((certassitioerrado segredo jogada) - cc)
                        putStrLn ("Certas no sitio certo = " ++ (show cc) ++ "\n" ++ "Certas no sitio errado = " ++ (show ce) ++ "\n")


certascertas :: String -> String -> Int
certascertas [] _ = 0
certascertas (x:xs) (y:ys) | x == y = 1 + (certascertas xs ys)
                           | otherwise = certascertas xs ys 

certassitioerrado :: String -> String -> Int
certassitioerrado [] _ = 0
certassitioerrado (x:xs) y | elem x y = 1 + (certassitioerrado xs y)
                           | otherwise = certassitioerrado xs y 




-- 