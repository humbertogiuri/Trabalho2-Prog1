module Utils(
   toDouble,
   toInt,
   pegaInfo,
   split,
   formataLinha,
   distanciaEuclediana,
   removeDup,
   geraFolds,
   criaPonto,
   criaDataset,
   criaTodosDatasets,
   coordenadas,
   classe,
   teste,
   treino,
   Ponto,
   Dataset,
) where

import System.Random (randomRs, mkStdGen)
import System.IO


data Ponto = Ponto [Double]  String deriving(Show)

coordenadas :: Ponto -> [Double]
coordenadas (Ponto coord classe) = coord

classe :: Ponto -> String
classe (Ponto coord classe) = classe


data Dataset = Dataset {
   datasetTreino :: [Ponto],   
   datasetTeste :: [Ponto]
} deriving(Show)

teste :: Dataset -> [Ponto]
teste =  datasetTeste

treino :: Dataset -> [Ponto]
treino =  datasetTreino

{-
   Input: Uma String.
   Output: 1 Double.
   
   Acao: Usa a funcao read para transformar uma string em 1 double.
-}
toDouble :: String -> Double
toDouble = read

{-
   Input: Uma String.
   Output: 1 inteiro.
   
   Acao: Usa a funcao read para transformar uma string em 1 inteiro.
-}
toInt :: String -> Int
toInt = read

{-
   Input: Uma String.
   Output: IO String
   
   Acao: Passa uma mensagem pro usuario no terminal e pega a proximo coisa que o usuario digitar
-}
pegaInfo :: String -> IO String
pegaInfo mensagem = do
   putStr mensagem
   hFlush stdout
   info <- getLine
   return info

{-
   Input: Uma String.
   Output: 1 vetor de Strings.
   
   Acao: Recebe uma string, representado uma linha do csv e separa todos os argumentos
         tendo em relação a vírgula para separar.
-}
split :: String -> [String]
split [] = []
split lista = split' (reverse lista) [[]]
    where
        split' [] ls = ls
        split' (x:xs) lista_aux@(l:ls)
            | x /= ',' =  split' xs ((x:l):ls)
            | otherwise = split' xs ([]:lista_aux)

{-
   Input: 1 vetor de strings.
   Output: 1 tipo Ponto.
   
   Acao: transforma o vetor de strings, que representa uma linha do csv ou seja 1 ponto,
         e transforma ele em 1 tipo ponto que contém as coordenadas cartesianas e o nome da classe.
-}
formataLinha :: [String] -> ([Double], String)
formataLinha linha = (map toDouble $ init linha, last linha)


{-
   Input: 2 vetores de double.
   Output: 1 double.
   
   Acao: Calcula a distancia euclidiana entre 2 vetores de doubles, esses vetores
         representam as coordenadas cartesianas de um ponto, logo eh calculada a distancia
         entre dois pontos.
-}
distanciaEuclediana :: [Double] -> [Double] -> Double
distanciaEuclediana [] _ = error "ponto invalido"
distanciaEuclediana _ [] = error "ponto invalido"
distanciaEuclediana xs ys = sqrt . sum $ zipWith (\x y -> (x - y)^2) xs ys

{-
   Input: 1 vetor de tipos Eq.
   Output: 1 vetor de tipos Eq.
   
   Acao: Percorre o vetor e remove informacoes iguais.
-}
removeDup :: Eq a => [a] -> [a]
removeDup l = removeD l []
   where
      removeD [] _ = []
      removeD (x:xs) ls
         | x `elem` ls = removeD xs ls
         | otherwise = x: removeD xs (x:ls)

{-
-}
geraFolds :: Int -> [Int] -> [[Int]]
geraFolds 0 _ = error "impossivel gerar foldes"
geraFolds k aleatorios = foldesFinais foldsIniciais (tamanho * k)
   where
      foldsIniciais = [[aleatorios !! x | x <- [(y * tamanho)..((y * tamanho) - 1 + tamanho)]] 
                                       | y <- [0..(k - 1)]]
      tamanho = length aleatorios `div` k
      foldesFinais (folde:foldes) ind
         | ind <= (length aleatorios) - 1 = [folde ++ [aleatorios !! ind]] ++ foldesFinais foldes (ind + 1)
         | foldes == [] = [folde]
         | otherwise = [folde] ++ foldesFinais foldes (ind + 1)


criaPonto :: [String] -> Ponto
criaPonto [] = error "impossivel criar pontos"
criaPonto linha = Ponto (fst resultado) (snd resultado)
    where
        resultado = formataLinha linha

criaDataset :: [[Int]] -> [Int] -> [Ponto] -> Int -> Dataset
criaDataset [] _ _ _= error "impossivel criar dataset"
criaDataset _ [] _ _= error "impossivel criar dataset"
criaDataset _ _ [] _= error "impossivel criar dataset"
criaDataset foldes indicesLinhas pontos indiceTeste = Dataset {datasetTreino = treino, datasetTeste = teste}
   where
      teste = [pontos !! x | x <- foldes !! indiceTeste]
      treino = [pontos !! y | y <- indicesLinhas, y `notElem` (foldes !! indiceTeste)]


criaTodosDatasets :: [[Int]] -> [Int] -> [Ponto] -> Int -> [Dataset]
criaTodosDatasets [] _ _ _= error "impossivel criar dataset"
criaTodosDatasets _ [] _ _= error "impossivel criar dataset"
criaTodosDatasets _ _ [] _= error "impossivel criar dataset"
criaTodosDatasets foldes indicesLinhas pontos k = [criaDataset foldes indicesLinhas pontos x | x <- foldeDaVez]
   where
      foldeDaVez = [0..k - 1]
   