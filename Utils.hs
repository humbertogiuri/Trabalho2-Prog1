module Utils(
   toDouble,
   toInt,
   fst3,
   snd3,
   thd3,
   pegaInfo,
   split,
   formataLinha,
   distanciaEuclediana,
   removeDup,
   geraVetorValoresAleatorios,
   geraFolds,
   criaPonto,
   criaDataset,
   criaTodosDatasets,
   calculaAcuraciaTodosFoldes,
   retornaReais,
   mediaMatriz,
   desvioPadraoMatriz,
   padronizar,
   desvioPadraoAcuracias,
   coordenadas,
   classe,
   teste,
   treino,
   Ponto,
   Dataset,
) where

import System.Random (randomRs, mkStdGen)
import System.IO
import Data.List (transpose)

{-
   Tipo criado que representa um ponto e ele contém um vetor de Doubles representando as coordenadas 
   e uma String que representa qual classe ele faz parte.
-}
data Ponto = Ponto [Double]  String deriving(Show)

{-
   Retorna o vetor de Doubles que representa as coordenadas do ponto.
-}
coordenadas :: Ponto -> [Double]
coordenadas (Ponto coord _) = coord

{-
   Retorna a classe que ponto carrega.
-}
classe :: Ponto -> String
classe (Ponto _ classe) = classe


{-
   Tipo que representa uma dataset e contem dois vetores de Pontos representando os pontos de treio
   e os de teste.
-}
data Dataset = Dataset {
   datasetTreino :: [Ponto],   
   datasetTeste :: [Ponto]
} deriving(Show)

--Retorna o dataset de teste.
teste :: Dataset -> [Ponto]
teste =  datasetTeste

--Retorna o dataset de treino.
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
   retorna a primeira posicao para um tupla de 3 elementos
-}
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a


{-
   retorna a segunda posicao para um tupla de 3 elementos
-}
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b


{-
   retorna a terceira posicao para um tupla de 3 elementos
-}
thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c


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
   Input: 3 inteiros.
   Output: 1 vetor de inteiros.
   
   Acao: gera 1 vetor de valores aleatorios e unicos para selecionar esses pontos para
         nosso dataset de teste. Esse vetor terá o tamanho que foi indicado para o dataset de teste. 
-}
geraVetorValoresAleatorios :: Int -> Int -> [Int]
geraVetorValoresAleatorios seed tamanhoTotal = take (tamanhoTotal) (removeDup ((randomRs (0, tamanhoTotal - 1) (mkStdGen seed) :: [Int])))


{-
   gera um vetor de vetores de inteiros que representam os indices dos dados que cada folde representa.  
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


{-
   Recebe um vetor de string e esse vetor representa uma linha do arquivo csv.
   Esse funcao pega essa linha e transforma ela em um ponto.
-}
criaPonto :: [String] -> Ponto
criaPonto [] = error "impossivel criar pontos"
criaPonto linha = Ponto (fst resultado) (snd resultado)
    where
        resultado = formataLinha linha

{-
   Pega um vetor de Pontos e os indices que sao para teste transforma ele em um dataset
   jogando os pontos que sao de teste para o dataset de teste e os outros pro de treino.
-}
criaDataset :: [[Int]] -> [Int] -> [Ponto] -> Int -> Dataset
criaDataset [] _ _ _= error "impossivel criar dataset"
criaDataset _ [] _ _= error "impossivel criar dataset"
criaDataset _ _ [] _= error "impossivel criar dataset"
criaDataset foldes indicesLinhas pontos indiceTeste = Dataset {datasetTreino = treino, datasetTeste = teste}
   where
      teste = [pontos !! x | x <- foldes !! indiceTeste]
      treino = [pontos !! y | y <- indicesLinhas, y `notElem` (foldes !! indiceTeste)]


{-
   Aplica a funcao criaDataset para todos os folds retornando um vetor de datasets.
-}
criaTodosDatasets :: [[Int]] -> [Int] -> [Ponto] -> [Dataset]
criaTodosDatasets [] _ _ = error "impossivel criar dataset"
criaTodosDatasets _ [] _ = error "impossivel criar dataset"
criaTodosDatasets _ _ [] = error "impossivel criar dataset"
criaTodosDatasets foldes indicesLinhas pontos = [criaDataset foldes indicesLinhas pontos x | x <- foldeDaVez]
   where
      qtdFoldes = length foldes
      foldeDaVez = [0..qtdFoldes - 1]


{-
   Input: 2 vetores de strings.
   Output: 1 Double.
   
   Acao: Compara os dois vetores de strings e cada vez que, na mesma posicao, as informacoes
         forem iguais conta um acerto. Dividimos o numero de acertos pelo tamanho do vetor
         e temos nossa acuracia.
-}
calculaAcuracia :: [String] -> [String] -> Double
calculaAcuracia [] _ = error "impossivel calcular acuracia"
calculaAcuracia _ [] = error "impossivel calcular acuracia"
calculaAcuracia predicoes reais  = (calculaQuantidadeCorretos predicoes reais) / fromIntegral (length reais)
   where
      calculaQuantidadeCorretos _ [] = 0
      calculaQuantidadeCorretos [] _ = 0
      calculaQuantidadeCorretos (p:predicoes) (r:reais)
         | p == r = 1 + calculaQuantidadeCorretos predicoes reais
         | otherwise = 0 + calculaQuantidadeCorretos predicoes reais


{-
   Aplica a funcao calculaAcuracia para todos os folds.
-}
calculaAcuraciaTodosFoldes :: [[String]] -> [[String]] -> [Double]
calculaAcuraciaTodosFoldes predicoes reais = [calculaAcuracia p r | (p, r) <- zip predicoes reais]

{-
   retorna as classes reias dos pontos para que possamos, mais tarde, saber de a predicao foi correta.
-}
retornaReais :: [[Ponto]] -> [[String]]
retornaReais pontos = [retornaClasses ponto | ponto <- pontos]
   where
      retornaClasses ponto = [classe x | x <- ponto]

{-
   pega as coordenadas dos pontos do dataset de treino e transforma em uma matriz, assim
   calcula a media de cada coordenada da matriz, ou seja, calcula a media de cada coluna.
-}
mediaMatriz :: Dataset -> [Double]
mediaMatriz dataset = [sum x / fromIntegral(length x) | x <- transposta]
   where
      matriz = [coordenadas vetor | vetor <- treino dataset]
      transposta = transpose matriz


{-
   Novamente transforma os pontos em uma matriz e calcula o desvio padrão.
-}
desvioPadraoMatriz :: Dataset -> [Double]
desvioPadraoMatriz dataset = [resultado x m | (x, m) <- zip transposta media]
   where
      matriz = [coordenadas vetor | vetor <- treino dataset]
      transposta = transpose matriz
      media = mediaMatriz dataset
      tamanho x = fromIntegral (length (treino dataset))
      menosMedia x m = map ((-) m) x
      quadrado x = sum (map (^2) x)
      dividido x = x / tamanho x
      resultado x m = sqrt (dividido (quadrado (menosMedia x m)))


{-
   Funcao que aplica a padronizacao de uma dataset, ou seja, aplica a formula z = (x - u)/desvio
   onde z é o novo valor para a coordenada, x eh a coordenada avaliada, u eh a media dos valores padronizdos
   e desvio eh o desvio padrao dos valores padronizados.
-}
padronizar :: Dataset -> Dataset
padronizar dataset = Dataset treinoPadronizado testePadronizado 
   where
      media = mediaMatriz dataset
      desvio = desvioPadraoMatriz dataset
      padroniza x = zipWith (/) (zipWith (-) x media) desvio
      treinoPadronizado = [Ponto (padroniza (coordenadas x)) (classe x) | x <- treino dataset]
      testePadronizado = [Ponto (padroniza (coordenadas x)) (classe x) | x <- teste dataset]


{-
   Recebe um lista contendo as acuracias para cada folde e retorna o desvio padrao
   dessas acuracias.
-}
desvioPadraoAcuracias :: [Double] -> Double
desvioPadraoAcuracias acuracias = sqrt (dividido (quadrado (menosMedia acuracias)))
   where
      tamanho = fromIntegral (length acuracias)
      mediaAcuracias = (sum acuracias) / tamanho
      menosMedia x = map ((-) mediaAcuracias) x
      quadrado x = sum (map (^2) x)
      dividido x = x / tamanho
