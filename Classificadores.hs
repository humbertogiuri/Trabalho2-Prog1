module Classificadores (
    retornaClassesUnicas,
    retornaPontosDaClasseTreino,
    retornaPontosDaClasseTeste,
    contaVizinhos,
    retornaVizinhos,
    aplicaKNNpara1Folde,
    centroidesParaFoldes,
    predicoesPraFolde,
    geraMatrizConfusaoFolde,
    geraDocumentoSaida
) where

import Utils
import Data.List (intercalate, sortBy, minimumBy)
import Data.Ord (comparing) 

{-
   Input: 1 tipo Dataset.
   Output: 1 vetor de strings.
   
   Acao: Percorre o dataset inteiro pegando apenas as classes e deixando as coordenadas de lado.
         Após isso, utiliza a funcao ja implementada removeDup para deixar apenas uma ocorrencia
         de cada classe, isso nos retorna um vetor contendo as classes que são representadas
         no dataset.
-}
retornaClassesUnicas :: [Ponto] -> [String]
retornaClassesUnicas dataset = removeDup . retornaClasses $ dataset
    where
        retornaClasses dataset = [classe ponto | ponto <- dataset]


{-
   Input: 1 tipo Dataset e uma string.
   Output: 1 vetor de vetores de doubles.
   
   Acao: Percorre o tipo Dataset passado e pega apenas as coordenadas cartesianas (vetor de doubles)
         e vai colocando essas coordenadas em um vetor.
-}
retornaPontosDaClasseTreino :: Dataset -> String -> [[Double]]
retornaPontosDaClasseTreino dataset classeRequerida = [coordenadas dado | dado <- treino dataset,  classe dado == classeRequerida]

{-
   Input: 1 tipo Dataset e uma string.
   Output: 1 vetor de vetores de doubles.
   
   Acao: Percorre o tipo Dataset passado e pega apenas as coordenadas cartesianas (vetor de doubles)
         e vai colocando essas coordenadas em um vetor.
-}
retornaPontosDaClasseTeste :: Dataset -> String -> [[Double]]
retornaPontosDaClasseTeste dataset classeRequerida = [coordenadas dado | dado <- teste dataset,  classe dado == classeRequerida]


retornaVizinhos :: Dataset -> Ponto -> Int -> [(Double, String)]
retornaVizinhos dataset ponto k = selecionaVizinhos [(distanciaEuclediana (coordenadas pontoAux) (coordenadas ponto), classe pontoAux) 
                                                | pontoAux <- datasetTreino]
    where
        datasetTreino = treino dataset
        selecionaVizinhos vetor =  take k (sortBy (comparing fst) vetor)


contaVizinhos :: [(Double, String)] -> [(Int, Double, String)]
contaVizinhos vizinhos = [(percorre x, calculaMedia $ somaLista x, x) | x <- removeDup $ map snd vizinhos]
    where
        percorre classe = sum [if snd x == classe then 1 else 0 | x <- vizinhos]
        somaLista classe = [if snd x == classe then fst x else 0 | x <- vizinhos]
        calculaMedia lista = sum lista / fromIntegral(length lista)


selecionaVizinho :: [(Int, Double, String)] -> String
selecionaVizinho [] = error "impossivel selecionar vizinho"
selecionaVizinho vizinhosAparicoes
    | length vizinhosAparicoes == 1 = thd3 $ head vizinhosAparicoes
    | otherwise = thd3 $ decidePorFrequencia vizinhosAparicoes
    where
        decidePorFrequencia iguais = foldr1 compara iguais
        compara x acc 
            | fst3 x > fst3 acc = x
            | fst3 x == fst3 acc && snd3 x < snd3 acc = x
            | otherwise = acc


knn :: Dataset -> Int -> Ponto -> String
knn dataset k ponto = selecionaVizinho (contaVizinhos vizinhosAparicoes)
    where
        vizinhosAparicoes = retornaVizinhos dataset ponto k


aplicaKNNpara1Folde :: Int -> Dataset-> [String]
aplicaKNNpara1Folde k dataset = map (knn dataset k) datasetTeste 
    where
        datasetTeste = teste dataset

{-
   Input: 1 tipo Dataset e uma string.
   Output: 1 vetor de vetores de doubles.
   
   Acao: Percorre o tipo Dataset passado e pega apenas as coordenadas cartesianas (vetor de doubles)
         e vai colocando essas coordenadas em um vetor.
-}
retornaPontosDaClasse :: Dataset -> String -> [[Double]]
retornaPontosDaClasse dataset clas = [coordenadas dado | dado <- treino dataset,  classe dado == clas]

{-
   Input: 1 vetor de vetores de doubles e 1 inteiro.
   Output: 1 vetor de doubles.
   
   Acao: Passa pelos vetor de vetores de double somando os vetores usando a funcao zipWith.
         Quando sobra apenas um vetor de doubles, que representa o somatorio de todos os vetores,
         divide todos os valores do vetor pelo tamanho total do vetor de vetores.
         No final sobra um vetor de doubles representando a media de todos os vetores
         isso eh a coordenada media de todos os pontos que representa as coordenadas
         do centroide.
-}
calculaCentroide :: [[Double]] -> Int -> [Double]
calculaCentroide [] _ = error "impossivel calcular centroide"
calculaCentroide _ 0 = error "impossivel calcular centroide"
calculaCentroide [x] i = [a / (fromIntegral i) | a <- x]
calculaCentroide (x:y:pontos) i = calculaCentroide ((zipWith (+) x y):pontos) (i + 1)


{-
   Input: 1 tipo Dataset e 1 vetor de strings.
   Output: 1 vetor de strings.
   
   Acao: Aplica a funcao calculaCentroide para cada classe presente no vetor de strings.
-}
centroides :: Dataset -> [String] ->  [([Double], String)]
centroides dataset classes = [(calculaCentroide (retornaPontosDaClasse dataset classe) 1, classe) | classe <- classes]


centroidesParaFoldes :: [Dataset] -> [[([Double], String)]]
centroidesParaFoldes datasets = [resultado dado | dado <- datasets]
    where
        resultado dado = centroides dado (retornaClassesUnicas (treino dado))

{-
   Input: 1 tipo Dataset e 1 tipo Ponto
   Output: Uma string.
   
   Acao: Passa pelos pontos do dataset calculando a distancia desses pontos com o ponto passado
         para funcao e seleciona o ponto do dataset com a menor distancia pro ponto passado.
         Apos saber o ponto do dataset com menor distancia pro ponto passado eh retornado
         a classe que esse ponto pertence.
-}
predicao :: [([Double], String)] -> Ponto -> String
predicao dataset pontoTeste = snd $ minimumBy (comparing fst) ([(distancia (fst ponto), snd ponto) | ponto <- dataset])
    where
        distancia ponto = distanciaEuclediana ponto (coordenadas pontoTeste)


{-
   Input: 2 tipos Dataset.
   Output: 1 vetor de strings.
   
   Acao: aplica a funcao de predicao a todos os pontos do segundo dataset passado.
-}
todasPredicoes :: [([Double], String)] -> Dataset -> [String]
todasPredicoes pontos dataset = map (predicao pontos) (teste dataset)



predicoesPraFolde :: [[([Double], String)]] -> [Dataset] -> [[String]]
predicoesPraFolde centroides dataset = [todasPredicoes x y | (x, y) <- zip centroides dataset]


{-
   Input: 3 vetores de strings.
   Output: 1 vetor de vetores de inteiros.
   
   Acao: Os 2 primeiros vetores passados para a funcao representam as classes que foram
         preditas pelo classificador e o outro sao as classes reais. O terceiro vetor
         representa as classes que podem ser classificadas. Então, ele seleciona duas classes
         das classes que podem ser preditas e confere, comparando os 2 primeiros vetores,
         a quantidade de vezes que a classe foi predita como a classe ou se foi predita como outra.
         Ele quantifica todos os resultados e coloca em uma matriz.
-}
geraMatrizConfusao :: [String] -> [String] -> [String] -> [[Int]]
geraMatrizConfusao [] _ _ = error "impossivel gerar matriz de confusao"
geraMatrizConfusao _ [] _ = error "impossivel gerar matriz de confusao"
geraMatrizConfusao _ _ [] = error "impossivel gerar matriz de confusao"
geraMatrizConfusao predicoes verdadeiros classes = 
    transposta  [ [celulaMatriz predicoes verdadeiros classe1 classe2| classe2 <- classes] 
        | classe1 <- classes]
    where
        celulaMatriz [] _ _ _ = 0
        celulaMatriz _ [] _ _ = 0
        celulaMatriz (p:predicoes) (v:verdadeiros) classe1 classe2 
            | classe1 == v && classe2 == p = 1 + celulaMatriz predicoes verdadeiros classe1 classe2
            | otherwise = celulaMatriz predicoes verdadeiros classe1 classe2

        transposta ([]:_) = []
        transposta matriz =  (map head matriz) : transposta (map tail matriz)


geraMatrizConfusaoFolde :: [[String]] -> [[String]] -> [String] ->[[[Int]]]
geraMatrizConfusaoFolde predicoes reais classes = [geraMatrizConfusao p r classes | (p, r) <- zip predicoes reais]


somaMatrizes :: [[[Int]]] -> [[Int]]
somaMatrizes [m] = m
somaMatrizes (a:b:matrizes) = somaMatrizes (soma2 a b : matrizes)
    where
        soma2 m1 m2 = [geraLinha x y | (x, y) <- zip m1 m2]
        geraLinha l1 l2 = [x + y | (x, y) <- zip l1 l2] 


{-
    Input: 1 vetor de vetor de inteiros.
    Output: Uma string.
    Acao: Cada vetor de inteiros representa uma linha da matriz, assim essa funcao
          percorre esses vetores transformando os vetores para string e adicionando
          as virgulas, espaços e '\n' onde sao necessarios usando a funcao foldr.
-}
formataMatrizConfusao :: [[Int]] -> String
formataMatrizConfusao matriz = intercalate "\n"  (map (tail . concat) (map processaLinha matriz))
    where
        processaLinha linha = foldr (\x acc -> (',':(espacos x)++show x):acc) [] linha
        espacos x = take (3 - length(show x)) (repeat ' ')


{-
    Input: 1 FilePath e 2 vetores de vetores de inteiros (matrizes).
    Output: IO().
    Acao: Apenas transfere pro arquivo determinado e como o formato especificado as matrizes.
-}
geraDocumentoSaida :: FilePath -> [[[Int]]] -> [[[Int]]] -> [[[Int]]] -> IO ()
geraDocumentoSaida arquivo matrizVizinhos matrizCentroides matrizKnn = do
   writeFile arquivo "vizinho mais próximo:\n"
   appendFile arquivo (formataMatrizConfusao (somaMatrizes matrizVizinhos))
   appendFile arquivo "\n\n"

   appendFile arquivo "centroides:\n"
   appendFile arquivo (formataMatrizConfusao (somaMatrizes matrizCentroides))
   appendFile arquivo "\n\n"

   appendFile arquivo "k-vizinhos mais próximos:\n"
   appendFile arquivo (formataMatrizConfusao (somaMatrizes matrizKnn))
   