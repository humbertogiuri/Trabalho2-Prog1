import Utils
import Classificadores
import System.IO
import Text.Printf (printf)

main :: IO ()
main = do
   --Pega os dados de entrada necessarios para o programa
   nomeArquivoEntrada <- pegaInfo "Forneca o nome do arquivo de entrada: "
   nomeArquivoSaida <- pegaInfo "Forneca o nome do arquivo de saida: "
   qtdFoldes <- pegaInfo "Forneca o numero de folds: "
   k <- pegaInfo "Forneca o numero de vizinhos: "
   seed <- pegaInfo "Forneca o valor da semente para geracao randomizada: "


   content <- readFile nomeArquivoEntrada

   --Le as linhas e transforma em pontos e tambem retorna as classes presentes no arquivo.
   let linhas = map split $ lines content
   let pontos = map criaPonto linhas
   let classes = retornaClassesUnicas pontos

   let tamanhoTotal = length pontos   
   let aleatorios = geraVetorValoresAleatorios (toInt seed) tamanhoTotal --gera o vetor de numeros aleatorios

   let foldes = geraFolds (toInt qtdFoldes) aleatorios --gera os folds
   let foldesDatasets = criaTodosDatasets foldes aleatorios pontos --gera todos os datasets para cada fold
   let padronizado = map padronizar foldesDatasets --padroniza todos os datasets

   let centroides = centroidesParaFoldes padronizado --calcula os centroides
   --gera os resultados para cada metodo
   let resultadosCentroides = predicoesPraFolde centroides padronizado
   let resultadosKNN = map (aplicaKNNpara1Folde (toInt k)) padronizado
   let resultadosVizinhos = map (aplicaKNNpara1Folde 1) padronizado
   
   --gera as acuracias para cada metodo
   let acuraciaKNN = calculaAcuraciaTodosFoldes resultadosKNN (retornaReais (map teste padronizado))
   let acuraciaVizinhos = calculaAcuraciaTodosFoldes resultadosVizinhos (retornaReais (map teste padronizado))
   let acuraciaCentroides = calculaAcuraciaTodosFoldes resultadosCentroides (retornaReais (map teste padronizado))

   --gera as matrizes de confusao de cada metodo
   let matrizKNN = geraMatrizConfusaoFolde resultadosKNN (retornaReais (map teste padronizado)) classes
   let matrizVizinhos = geraMatrizConfusaoFolde resultadosVizinhos (retornaReais (map teste padronizado)) classes
   let matrizCentroides = geraMatrizConfusaoFolde resultadosCentroides (retornaReais (map teste padronizado)) classes

   --mostra na tela os resultados de acuracia e desvio padrao
   printf "Acuracia(vizinhos): %.2f%%\n" (100 * sum(acuraciaVizinhos) / fromIntegral (length acuraciaVizinhos))
   printf "Desvio-Padrao(vizinho): %.2f%%\n" (desvioPadraoAcuracias acuraciaVizinhos * 100)
   printf "Acuracia(centroide): %.2f%%\n" (100 * sum(acuraciaCentroides) / fromIntegral (length acuraciaCentroides))
   printf "Desvio-Padrao(centroide): %.2f%%\n" (desvioPadraoAcuracias acuraciaCentroides * 100)
   printf "Acuracia(k-vizinhos): %.2f%%\n" (100 * sum(acuraciaKNN) / fromIntegral (length acuraciaKNN))
   printf "Desvio-Padrao(k-vizinhos): %.2f%%\n" (desvioPadraoAcuracias acuraciaKNN * 100)

   --gera o documento de saida com as matrizes formatadas
   geraDocumentoSaida nomeArquivoSaida matrizVizinhos matrizCentroides matrizKNN

   return ()