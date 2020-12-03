import Utils
import Classificadores
import System.IO
import Text.Printf (printf)

main :: IO ()
main = do
   --Pega os dados de entrada necessarios para o programa
   --nomeArquivoEntrada <- pegaInfo "Forneca o nome do arquivo de entrada: "
   --nomeArquivoSaida <- pegaInfo "Forneca o nome do arquivo de saida: "
   --porcentagem <- pegaInfo "Forneca o percentual de exemplos de teste: "
   --seed <- pegaInfo "Forneca o valor da semente para geracao randomizada: "
   let seed = 42
   
   let k = 4
   let arquivoEntrada = "iris.csv"
   content <- readFile arquivoEntrada

   let linhas = map split $ lines content
   let pontos = map criaPonto linhas 
   let tamanhoTotal = length pontos
   let indicesLinhas = [0..(tamanhoTotal - 1)]
   let foldes = geraFolds k indicesLinhas
   let foldesDatasets = criaTodosDatasets foldes indicesLinhas pontos k
   
   print $ length (map show (teste (foldesDatasets !! 0)))

   return ()