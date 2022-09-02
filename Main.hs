{- ALUNO: Rafael Vitagliano Tannenbaum Nuñez -}

import Data.Char

{-
1. Escreva uma função chamada fatorialn que usando o operador range e a função foldr devolva o fatorial de n.
-}

fatorialn :: Int -> Int
fatorialn n = foldr (*) 1 [1 .. n]

{-
2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos reais listados.
-}

quadradoReal :: [Float] -> [Float]
quadradoReal [] = []
quadradoReal x = map (^ 2) x

{-
3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de palavras e devolve uma lista com o comprimento de cada uma destas palavras.
-}

comprimentoPalavras :: [[x]] -> [Int]
comprimentoPalavras y = map length y

{-
4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior número entre 0 e 100000 que seja divisivel por 29.
-}

maiorMultiploDe29 :: Int
maiorMultiploDe29 = maximum (filter y [0 .. 99999])
  where
    y x = x `mod` 29 == 0

--maiorMultiploDe29 y = maximum filter (>29) y

{-
5. Usando a função filter escreva uma função, chamada maiorMultiploDe que recebe um inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro.
-}

maiorMultiploDe :: Int -> Int
maiorMultiploDe z = maximum (filter y [0 .. 99999])
  where
    y x = x `mod` z == 0

{-
6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva
a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠 = 1^2 + 2^2 + 3^2 + 4^2 . . . + 𝑛^2
-}

somaQuadrados :: Int -> Int
somaQuadrados n = foldr (+) 0 (map (^ 2) [1 .. n])

{-
7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o comprimento (cardinalidade) de uma lista dada.
-}

comprimento :: [Int] -> Int
comprimento x = foldl (+) 0 (map (\n -> 1) x)

{-
8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos.
-}

{- Os exemplos estão depois do Main-}

{- Função usada em um dos exemplos de curry-}
subtracao :: (Int, Int) -> Int
subtracao (a,b) = a - b

main = do
  putStrLn $ "Func. 1; entrada: 5; resultado = " ++ show (fatorialn 5)
  putStrLn $ "Func. 2; entrada:  [12.9,1,2.2,3,-4,-5,-6]; resultado = " ++ show (quadradoReal [12.9, 1, 2.2, 3, -4, -5, -6])
  putStrLn $ "Func. 3; entrada:  [rafael, nunez, aluno, computacao]; resultado = " ++ show (comprimentoPalavras ["rafael", "nunez", "aluno", "computacao"])
  putStrLn $ "Func. 4; entrada: não recebe valor de entrada; resultado = " ++ show (maiorMultiploDe29)
  putStrLn $ "Func. 5; entrada: 20; resultado = " ++ show (maiorMultiploDe 20)
  putStrLn $ "Func. 6; entrada: 10; resultado = " ++ show (somaQuadrados 10)
  putStrLn $ "Func. 7; entrada: [1..5]; resultado = " ++ show (comprimento [1..5])

  putStrLn $ "\nFunc. 8 - flip; entrada: Nunez Rafael ; resultado = " ++ show (flip (++) "Nunez" "Rafael ")
  putStrLn $ "Func. 8 - flip; entrada: 3 7, ; resultado = " ++ show (flip (*) 3 7)

  putStrLn $ "\nFunc. 8 - ord; entrada: 'r'; resultado = " ++ show (ord 'r')
  putStrLn $ "Func. 8 - ord; entrada: 'barra + r'; resultado = " ++ show (ord '\r')

  putStrLn $ "\nFunc. 8 - max; entrada: 15.4 22.9; resultado = " ++ show (max 15.4 22.9)
  putStrLn $ "Func. 8 - max; entrada: 11 6; resultado = " ++ show (max 11 6)

  putStrLn $ "\nFunc. 8 - min; entrada: 15.4 22.9; resultado = " ++ show (min 15.4 22.9)
  putStrLn $ "Func. 8 - min; entrada: 19 10; resultado = " ++ show (min 19 10)

  putStrLn $ "\nFunc. 8 - curry; entrada: 1 2 ; resultado = " ++ show (curry fst 1 2)
  putStrLn $ "Func. 8 - curry; entrada: 20 12 ; resultado = " ++ show (curry subtracao 20 12)

  putStrLn $ "\nFunc. 8 - uncurry; entrada: (1,2) ; resultado = " ++ show (uncurry (+) (1,2))
  putStrLn $ "Func. 8 - uncurry; entrada: (8,2) ; resultado = " ++ show (uncurry mod (8,2))