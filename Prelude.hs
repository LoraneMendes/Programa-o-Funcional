{--1. Escreva  uma  função  chamada  fatorialn  que  usando  o  operador  range  e  a  função  foldr devolva o fatorial de n. --}
fatorialn  n  =  foldr (*) 1 [1 .. n]

{--2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de 
números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos 
reais listados. --}
quadradoReal :: [Int] -> [Int]
quadradoReal x = map (^2) x  

{--3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de 
palavras e devolve uma lista com o comprimento de cada uma destas palavras. --}
comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras = map length

{--4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior 
número entre 0 e 100000 que seja divisivel por 29. --}
--fibonacciInverso = map fst (iterate f (0,1))
--let {largestDivisible :: (Integral a) -> a
maiorMultiploDe29 :: (Integral a) => a
maiorMultiploDe29 = head (filter p [1, 100000..]) where p x = x `mod` 29 == 0

{--5. Usando  a  função  filter  escreva  uma  função,  chamada  maiorMultiploDe que  recebe  um 
inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro. --}
maiorMultiploDe = head (filter p [1, 100000..]) where p x = x `mod` 5 == 0

{--6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva 
a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De 
tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠=12 +22 +32 +42...+𝑛2. --}
somaQuadrados n = foldr (\x y -> x*x + y) 0 [1..n]

{--7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o 
comprimento (cardinalidade) de uma lista dada.  --}
mylength :: [a] -> Int
mylength l = foldr f 0 l
    where f x y = y+1

{--8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso 
das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada 
uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos. --}
pesquisa1 x y = x - y
pesquisa2 x y = x + y
pesquisa3 :: (Num a, Ord a) => a -> Ordering  
pesquisa3 = compare 100 
pesquisa4 :: (Num a, Ord a) => a -> Ordering  
pesquisa4 = compare 100

-- curry
add' :: Int -> (Int -> Int)
add' a b = a + b
addCinco :: Int -> Int
addCinco a = add' 5 a

menos' :: Int -> (Int -> Int)
menos' c d = c - d
menosCinco :: Int -> Int 
menosCinco c = menos' 5 c

-- uncurry
uncurryAdd' :: Int -> Int -> Int -> Int
uncurryAdd' = (\a -> (\b -> (\c -> a + b + c)))
uncurryAddCinco :: Int -> Int
uncurryAddCinco a = uncurryAdd' 1 2 3

uncDois' :: Int -> Int -> Int -> Int
uncDois' = (\a -> (\b -> (\c -> a - b - c)))
uncD :: Int -> Int
uncD a = uncDois' 1 2 3


main = do
  print ("F1: entrada: 5, ; resultado: " ++ show(fatorialn 5))
  print ("F2: entrada: (-2),1,2,4, ; resultado: " ++ show(quadradoReal[-2,1,2,4]))
  print ("F2: entrada: (-2),1,2,4, ; resultado: " ++ show(quadradoReal[-2..4]))
  print ("F3: entrada: exercicio,numero, tres; resultado: " ++ show(comprimentoPalavras ["exercicio","numero","tres"]))
  print ("F4: entrada: 1..100000; resultado: " ++ show (maiorMultiploDe29))
  print ("F5: entrada: 5; resultado: " ++ show (maiorMultiploDe))
  print ("F6: entrada: 5; resultado: " ++ show (somaQuadrados 2))
  print ("F7: entrada: 1,2,3,4,5,6,7,8,910; resultado: " ++ show (mylength(1:2:3:4:5:6:7:8:910 :[])))
  print ("F8 - flip: entrada: 3,1; resultado: " ++ show (flip pesquisa1 3 1))
  print ("F8 - flip: entrada: 3,1; resultado: " ++ show (flip pesquisa2 3 1))
  print ("F8 - ord: entrada: 100; resultado: " ++ show (pesquisa3 100))
  print ("F8 - ord: entrada: 1000; resultado: " ++ show (pesquisa3 1000))
  print("F8 - max: entrada: 1.7 e 0.9; resultado: " ++ show (max 1.7 0.9)) --pesquisa 5
  print("F8 - max: entrada: 0.9 e 1.7; resultado: " ++ show (max 0.9 1.7)) --pesquisa 5
  print("F8 - min: entrada: 0.9 e 1.7; resultado: " ++ show (min 0.9 1.7)) --pesquisa 6
  print("F8 - min: entrada: 0.9 e 1.7; resultado: " ++ show (min 0.9 1.7)) --pesquisa 6
  print("F8 - currying: entrada: 10; resultado: " ++ show (addCinco 10)) --pesquisa 8
  print("F8 - currying: entrada: 10; resultado: " ++ show (menosCinco 10))
  print("F8 - uncurrying: entrada: 1 2 3; resultado: " ++ show (uncurryAddCinco 3))
  print("F8 - uncurrying: entrada: 1 2 3; resultado: " ++ show (uncD 3))
