{-# LANGUAGE LambdaCase #-}
{--1. Escreva  uma  função  para  o  cálculo  dos  números  da  sequência  de  Fibonacci,  utilizando 
Haskell.--}
fibonacci = map fst (iterate f (0,1)) where f (x,y) = (y,x+y)
resultadoFibonacci = take 10 fibonacci

{--2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor  Comum  (MDC)  de  Euclides  publicado  por  volta do  ano 300 AC.
Podemos simplificar  este algoritmo  dizendo  que  dados  dois  inteiros  A  e  B,  o  MDC  entre  eles  será dado  pelo  valor absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva uma  função  para  o  cálculo  do  MDC  entre  dois  números  inteiros  positivos,  usando  o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell.    --}
calculoMDC :: Integer -> Integer -> (Integer, Integer, Integer)
calculoMDC 0 b = (b, 0, 1)
calculoMDC a b = let (g, s, t) = calculoMDC (b `mod` a) a
       in (g, t - (b `div` a) * s, s)

{--3.  Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste  número.  Exemplo:  dado  1234  a  função  deverá  devolver  10.  Utilizando  Haskell  e recursividade. --}
somaDigitos :: [Int] -> Int
somaDigitos [] = 0
somaDigitos (x:xs) = x + somaDigitos xs

{--4. Escreva  uma  função  que  devolva  ,a  soma  de  todos  os  números  menores  que  10000  que sejam múltiplos de 3 ou 5. 
fonte: https://codereview.stackexchange.com/questions/161439/project-euler-1-sum-of-multiples-of-3-or-5-less-than-1000-in-haskell--}
divide x y = x `mod` y == 0
somaMenorQueCem = somaDigitos([x | x<-[1..9999], any (divide x) [3,5]])

{--5. Escreva  uma  função que,  recebendo  uma  lista  de  inteiros,  apresente  a  diferença  entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade.--}
{--sqTD :: Integer->Integer->Integer
sqTD n m
sqN = n^2 
sqM = m^2
let sqTD = sqN + sqM--}


{--6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números 
primos menores que um determinado inteiro dado.  --}

{--7. Nem  só  de  Fibonacci  vivem  os  exemplos  de  recursão.  Escreva  uma  função  que  devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado. 
https://hackage.haskell.org/package/arithmoi-0.4.1.1/docs/src/Math-NumberTheory-Lucas.html
 xn = xn − 1 + xn − 2.--}
fibonacciInverso = map fst (iterate f (2,1)) where f (x,y) = (y,x+y)
sequenciaLucas = take 10 fibonacciInverso
foo n sequenciaLucas = filter (> 5) where n(sequenciaLucas) = (filter (>5))

{--8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] 
devolva [3,2,1].  --}
aoContrario :: [Int] -> [Int]
aoContrario =   \case
  [] -> []
  x : fibonacciInverso -> aoContrario fibonacciInverso ++ [x]

{--9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o 
produto destes valores sem usar o operador de multiplicação. --}
somaRecursiva :: Integer->Integer->Integer
somaRecursiva x 1 = x
somaRecursiva x y = x + somaRecursiva x (y-1)

{-- 10. Escreva uma função chamada comprimento que receba uma lista de  inteiros e devolva o 
comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule 
o comprimento de uma lista. --}
nome xs = sum [1 | _ <- xs]

main = do
  print ("F1: entrada: 10; resultado: " ++ show (resultadoFibonacci))
  print ("F2: entrada: 10 2; resultado: " ++ show (calculoMDC 10 2))
  print ("F3: entrada: 1, 2, 3, 4, 5; resultado: " ++ show (calculoMDC 10 2))
  print ("F3: entrada: 1, 2, 3, 4; resultado: " ++ show (somaDigitos(1:2:3:4 :[])))
  print ("F4: entrada: 0..9999; resultado: " ++ show (somaMenorQueCem))
  print ("F5: Nao fiz")
  print ("F6: Nao fiz")
  print ("F7: entrada: 10; resultado: " ++ show (sequenciaLucas))
  print ("F8: entrada: 1, 2, 3; resultado: " ++ show(reverse [1, 2, 3]))
  print ("F9: entrada: 2, 2; resultado: " ++ show(somaRecursiva 2 2))
  print ("F10: entrada: 1, 2, 3, 4, 5; resultado: " ++ show(nome(1:2:3:4:5 :[])))
