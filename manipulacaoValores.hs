{- Loraine de Fatima Mendes-}

{-1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada.  -}
soma1::Integer->Integer
soma1 x = x + 1

{-2. Escreva  uma  função  chamada  sempre  que,  não  importando  o  valor  de  entrada,  devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo. -}
sempre::a->Integer
sempre x = 0

{-3. Escreva  uma  função  chamada  treco  que  receba  três  valores  em  ponto  flutuantes  com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro. -}
treco::Double->Double->Double->Double
treco x y z = (x + y) * z

{-4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiros. -}
resto::Integer->Integer->Integer
resto x y = x `mod` y

{-5. Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores  monetários. -}
maiorD::Double->Double->Double
maiorD x y 
      | x > y = x
      |otherwise = y 

precoMaior::Double->Double->Double->Double->Double
precoMaior x y w z = maiorD x (maiorD y (maiorD w z)) 

{-6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto 
de dois números inteiros for ímpar.-}
impar::Integer->Integer->Bool
impar x y 
      |even(x * y) = False
      |otherwise = True

{-Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟∷(𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva 
uma função em Haskell que devolva a soma dos componentes de um par de inteiros.-}
soma::Integer->Integer->Integer
soma x y = x + y

{--7. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado 
da equação 𝑥2 +𝑦
2 +𝑧.--}
eq::Double->Double->Double->Double
eq x y z = (x*x) + (y/2.0) + z

{-8. Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima 
um  diagnóstico  de  obesidade,  segundo  a  tabela  que  pode  ser  encontrada  no  link: -}
imc::Double->Double->Double
imc x y 
      |x > y = x / (y * y)
      |otherwise = y / (x * x)

diagnostico::Double->Double->String
diagnostico x y
      |imc x y < 17.0 = "Muito abaixo do peso!"
      |imc x y >= 17.0 && imc x y < 18.5 = "Abaixo do peso"
      |imc x y >= 18.5 && imc x y < 25.0 = "Peso normal"
      |imc x y >= 25.0 && imc x y < 30.0 = "Sobrepeso"
      |imc x y >= 30.0 && imc x y < 35.0 = "Obesidade leve"
      |imc x y >= 35.0 && imc x y < 40.0 = "Obesidade severa"
      |imc x y >= 40.0 = "Obesidade morbida"


{--Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o 
ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:  --}
bissexto::Integer->Bool
bissexto x = ((x `mod` 4 == 0) && (x `mod` 100 /= 0)) || x `mod` 400 == 0

main = do
    print ("F1: entrada: 5; resultado: " ++ show (soma1 1))
    print("F2: entrada: teste; resultado: " ++ show (sempre "teste"))
    print("F3: entrada: 1.1 2.2 e 3.3; resultado" ++ show(treco 1.1 2.2 3.3))
    print("F4: entrada: 1 2; resultado:" ++ show (resto 10 3))
    print("F5: entrada: 11.1 12.12 13.13 14.14; resultado: " ++ show (precoMaior 11.1 12.12 13.13 14.14))
    print("F6: entrada: 1 e 3; resultado: " ++ show (impar 1 3))
    print("F6.2: entrada: 2 7; resultado: " ++ show (soma 4 5))
    print("F7: entrada: 1.2 1.3 1.4 1.5; resultado:  " ++ show (eq 1.2 1.3 1.4))
    print("F8: entrada: 1.56 55; resultado: " ++ show (diagnostico  1.56 55))
    print("F9: entrada: 1990; resultado: " ++ show (bissexto 1990))
    print("F9: entrada: 2000; resultado: " ++ show (bissexto 2000))
