(print "exercicio 1:")
(comment " 1. Utilizando  a  linguagem  Clojure,  crie  uma  função  chamada  ultimo  que  receba  uma  lista  e devolva o último elemento desta lista sem usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.  ")

(defn ultimo []
   (def seq1 (seq [1 2 3 4 5 6]))
   (println (take-last 1 seq1)))
(ultimo)

(comment " 2. Utilizando a linguagem Clojure, crie uma função chamada penultimo que receba uma lista e  devolva  o  penúltimo  elemento  desta  lista  usar as  funções  já  prontas  e disponíveis para esta mesma finalidade na linguagem Clojure.   ")
(print "exercicio 2:")
(defn ss []
   (def seq2 (seq [1 2 3 4 5 6 7 8]))
   (println (second(reverse seq2))))
(ss)


(comment " 3. Utilizando a linguagem Clojure, crie uma função chamada elementoN que receba uma lista e um inteiro N e devolva o  elemento que  está na  posição N desta lista usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.  ")
(print "exercicio 3:")
(defn elementoN []
   (def seq3 (seq [1 2 3 4 5 6 7 8 9 10]))
   (println (nth seq3 9)))
(elementoN)

(comment " 4. Utilizando  a  linguagem Clojure,  crie  uma função  chamada  inverso  que  receba uma  lista  e devolva esta lista com as posições dos elementos invertidas. Por exemplo recebe [1,2,3] e devolve [3,2,1]. Sem usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.  ")
(print "exercicio 4:")
(defn inverso []
   (def seq4 (seq [1 2 3 4 5 6 7 8 9 10]))
   (println (reverse seq4)))
(inverso)

(comment " 5. Utilizando a  linguagem Clojure, crie uma função chamada  mdc que receba  dois inteiros e devolve o mínimo divisor comum entre eles.  Sem usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure. ")
(print "exercicio 5:")
(defn mdc[]
	'(5 10)
	(fn gcd[a b] 
	  (if (= b 0)
	    a
	    (recur b (mod a b))
	))
  )
(mdc)
