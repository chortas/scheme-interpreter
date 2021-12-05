(ns tp.core-test
  (:require [clojure.test :refer :all]
            [tp.core :refer :all]))

(deftest verificar-parentesis-test
  (testing "Funcion verificar-parentesis"
    (is (= 1 (verificar-parentesis "(hola 'mundo")))
    (is (= -1 (verificar-parentesis "(hola '(mundo)))")))
    (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7)")))
    (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")))
    (is (= 0 (verificar-parentesis "(hola '(mundo) )")))      
  )
)

(deftest buscar-test
  (testing "Funcion buscar"
    (is (= 1 (buscar 'a '(a 1 b 2 c 3 d 4 e 5)))) 
    (is (= 2 (buscar 'b '(a 1 b 2 c 3 d 4 e 5)))) 
    (is (= 3 (buscar 'c '(a 1 b 2 c 3 d 4 e 5)))) 
    (is (= 4 (buscar 'd '(a 1 b 2 c 3 d 4 e 5)))) 
    (is (= 5 (buscar 'e '(a 1 b 2 c 3 d 4 e 5)))) 
    (is (= "(;ERROR: unbound variable: f)" (str (buscar 'f '(a 1 b 2 c 3 d 4 e 5)))))
  )
)

(deftest error?-test
  (testing "Funcion error?"
    (is (= true (error? (list (symbol ";ERROR:") 'mal 'hecho)))) 
    (is (= false (error? (list 'mal 'hecho)))) 
    (is (= true (error? (list (symbol ";WARNING:") 'mal 'hecho)))) 
  )
)

(deftest actualizar-amb-test
  (testing "Funcion actualizar-amb"
    (is (= '(a 1 b 2 c 3 d 4) (actualizar-amb '(a 1 b 2 c 3) 'd 4)))
    (is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b 4)))
    (is (= '(a 1 b 2 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho)))) 
    (is (= '(b 7) (actualizar-amb () 'b 7)))
  )
)

(deftest proteger-bool-en-str-test
  (testing "Funcion proteger-bool-en-str"
    (is (= "(or %F %f %t %T)" (proteger-bool-en-str "(or #F #f #t #T)")))
    (is (= "(and (or %F %f %t %T) %T)" (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))
    (is (= "" (proteger-bool-en-str "")))
  )
)

(deftest restaurar-bool-test
  (testing "Funcion restaurar-bool"
    (is (= (symbol "(and (or #F #f #t #T) #T)") (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))))
    (is (= (symbol "(and (or #F #f #t #T) #T)") (restaurar-bool (read-string "(and (or %F %f %t %T) %T)"))))
  )
)

(deftest igual?-test
  (testing "Funcion igual?"
    (is (= true (igual? 'if 'IF))) 
    (is (= true (igual? 'if 'if))) 
    (is (= true (igual? 'IF 'IF))) 
    (is (= false (igual? 'IF "IF"))) 
    (is (= false (igual? 6 "6"))) 
  )
)

(deftest fnc-append-test
  (testing "Funcion fnc-append"
    (is (= '(1 2 3 4 5 6 7) (fnc-append '( (1 2) (3) (4 5) (6 7)))))
    (is (= "(;ERROR: append: Wrong type in arg 3)" (str (fnc-append '( (1 2) 3 (4 5) (6 7))))))
    (is (= "(;ERROR: append: Wrong type in arg A)" (str (fnc-append '( (1 2) A (4 5) (6 7))))))
  )
)

(deftest fnc-equal-test
  (testing "Funcion fnc-equal"
    (is (= "#t" (fnc-equal? ())))
    (is (= "#t" (fnc-equal? '(A))))
    (is (= "#t" (fnc-equal? '(A a))))
    (is (= "#t" (fnc-equal? '(A a))))
    (is (= "#t" (fnc-equal? '(A a A))))
    (is (= "#t" (fnc-equal? '(A a A a))))
    (is (= "#f" (fnc-equal? '(A a A B))))
    (is (= "#t" (fnc-equal? '(1 1 1 1))))
    (is (= "#f" (fnc-equal? '(1 1 2 1))))
  )
)

(deftest fnc-read-test
  (testing "Funcion fnc-read"
    (is (= "(;ERROR: read: Use of I/O ports not implemented)" (str (fnc-read '(1)))))
    (is (= "(;ERROR: Wrong number of args given #<primitive-procedure read>)" (str (fnc-read '(1 2)))))
    (is (= "(;ERROR: Wrong number of args given #<primitive-procedure read>)" (str (fnc-read '(1 2 3)))))
  )
)

(deftest fnc-sumar-test
  (testing "Funcion fnc-sumar"
    (is (= 0 (fnc-sumar ())))
    (is (= 3 (fnc-sumar '(3))))
    (is (= 7 (fnc-sumar '(3 4))))
    (is (= 12 (fnc-sumar '(3 4 5))))
    (is (= 18 (fnc-sumar '(3 4 5 6))))
    (is (= "(;ERROR: +: Wrong type in arg1 A)" (str (fnc-sumar '(A 4 5 6)))))
    (is (= "(;ERROR: +: Wrong type in arg2 A)" (str (fnc-sumar '(3 A 5 6)))))
    (is (= "(;ERROR: +: Wrong type in arg2 A)" (str (fnc-sumar '(3 4 A 6)))))
  )
)

(deftest fnc-restar-test
  (testing "Funcion fnc-restar"
    (is (= "(;ERROR: -: Wrong number of args given)" (str (fnc-restar ()))))
    (is (= -3 (fnc-restar '(3))))
    (is (= -1 (fnc-restar '(3 4))))
    (is (= -6 (fnc-restar '(3 4 5))))
    (is (= -12 (fnc-restar '(3 4 5 6))))
    (is (= "(;ERROR: -: Wrong type in arg1 A)" (str (fnc-restar '(A 4 5 6)))))
    (is (= "(;ERROR: -: Wrong type in arg2 A)" (str (fnc-restar '(3 A 5 6)))))
    (is (= "(;ERROR: -: Wrong type in arg2 A)" (str (fnc-restar '(3 4 A 6)))))
  )
)