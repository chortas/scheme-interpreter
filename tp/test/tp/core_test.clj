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