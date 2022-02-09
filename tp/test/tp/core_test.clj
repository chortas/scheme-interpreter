(ns tp.core-test
  (:require [clojure.test :refer :all]
            [tp.core :refer :all]))

(deftest leer-entrada-test
  (testing "Funcion leer-entrada"
    (is (= "(hola mundo)" (with-in-str "(hola\nmundo)" (leer-entrada))))
    (is (= "123" (with-in-str "123" (leer-entrada))))))

(deftest verificar-parentesis-test
  (testing "Funcion verificar-parentesis"
    (is (= 1 (verificar-parentesis "(hola 'mundo")))
    (is (= -1 (verificar-parentesis "(hola '(mundo)))")))
    (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7)")))
    (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")))
    (is (= 0 (verificar-parentesis "(hola '(mundo) )")))))

(deftest buscar-test
  (testing "Funcion buscar"
    (is (= 1 (buscar 'a '(a 1 b 2 c 3 d 4 e 5))))
    (is (= 2 (buscar 'b '(a 1 b 2 c 3 d 4 e 5))))
    (is (= 3 (buscar 'c '(a 1 b 2 c 3 d 4 e 5))))
    (is (= 4 (buscar 'd '(a 1 b 2 c 3 d 4 e 5))))
    (is (= 5 (buscar 'e '(a 1 b 2 c 3 d 4 e 5))))
    (is (= "(;ERROR: unbound variable: f)" (str (buscar 'f '(a 1 b 2 c 3 d 4 e 5)))))))

(deftest error?-test
  (testing "Funcion error?"
    (is (= true (error? (list (symbol ";ERROR:") 'mal 'hecho))))
    (is (= false (error? (list 'mal 'hecho))))
    (is (= true (error? (list (symbol ";WARNING:") 'mal 'hecho))))))

(deftest actualizar-amb-test
  (testing "Funcion actualizar-amb"
    (is (= '(a 1 b 2 c 3 d 4) (actualizar-amb '(a 1 b 2 c 3) 'd 4)))
    (is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b 4)))
    (is (= '(a 1 b 2 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho))))
    (is (= '(b 7) (actualizar-amb () 'b 7)))))

(deftest proteger-bool-en-str-test
  (testing "Funcion proteger-bool-en-str"
    (is (= "(or %F %f %t %T)" (proteger-bool-en-str "(or #F #f #t #T)")))
    (is (= "(and (or %F %f %t %T) %T)" (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))
    (is (= "" (proteger-bool-en-str "")))))

(deftest restaurar-bool-test
  (testing "Funcion restaurar-bool"
    (is (= (list (symbol "and") (list (symbol "or") (symbol "#F") (symbol "#f") (symbol "#t") (symbol "#T")) (symbol "#T")) (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))))
    (is (= (list (symbol "and") (list (symbol "or") (symbol "#F") (symbol "#f") (symbol "#t") (symbol "#T")) (symbol "#T")) (restaurar-bool (read-string "(and (or %F %f %t %T) %T)"))))))

(deftest igual?-test
  (testing "Funcion igual?"
    (is (= true (igual? 'if 'IF)))
    (is (= true (igual? 'if 'if)))
    (is (= true (igual? 'IF 'IF)))
    (is (= false (igual? 'IF "IF")))
    (is (= false (igual? 6 "6")))))

(deftest fnc-append-test
  (testing "Funcion fnc-append"
    (is (= '(1 2 3 4 5 6 7) (fnc-append '((1 2) (3) (4 5) (6 7)))))
    (is (= "(;ERROR: append: Wrong type in arg 3)" (str (fnc-append '((1 2) 3 (4 5) (6 7))))))
    (is (= "(;ERROR: append: Wrong type in arg A)" (str (fnc-append '((1 2) A (4 5) (6 7))))))))

(deftest fnc-equal-test
  (testing "Funcion fnc-equal"
    (is (= (symbol "#t") (fnc-equal? ())))
    (is (= (symbol "#t") (fnc-equal? '(A))))
    (is (= (symbol "#t") (fnc-equal? '(A a))))
    (is (= (symbol "#t") (fnc-equal? '(A a))))
    (is (= (symbol "#t") (fnc-equal? '(A a A))))
    (is (= (symbol "#t") (fnc-equal? '(A a A a))))
    (is (= (symbol "#f") (fnc-equal? '(A a A B))))
    (is (= (symbol "#t") (fnc-equal? '(1 1 1 1))))
    (is (= (symbol "#f") (fnc-equal? '(1 1 2 1))))))

(deftest fnc-read-test
  (testing "Funcion fnc-read"
    (is (= "(hola mundo)" (str (with-in-str "(hola\nmundo)" (fnc-read ())))))
    (is (= "(;ERROR: read: Use of I/O ports not implemented)" (str (fnc-read '(1)))))
    (is (= "(;ERROR: Wrong number of args given #<primitive-procedure read>)" (str (fnc-read '(1 2)))))
    (is (= "(;ERROR: Wrong number of args given #<primitive-procedure read>)" (str (fnc-read '(1 2 3)))))))

(deftest fnc-sumar-test
  (testing "Funcion fnc-sumar"
    (is (= 0 (fnc-sumar ())))
    (is (= 3 (fnc-sumar '(3))))
    (is (= 7 (fnc-sumar '(3 4))))
    (is (= 12 (fnc-sumar '(3 4 5))))
    (is (= 18 (fnc-sumar '(3 4 5 6))))
    (is (= "(;ERROR: +: Wrong type in arg1 A)" (str (fnc-sumar '(A 4 5 6)))))
    (is (= "(;ERROR: +: Wrong type in arg2 A)" (str (fnc-sumar '(3 A 5 6)))))
    (is (= "(;ERROR: +: Wrong type in arg2 A)" (str (fnc-sumar '(3 4 A 6)))))))

(deftest fnc-restar-test
  (testing "Funcion fnc-restar"
    (is (= "(;ERROR: -: Wrong number of args given)" (str (fnc-restar ()))))
    (is (= -3 (fnc-restar '(3))))
    (is (= -1 (fnc-restar '(3 4))))
    (is (= -6 (fnc-restar '(3 4 5))))
    (is (= -12 (fnc-restar '(3 4 5 6))))
    (is (= "(;ERROR: -: Wrong type in arg1 A)" (str (fnc-restar '(A 4 5 6)))))
    (is (= "(;ERROR: -: Wrong type in arg2 A)" (str (fnc-restar '(3 A 5 6)))))
    (is (= "(;ERROR: -: Wrong type in arg2 A)" (str (fnc-restar '(3 4 A 6)))))))

(deftest fnc-menor-test
  (testing "Funcion fnc-menor"
    (is (= (symbol "#t") (fnc-menor ())))
    (is (= (symbol "#t") (fnc-menor '(1))))
    (is (= (symbol "#t") (fnc-menor '(1 2))))
    (is (= (symbol "#t") (fnc-menor '(1 2 3))))
    (is (= (symbol "#t") (fnc-menor '(1 2 3 4))))
    (is (= (symbol "#f") (fnc-menor '(1 2 2 4))))
    (is (= (symbol "#f") (fnc-menor '(1 2 1 4))))
    (is (= "(;ERROR: <: Wrong type in arg1 A)" (str (fnc-menor '(A 1 2 4)))))
    (is (= "(;ERROR: <: Wrong type in arg2 A)" (str (fnc-menor '(1 A 1 4)))))
    (is (= "(;ERROR: <: Wrong type in arg2 A)" (str (fnc-menor '(1 2 A 4)))))))

(deftest fnc-mayor-test
  (testing "Funcion fnc-mayor"
    (is (= (symbol "#t") (fnc-mayor ())))
    (is (= (symbol "#t") (fnc-mayor '(1))))
    (is (= (symbol "#t") (fnc-mayor '(2 1))))
    (is (= (symbol "#t") (fnc-mayor '(3 2 1))))
    (is (= (symbol "#t") (fnc-mayor '(4 3 2 1))))
    (is (= (symbol "#f") (fnc-mayor '(4 2 2 1))))
    (is (= (symbol "#f") (fnc-mayor '(4 2 1 4))))
    (is (= "(;ERROR: >: Wrong type in arg1 A)" (str (fnc-mayor '(A 3 2 1)))))
    (is (= "(;ERROR: >: Wrong type in arg2 A)" (str (fnc-mayor '(3 A 2 1)))))
    (is (= "(;ERROR: >: Wrong type in arg2 A)" (str (fnc-mayor '(3 2 A 1)))))))

(deftest fnc-mayor-o-igual-test
  (testing "Funcion fnc-mayor-o-igual"
    (is (= (symbol "#t") (fnc-mayor-o-igual ())))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(1))))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(2 1))))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(3 2 1))))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(4 3 2 1))))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(4 2 2 1))))
    (is (= (symbol "#f") (fnc-mayor-o-igual '(4 2 1 4))))
    (is (= "(;ERROR: >=: Wrong type in arg1 A)" (str (fnc-mayor-o-igual '(A 3 2 1)))))
    (is (= "(;ERROR: >=: Wrong type in arg2 A)" (str (fnc-mayor-o-igual '(3 A 2 1)))))
    (is (= "(;ERROR: >=: Wrong type in arg2 A)" (str (fnc-mayor-o-igual '(3 2 A 1)))))))

(deftest evaluar-escalar-test
  (testing "Funcion evaluar-escalar"
    (is (= '(32 (x 6 y 11 z "hola")) (evaluar-escalar 32 '(x 6 y 11 z "hola"))))
    (is (= '("chau" (x 6 y 11 z "hola")) (evaluar-escalar "chau" '(x 6 y 11 z "hola"))))
    (is (= '(11 (x 6 y 11 z "hola"))) (evaluar-escalar 'y '(x 6 y 11 z "hola")))
    (is (= '("hola" (x 6 y 11 z "hola"))) (evaluar-escalar 'z '(x 6 y 11 z "hola")))
    (is (= ";ERROR: unbound variable: n") (str (nth (evaluar-escalar 'n '(x 6 y 11 z "hola")) 0)))
    (is (= '(x 6 y 11 z "hola")) (nth (evaluar-escalar 'n '(x 6 y 11 z "hola")) 1))))

(deftest evaluar-define-test
  (testing "Funcion evaluar-define"
    (is (= (list (symbol "#<unspecified>") '(x 2)) (evaluar-define '(define x 2) '(x 1))))
    (is (= (list (symbol "#<unspecified>") '(x 1 f (lambda (x) (+ x 1)))) (evaluar-define '(define (f x) (+ x 1)) '(x 1))))
    (is (= (list (symbol "#<unspecified>") '(x 1 f (lambda (x y) (+ x y)))) (evaluar-define '(define (f x y) (+ x y)) '(x 1))))
    (is (= (list (symbol "#<unspecified>") '(x 1 f (lambda (x) (display x) (newline) (+ x 1)))) (evaluar-define '(define (f x) (display x) (newline) (+ x 1)) '(x 1))))
    (is (= "((;ERROR: define: missing or extra expression (define)) (x 1))" (str (evaluar-define '(define) '(x 1)))))
    (is (= "((;ERROR: define: missing or extra expression (define x)) (x 1))" (str (evaluar-define '(define x) '(x 1)))))
    (is (= "((;ERROR: define: missing or extra expression (define x 2 3)) (x 1))" (str (evaluar-define '(define x 2 3) '(x 1)))))
    (is (= "((;ERROR: define: missing or extra expression (define ())) (x 1))" (str (evaluar-define '(define ()) '(x 1)))))
    (is (= "((;ERROR: define: bad variable (define () 2)) (x 1))" (str (evaluar-define '(define () 2) '(x 1)))))
    (is (= "((;ERROR: define: bad variable (define 2 x)) (x 1))" (str (evaluar-define '(define 2 x) '(x 1)))))))

(deftest evaluar-if-test
  (testing "Funcion evaluar-if")
  (is (= '(2 (n 7)) (evaluar-if '(if 1 2) '(n 7))))
  (is (= '(7 (n 7)) (evaluar-if '(if 1 n) '(n 7))))
  (is (= '(7 (n 7)) (evaluar-if '(if 1 n 8) '(n 7))))
  (is (= (list (symbol "#<unspecified>") (list 'n 7 (symbol "#f") (symbol "#f"))) (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f")))))
  (is (= (list 8 (list 'n 7 (symbol "#f") (symbol "#f"))) (evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f")))))
  (is (= (list (symbol "#<unspecified>") (list 'n 9 (symbol "#f") (symbol "#f"))) (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))))
  (is (= "((;ERROR: if: missing or extra expression (if)) (n 7))" (str (evaluar-if '(if) '(n 7)))))
  (is (= "((;ERROR: if: missing or extra expression (if 1)) (n 7))" (str (evaluar-if '(if 1) '(n 7))))))

(deftest evaluar-or-test
  (testing "Funcion evaluar-or"
    (is (= (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or (list 'or) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
    (is (= (list (symbol "#t") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or (list 'or (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
    (is (= (list 7 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or (list 'or 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
    (is (= (list 5 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
    (is (= (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or (list 'or (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))))

(deftest evaluar-set-test
  (testing "Funcion evaluar-set"
    (is (= (list (symbol "#<unspecified>") (list (symbol "x") 1)) (evaluar-set! '(set! x 1) '(x 0))))
    (is (= "((;ERROR: unbound variable: x) ())" (str (evaluar-set! '(set! x 1) '()))))
    (is (= "((;ERROR: set!: missing or extra expression (set! x)) (x 0))" (str (evaluar-set! '(set! x) '(x 0)))))
    (is (= "((;ERROR: set!: missing or extra expression (set! x 1 2)) (x 0))" (str (evaluar-set! '(set! x 1 2) '(x 0)))))
    (is (= "((;ERROR: set!: bad variable 1) (x 0))" (str (evaluar-set! '(set! 1 2) '(x 0)))))))

; FINAL

(deftest fnc-multiplicar-test
  (testing "Funcion fnc-multiplicar"
    (is (= 1 (fnc-multiplicar ())))
    (is (= 3 (fnc-multiplicar '(3))))
    (is (= 12 (fnc-multiplicar '(3 4))))
    (is (= 60 (fnc-multiplicar '(3 4 5))))
    (is (= 360 (fnc-multiplicar '(3 4 5 6))))
    (is (= "(;ERROR: *: Wrong type in arg1 A)" (str (fnc-multiplicar '(A 4 5 6)))))
    (is (= "(;ERROR: *: Wrong type in arg2 A)" (str (fnc-multiplicar '(3 A 5 6)))))
    (is (= "(;ERROR: *: Wrong type in arg2 A)" (str (fnc-multiplicar '(3 4 A 6)))))))

(deftest fnc-dividir-test
  (testing "Funcion fnc-dividir"
    (is (= "(;ERROR: /: Wrong number of args given)" (str (fnc-dividir ()))))
    (is (= 1/3 (fnc-dividir '(3))))
    (is (= 3/4 (fnc-dividir '(3 4))))
    (is (= 3/20 (fnc-dividir '(3 4 5))))
    (is (= 1/40 (fnc-dividir '(3 4 5 6))))
    (is (= 0 (fnc-dividir '(0 4 5 6))))
    (is (= "(;ERROR: /: Wrong type in arg1 A)" (str (fnc-dividir '(A 4 5 6)))))
    (is (= "(;ERROR: /: Wrong type in arg2 A)" (str (fnc-dividir '(3 A 5 6)))))
    (is (= "(;ERROR: /: Wrong type in arg2 A)" (str (fnc-dividir '(3 4 A 6)))))
    (is (= "(;ERROR: /: Zero Division)" (str (fnc-dividir '(0)))))
    (is (= "(;ERROR: /: Zero Division)" (str (fnc-dividir '(3 0)))))
    (is (= "(;ERROR: /: Zero Division)" (str (fnc-dividir '(3 0 3)))))))

(deftest fnc-igual-test
 (testing "Funcion fnc-igual"
   (is (= (symbol "#t") (fnc-igual ())))
   (is (= (symbol "#t") (fnc-igual '(1))))
   (is (= (symbol "#t") (fnc-igual '(2 2))))
   (is (= (symbol "#f") (fnc-igual '(3 2))))
   (is (= "(;ERROR: =: Wrong type in arg1 A)" (str (fnc-igual '(A 2 2 2)))))
   (is (= "(;ERROR: =: Wrong type in arg2 A)" (str (fnc-igual '(2 A 2 2)))))
   (is (= "(;ERROR: =: Wrong type in arg2 A)" (str (fnc-igual '(2 2 A 2)))))))

(deftest fnc-menor-o-igual-test
 (testing "Funcion fnc-menor-o-igual"
   (is (= (symbol "#t") (fnc-menor-o-igual ())))
   (is (= (symbol "#t") (fnc-menor-o-igual '(1))))
   (is (= (symbol "#t") (fnc-menor-o-igual '(1 2))))
   (is (= (symbol "#t") (fnc-menor-o-igual '(1 2 3))))
   (is (= (symbol "#t") (fnc-menor-o-igual '(1 3 3 4))))
   (is (= (symbol "#t") (fnc-menor-o-igual '(1 2 2 4))))
   (is (= (symbol "#f") (fnc-menor-o-igual '(1 2 4 1))))
   (is (= "(;ERROR: <=: Wrong type in arg1 A)" (str (fnc-menor-o-igual '(A 1 2 3)))))
   (is (= "(;ERROR: <=: Wrong type in arg2 A)" (str (fnc-menor-o-igual '(1 A 2 3)))))
   (is (= "(;ERROR: <=: Wrong type in arg2 A)" (str (fnc-menor-o-igual '(1 2 A 3)))))))

(deftest fnc-eq-test
 (testing "Funcion fnc-eq"
   (is (= (symbol "#t") (fnc-eq? ())))
   (is (= (symbol "#t") (fnc-eq? '(A))))
   (is (= (symbol "#t") (fnc-eq? '(a a))))
   (is (= (symbol "#f") (fnc-eq? '(hello goodbye))))
   (is (= (symbol "#f") (fnc-eq? (list (list 1 2) (list 1 2)))))
   (is (= (symbol "#t") (fnc-eq? (list (list) (list)))))
   (is (= (symbol "#f") (fnc-eq? '(2.5 2.5))))
   (is (= (symbol "#t") (fnc-eq? '(a a a))))
   (is (= (symbol "#f") (fnc-eq? '(a a b))))))

(deftest evaluar-and-test
 (testing "Funcion evaluar-and"
   (is (= (list (symbol "#t") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-and (list 'and) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
   (is (= (list (symbol "#t") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-and (list 'and (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
   (is (= (list 7 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-and (list 'and 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
   (is (= (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-and (list 'and (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
   (is (= (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-and (list 'and (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
   (is (= (list 7 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-and (list 'and (symbol "#t") 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
   (is (= (list (symbol "#t") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-and (list 'and (symbol "#t") 7 (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))))

(deftest evaluar-let-test
 (testing "Funcion evaluar-let"
   (is (= (list 30 (list 'x 1 '+ '+)) (evaluar-let (list 'let (list (list 'x 10) (list 'y 20)) (list '+ 'x 'y)) (list 'x 1 '+ '+))))
   (is (= (list 12 (list 'x 1 '+ '+)) (evaluar-let (list 'let (list (list 'x 10)) (list '+ 'x 2)) (list 'x 1 '+ '+))))
   (is (= (list 200 (list 'x 1 '* '*)) (evaluar-let (list 'let (list (list 'x 10) (list 'y 20)) (list '* 'x 'y)) (list 'x 1 '* '*))))))

(deftest evaluar-begin-test
 (testing "Funcion evaluar-begin"
   (is (= (list 6 (list 'x 5 '+ '+ 'set! 'set!)) (evaluar-begin (list 'begin (list 'set! 'x 5) (list '+ 'x 1)) (list 'x 0 '+ '+ 'set! 'set!))))))
