(ns tp.core-test
  (:require [clojure.test :refer :all]
            [tp.core :refer :all]))

(deftest verificar-parentesis-1-test
  (testing "Un parentesis"
    (is (= 1 (verificar-parentesis "(hola 'mundo")))))

(deftest verificar-parentesis-2-test
  (testing "Parentesis desbalanceados por uno"
    (is (= -1 (verificar-parentesis "(hola '(mundo)))")))))

(deftest verificar-parentesis-3-test
  (testing "Parentesis desbalanceados por más de uno"
    (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7)")))))

(deftest verificar-parentesis-4-test
  (testing "Parentesis desbalanceados por más de uno"
    (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")))))

(deftest verificar-parentesis-5-test
  (testing "Parentesis balanceados"
    (is (= 0 (verificar-parentesis "(hola '(mundo) )")))))