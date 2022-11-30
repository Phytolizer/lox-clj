(ns lox.ast-printer-test
  (:refer-clojure :exclude [print])
  (:require [clojure.test :refer [deftest is testing]]
            [lox.ast-printer :refer [print]]
            [lox.expr :refer [->BinaryExpr ->GroupingExpr ->LiteralExpr
                              ->UnaryExpr]]
            [lox.token :refer [->Token]]))

(deftest example
  (testing "print example expr"
    (is (=
         "(* (- 123) (group 45.67))"
         (print (->BinaryExpr
                 (->UnaryExpr
                  (->Token :minus "-" nil 1)
                  (->LiteralExpr 123))
                 (->Token :star "*" nil 1)
                 (->GroupingExpr
                  (->LiteralExpr 45.67))))))))
