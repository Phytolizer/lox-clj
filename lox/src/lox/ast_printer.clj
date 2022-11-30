(ns lox.ast-printer
  (:refer-clojure :exclude [print])
  (:require [lox.helpers :refer [accept]]))

(defn print [expr]
  (letfn [(parenthesize [name exprs]
            (apply str (flatten
                        (list "(" name
                              (for [expr exprs] (str " " (print expr)))
                              ")"))))
          (visit-binary-expr [_ expr]
            (parenthesize
             (.lexeme (:operator expr))
             (list (:left expr)
                   (:right expr))))
          (visit-grouping-expr [_ expr]
            (parenthesize
             "group"
             (list (:expression expr))))
          (visit-literal-expr [_ expr]
            (case (:value expr)
              nil "nil"
              (.toString (:value expr))))
          (visit-unary-expr [_ expr]
            (parenthesize
             (.lexeme (:operator expr))
             (list (:right expr))))]
    (let [fns {:binary visit-binary-expr
               :grouping visit-grouping-expr
               :literal visit-literal-expr
               :unary visit-unary-expr}]
      (accept expr fns nil))))
