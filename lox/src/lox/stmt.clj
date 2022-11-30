(ns lox.stmt)

(defn ->ExpressionStmt [expression]
  {:type :expression
   :value {:expression expression}})

(defn ->PrintStmt [expression]
  {:type :print
   :value {:expression expression}})
