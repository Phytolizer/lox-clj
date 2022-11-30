(ns lox.stmt)

(defn ->BlockStmt [statements]
  {:type :block
   :value {:statements statements}})

(defn ->ExpressionStmt [expression]
  {:type :expression
   :value {:expression expression}})

(defn ->IfStmt [condition then-branch else-branch]
  {:type :if
   :value {:condition condition
           :then-branch then-branch
           :else-branch else-branch}})

(defn ->PrintStmt [expression]
  {:type :print
   :value {:expression expression}})

(defn ->VarStmt [name initializer]
  {:type :var
   :value {:name name
           :initializer initializer}})
