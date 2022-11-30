(ns lox.expr)

(defn ->AssignExpr [name value]
  {:type :assign
   :value {:name name
           :value value}})

(defn ->BinaryExpr [left operator right]
  {:type :binary
   :value {:left left
           :operator operator
           :right right}})

(defn ->GroupingExpr [expression]
  {:type :grouping
   :value {:expression expression}})

(defn ->LiteralExpr [value]
  {:type :literal
   :value {:value value}})

(defn ->LogicalExpr [left operator right]
  {:type :logical
   :value {:left left
           :operator operator
           :right right}})

(defn ->UnaryExpr [operator right]
  {:type :unary
   :value {:operator operator
           :right right}})

(defn ->VariableExpr [name]
  {:type :variable
   :value {:name name}})
