(ns lox.interpreter
  (:require [lox.environment :refer [->Environment]]
            lox.errors
            [lox.helpers :refer [accept]]))

(defn- truthy? [value]
  (not (contains? #{nil false} value)))

(defn- runtime-error [token message]
  (ex-info "runtime error" {:token token
                            :message message}))

(defn- check-number-operand [operator operand]
  (if (number? operand)
    operand
    (throw (runtime-error operator "Operand must be a number."))))

(defn- check-number-operands [operator left right]
  (if (and (number? left) (number? right))
    [left right]
    (throw (runtime-error operator "Operands must be numbers."))))

(def ^:private execute
  (letfn [(evaluate [self expr]
            (accept expr
                    {:assign visit-assign-expr
                     :binary visit-binary-expr
                     :grouping visit-grouping-expr
                     :literal visit-literal-expr
                     :unary visit-unary-expr
                     :variable visit-variable-expr}
                    self))
          (execute [self stmt]
            (accept stmt
                    {:expression visit-expression-stmt
                     :print visit-print-stmt
                     :var visit-var-stmt}
                    self))
          (visit-expression-stmt [self stmt]
            (evaluate self (:expression stmt)))
          (visit-print-stmt [self stmt]
            (let [[self value] (evaluate self (:expression stmt))]
              (println value)
              (list self nil)))
          (visit-var-stmt [self stmt]
            (let [[self value] (if (:initializer stmt)
                                 (evaluate self (:initializer stmt))
                                 (list self nil))
                  environment (lox.environment/define (:environment self)
                                (.lexeme (:name stmt))
                                value)]
              (list (assoc self :environment environment) nil)))
          (visit-assign-expr [self expr]
            (let [[self value] (evaluate self (:value expr))
                  environment  (lox.environment/assign (:environment self)
                                                       (:name expr)
                                                       value)]
              (list (assoc self :environment environment) value)))
          (visit-literal-expr [self expr]
            (list self (:value expr)))
          (visit-grouping-expr [self expr]
            (evaluate self (:expression expr)))
          (visit-unary-expr [self expr]
            (let [[self right] (evaluate self (:right expr))
                  value (case (.type (:operator expr))
                          :minus (- (check-number-operand (:operator expr) right))
                          :bang (not (truthy? right)))]
              (list self value)))
          (visit-variable-expr [self expr]
            (let [value (lox.environment/get (:environment self) (:name expr))]
              (list self value)))
          (visit-binary-expr [self expr]
            (let [[self left] (evaluate self (:left expr))
                  [self right] (evaluate self (:right expr))
                  value (case (.type (:operator expr))
                          :greater (apply > (check-number-operands (:operator expr) left right))
                          :greater-equal (apply >= (check-number-operands (:operator expr) left right))
                          :less (apply < (check-number-operands (:operator expr) left right))
                          :less-equal (apply <= (check-number-operands (:operator expr) left right))
                          :bang-equal (not= left right)
                          :equal-equal (= left right)
                          :minus (apply - (check-number-operands (:operator expr) left right))
                          :slash (apply / (check-number-operands (:operator expr) left right))
                          :star (apply * (check-number-operands (:operator expr) left right))
                          :plus (cond
                                  (and (number? left) (number? right)) (+ left right)
                                  (and (string? left) (string? right)) (str left right)
                                  :else (throw (runtime-error (:operator expr) "Operands must be two numbers or two strings."))))]
              (list self value)))]
    execute))

(defn- stringify [value]
  (cond
    (nil? value) "nil"
    (number? value) (let [text (str value)]
                      (if (.endsWith text ".0")
                        (subs text 0 (- (count text) 2))
                        text))
    :else (.toString value)))

(defn interpret [stmts state interpreter]
  (try
    (letfn [(run [self stmts]
              (if (empty? stmts)
                self
                (let [[self _] (execute self (first stmts))]
                  (recur self (rest stmts)))))]
      (list (run interpreter stmts) state))
    (catch clojure.lang.ExceptionInfo e
      (list interpreter (lox.errors/runtime-error e state)))))

(defn ->Interpreter []
  {:environment (->Environment)})
