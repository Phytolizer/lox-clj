(ns lox.parser
  (:refer-clojure :exclude (peek))
  (:require [lox.errors]
            [lox.expr :refer [->BinaryExpr ->GroupingExpr ->LiteralExpr
                              ->UnaryExpr]]
            [lox.helpers :refer [try-any]]
            [lox.stmt :refer [->ExpressionStmt ->PrintStmt]]))

(defn ->Parser [tokens]
  {:tokens tokens
   :current 0})

(defn- peek [parser]
  (nth (:tokens parser) (:current parser)))

(defn- previous [parser]
  (nth (:tokens parser) (dec (:current parser))))

(defn- is-at-end [parser]
  (= (.type (peek parser)) :eof))

(defn- advance [parser]
  (if (is-at-end parser)
    parser
    (assoc parser :current (inc (:current parser)))))

(defn- check [parser type]
  (and (not (is-at-end parser))
       (= (.type (peek parser)) type)))

(defn- match [parser types]
  (if (try-any #(check parser %) types)
    (list (advance parser) true)
    (list parser false)))

(defn- binary-parser-loop [f types [parser state expr]]
  (let [[parser matched] (match parser types)]
    (if matched
      (let [operator (previous parser)
            [parser state right] (f parser state)
            expr (->BinaryExpr expr operator right)]
        (recur f types [parser state expr]))
      (list parser state expr))))

(defn- error [parser state message]
  (let [state (lox.errors/error-token (peek parser) message state)]
    (ex-info "parser error" {:state state})))

(defn- consume [parser state type message]
  (if (check parser type)
    (advance parser)
    (throw (error parser state message))))

(defn- synchronize [parser]
  (letfn [(loop [parser]
            (if (or (is-at-end parser)
                    (= (.type (previous parser)) :semicolon)
                    (contains?
                     #{:class
                       :fun
                       :var
                       :for
                       :if
                       :while
                       :print
                       :return}
                     (.type (peek parser))))
              nil
              (recur (advance parser))))]
    (loop (advance parser))))

(defn- try-match [parser state & pairs]
  (let [[types result-if-match] (first pairs)]
    (if (empty? types)
      (result-if-match parser state)
      (let [[parser matched] (match parser types)]
        (if matched
          (result-if-match parser state)
          (recur parser state (rest pairs)))))))

(defn parse [parser state]
  (letfn [(primary [parser state]
            (try-match parser state
                       ['(:false) #(list %1 %2 (->LiteralExpr false))]
                       ['(:true) #(list %1 %2 (->LiteralExpr true))]
                       ['(:nil) #(list %1 %2 (->LiteralExpr nil))]
                       ['(:number :string) #(list %1 %2 (->LiteralExpr (.literal (previous %1))))]
                       ['(:left-paren) (fn [parser state]
                                         (let [[parser state expr] (expression parser state)]
                                           (consume parser state :right-paren "Expect ')' after expression.")
                                           (list parser state (->GroupingExpr expr))))]
                       ['() #(throw (error %1 %2 "Expect expression."))]))
          (unary [parser state]
            (try-match parser state
                       ['(:bang :minus) (fn [parser state]
                                          (let [operator (previous parser)
                                                [parser state right] (unary parser state)]
                                            (list parser state (->UnaryExpr operator right))))]
                       ['() #(primary %1 %2)]))
          (factor [parser state]
            (binary-parser-loop unary
                                '(:slash :star)
                                (unary parser state)))
          (term [parser state]
            (binary-parser-loop factor
                                '(:minus :plus)
                                (factor parser state)))
          (comparison [parser state]
            (binary-parser-loop term
                                '(:greater :greater-equal :less :less-equal)
                                (term parser state)))
          (equality [parser state]
            (binary-parser-loop comparison
                                '(:bang-equal :equal-equal)
                                (comparison parser state)))
          (expression [parser state]
            (equality parser state))
          (print-statement [parser state]
            (let [[parser state value] (expression parser state)
                  parser (consume parser state :semicolon "Expect ';' after value.")]
              (list parser state (->PrintStmt value))))
          (expression-statement [parser state]
            (let [[parser state expr] (expression parser state)
                  parser (consume parser state :semicolon "Expect ';' after expression.")]
              (list parser state (->ExpressionStmt expr))))
          (statement [parser state]
            (try-match parser state
                       ['(:print) #(print-statement %1 %2)]
                       ['() #(expression-statement %1 %2)]))]
    (letfn [(stmt-loop [parser state statements]
              (if (is-at-end parser)
                (list parser state statements)
                (let [[parser state s] (statement parser state)]
                  (recur parser state (conj statements s)))))]
      (stmt-loop parser state []))))
