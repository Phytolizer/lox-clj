(ns lox.parser
  (:refer-clojure :exclude (peek))
  (:require [lox.errors]
            [lox.expr :refer [->BinaryExpr ->GroupingExpr ->LiteralExpr
                              ->UnaryExpr]]
            [lox.helpers :refer [try-any]]))

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
  (if (empty? pairs)
    (throw (error parser state "Expect expression."))
    (let [[types result-if-match] (first pairs)
          [parser matched] (match parser types)]
      (if matched
        (result-if-match parser state)
        (recur parser state (rest pairs))))))

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
                                           (list parser state (->GroupingExpr expr))))]))
          (unary [parser state]
            (let [[parser matched] (match parser '(:bang :minus))]
              (if matched
                (let [operator (previous parser)
                      right (unary parser state)
                      expr (->UnaryExpr operator right)]
                  (list parser state expr))
                (primary parser state))))
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
            (equality parser state))]
    (try
      (expression parser state)
      (catch clojure.lang.ExceptionInfo e
        (let [{:keys [state]} (ex-data e)]
          (list parser state nil))))))
