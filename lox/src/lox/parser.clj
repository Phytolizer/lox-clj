(ns lox.parser
  (:refer-clojure :exclude (peek))
  (:require [lox.errors]
            [lox.expr :refer [->AssignExpr ->BinaryExpr ->GroupingExpr
                              ->LiteralExpr ->LogicalExpr ->UnaryExpr ->VariableExpr]]
            [lox.helpers :refer [try-any]]
            [lox.stmt :refer [->BlockStmt ->ExpressionStmt ->IfStmt
                              ->PrintStmt ->VarStmt ->WhileStmt]]))

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

(defn- binary-parser-loop [f types [parser state expr] result-fn]
  (let [[parser matched] (match parser types)]
    (if matched
      (let [operator (previous parser)
            [parser state right] (f parser state)
            expr (result-fn expr operator right)]
        (recur f types [parser state expr] result-fn))
      (list parser state expr))))

(defn- error
  ([parser state message] (error parser state (peek parser) message))
  ([_ state token message]
   (let [state (lox.errors/error-token token message state)]
     (ex-info "parser error" {:state state}))))

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
              parser
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
                       ['(:identifier) #(list %1 %2 (->VariableExpr (previous %1)))]
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
                                (unary parser state)
                                ->BinaryExpr))
          (term [parser state]
            (binary-parser-loop factor
                                '(:minus :plus)
                                (factor parser state)
                                ->BinaryExpr))
          (comparison [parser state]
            (binary-parser-loop term
                                '(:greater :greater-equal :less :less-equal)
                                (term parser state)
                                ->BinaryExpr))
          (equality [parser state]
            (binary-parser-loop comparison
                                '(:bang-equal :equal-equal)
                                (comparison parser state)
                                ->BinaryExpr))
          (and-expr [parser state]
            (binary-parser-loop equality
                                '(:and)
                                (equality parser state)
                                ->LogicalExpr))
          (or-expr [parser state]
            (binary-parser-loop and-expr
                                '(:or)
                                (and-expr parser state)
                                ->LogicalExpr))
          (assignment [parser state]
            (let [[parser state expr] (or-expr parser state)
                  [parser matched] (match parser '(:equal))]
              (if matched
                (let [equals (previous parser)
                      [parser state value] (assignment parser state)]
                  (if (= (:type expr) :variable)
                    (list parser state (->AssignExpr (:name (:value expr)) value))
                    (throw (error parser state equals "Invalid assignment target."))))
                (list parser state expr))))
          (expression [parser state]
            (assignment parser state))
          (print-statement [parser state]
            (let [[parser state value] (expression parser state)
                  parser (consume parser state :semicolon "Expect ';' after value.")]
              (list parser state (->PrintStmt value))))
          (expression-statement [parser state]
            (let [[parser state expr] (expression parser state)
                  parser (consume parser state :semicolon "Expect ';' after expression.")]
              (list parser state (->ExpressionStmt expr))))
          (block-statement [parser state]
            (letfn [(stmt-loop [parser state stmts]
                      (if (or (is-at-end parser) (check parser :right-brace))
                        (list parser state stmts)
                        (let [[parser state stmt] (declaration parser state)]
                          (recur parser state (conj stmts stmt)))))]
              (let [[parser state stmts] (stmt-loop parser state [])
                    parser (consume parser state :right-brace "Expect '}' after block.")]
                (list parser state (->BlockStmt stmts)))))
          (if-statement [parser state]
            (let [parser (consume parser state :left-paren "Expect '(' after 'if'.")
                  [parser state condition] (expression parser state)
                  parser (consume parser state :right-paren "Expect ')' after if condition.")
                  [parser state then-branch] (statement parser state)
                  [parser state else-branch] (let [[parser matched] (match parser '(:else))]
                                               (if matched
                                                 (statement parser state)
                                                 (list parser state nil)))]
              (list parser state (->IfStmt condition then-branch else-branch))))
          (while-statement [parser state]
            (let [parser (consume parser state :left-paren "Expect '(' after 'while'.")
                  [parser state condition] (expression parser state)
                  parser (consume parser state :right-paren "Expect ')' after while condition.")
                  [parser state body] (statement parser state)]
              (list parser state (->WhileStmt condition body))))
          (for-statement [parser state]
            (let [parser (consume parser state :left-paren "Expect '(' after 'for'.")
                  [parser state initializer] (try-match parser state
                                                        ['(:semicolon) #(list %1 %2 nil)]
                                                        ['(:var) (fn [parser state]
                                                                   (let [[parser state var-decl] (var-declaration parser state)]
                                                                     (list parser state var-decl)))]
                                                        ['() (fn [parser state]
                                                               (let [[parser state expr] (expression-statement parser state)]
                                                                 (list parser state expr)))])
                  [parser state condition] (let [[parser state expr] (if (check parser :semicolon)
                                                                       (list parser state nil)
                                                                       (expression parser state))
                                                 parser (consume parser state :semicolon "Expect ';' after loop condition.")]
                                             (list parser state expr))
                  [parser state increment] (let [[parser state expr] (if (check parser :right-paren)
                                                                       (list parser state nil)
                                                                       (expression parser state))
                                                 parser (consume parser state :right-paren "Expect ')' after for clauses.")]
                                             (list parser state expr))
                  [parser state body] (statement parser state)
                  body (if increment
                         (->BlockStmt [body (->ExpressionStmt increment)])
                         body)
                  body (->WhileStmt
                        (or condition (->LiteralExpr true))
                        body)
                  body (if initializer
                         (->BlockStmt [initializer body])
                         body)]
              (list parser state body)))
          (statement [parser state]
            (try-match parser state
                       ['(:for) #(for-statement %1 %2)]
                       ['(:if) #(if-statement %1 %2)]
                       ['(:print) #(print-statement %1 %2)]
                       ['(:while) #(while-statement %1 %2)]
                       ['(:left-brace) #(block-statement %1 %2)]
                       ['() #(expression-statement %1 %2)]))
          (var-declaration [parser state]
            (let [parser (consume parser state :identifier "Expect variable name.")
                  name (previous parser)
                  [parser state initializer] (try-match parser state
                                                        ['(:equal) #(expression %1 %2)]
                                                        ['() #(list %1 %2 nil)])
                  parser (consume parser state :semicolon "Expect ';' after variable declaration.")]
              (list parser state (->VarStmt name initializer))))
          (declaration [parser state]
            (try
              (try-match parser state
                         ['(:var) #(var-declaration %1 %2)]
                         ['() #(statement %1 %2)])
              (catch clojure.lang.ExceptionInfo e
                (list (synchronize parser) (:state (ex-data e)) nil))))]
    (letfn [(stmt-loop [parser state statements]
              (if (is-at-end parser)
                (list parser state statements)
                (let [[parser state s] (declaration parser state)]
                  (recur parser state (conj statements s)))))]
      (let [[_ state stmts] (stmt-loop parser state [])]
        (list state stmts)))))
