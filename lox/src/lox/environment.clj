(ns lox.environment
  (:refer-clojure :exclude [get]))

(defn ->Environment
  ([] (->Environment nil))
  ([enclosing] {:values {}
                :enclosing enclosing}))

(defn define [env name value]
  (assoc-in env [:values name] value))

(defn get [env name]
  (let [entry (find (:values env) (.lexeme name))]
    (cond
      entry (val entry)
      (:enclosing env) (get (:enclosing env) name)
      :else (throw (ex-info "runtime error"
                            {:token name
                             :message (str "Undefined variable '" (.lexeme name) "'.")})))))

(defn assign [env name value]
  (let [entry (find (:values env) (.lexeme name))]
    (cond
      entry (assoc-in env [:values (.lexeme name)] value)
      (:enclosing env) (assign (:enclosing env) name value)
      :else (throw (ex-info "runtime error"
                            {:token name
                             :message (str "Undefined variable '" (.lexeme name) "'.")})))))
