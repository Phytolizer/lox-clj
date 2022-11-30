(ns lox.environment
  (:refer-clojure :exclude [get]))

(defn ->Environment []
  {:values {}})

(defn define [env name value]
  (assoc-in env [:values name] value))

(defn get [env name]
  (let [entry (find (:values env) (.lexeme name))]
    (if entry
      (val entry)
      (throw (ex-info "runtime error"
                      {:token name
                       :message (str "Undefined variable '" (.lexeme name) "'.")})))))

(defn assign [env name value]
  (let [entry (find (:values env) (.lexeme name))]
    (if entry
      (assoc-in env [:values (.lexeme name)] value)
      (throw (ex-info "runtime error"
                      {:token name
                       :message (str "Undefined variable '" (.lexeme name) "'.")})))))
