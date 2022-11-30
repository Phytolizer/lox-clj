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
      (throw (ex-info "runtime error" {:token name
                                       :message (str "Undefined variable '" (.lexeme name) "'.")})))))
