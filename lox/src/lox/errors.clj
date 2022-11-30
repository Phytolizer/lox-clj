(ns lox.errors)

(defn- report [line where message state]
  (binding [*out* *err*]
    (println (str "[line " line "] Error" where ": " message)))
  (assoc state :had-error true))

(defn error [line message state]
  (report line "" message state))
