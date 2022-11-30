(ns lox.errors)

(defn- report [line where message state]
  (binding [*out* *err*]
    (println (str "[line " line "] Error" where ": " message)))
  (assoc state :had-error true))

(defn error [line message state]
  (report line "" message state))

(defn error-token [token message state]
  (case (.type token)
    :eof (report (.line token) " at end" message state)
    (report (.line token) (str " at '" (.lexeme token) "'") message state)))

(defn runtime-error [error state]
  (let [{:keys [token message]} (ex-data error)]
    (binding [*out* *err*]
      (println message)
      (println (str "[line " (.line token) "]")))
    (assoc state :had-runtime-error true)))
