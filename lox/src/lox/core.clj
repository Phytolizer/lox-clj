(ns lox.core
  (:gen-class)
  (:require lox.ast-printer
            [lox.parser :refer [->Parser parse]]
            [lox.scanner :refer [->Scanner scan-tokens]]))

(def new-state
  {:had-error false})

(defn run [source state]
  (let [scanner (->Scanner source)
        [tokens state] (scan-tokens scanner state)
        [_ state expr] (parse (->Parser tokens) state)]
    (when (not (:had-error state))
      (println (lox.ast-printer/print expr)))
    state))

(defn run-prompt []
  (print "lox> ")
  (flush)
  (let [source (read-line)]
    (if (= source nil)
      (println ".q")
      (do
        (run source new-state)
        (recur)))))

(defn run-file [file]
  (let [source (slurp file)
        state (run source new-state)]
    (when (:had-error state)
      (System/exit 65))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (case (count args)
    0 (run-prompt)
    1 (run-file (first args))
    (do
      (println "Usage: lox [script]")
      (System/exit 64))))
