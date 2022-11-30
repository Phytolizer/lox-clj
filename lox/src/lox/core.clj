(ns lox.core
  (:gen-class)
  (:require lox.ast-printer
            [lox.interpreter :refer [->Interpreter interpret]]
            [lox.parser :refer [->Parser parse]]
            [lox.scanner :refer [->Scanner scan-tokens]]))

(def new-state
  {:had-error false
   :had-runtime-error false})

(defn run [source interpreter state]
  (let [scanner (->Scanner source)
        [tokens state] (scan-tokens scanner state)
        [_ state expr] (parse (->Parser tokens) state)]
    (if (:had-error state)
      (list interpreter state)
      (interpret expr state interpreter))))

(defn run-prompt [interpreter]
  (print "lox> ")
  (flush)
  (let [source (read-line)]
    (if (= source nil)
      (println ".q")
      (let [[interpreter _] (run source interpreter new-state)]
        (recur interpreter)))))

(defn run-file [file]
  (let [source (slurp file)
        state (run source (->Interpreter) new-state)]
    (when (:had-error state)
      (System/exit 65))
    (when (:had-runtime-error state)
      (System/exit 70))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (case (count args)
    0 (run-prompt (->Interpreter))
    1 (run-file (first args))
    (do
      (println "Usage: lox [script]")
      (System/exit 64))))
