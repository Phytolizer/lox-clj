(ns lox.core
  (:gen-class)
  (:require [lox.scanner :refer [->Scanner scan-tokens]]))

(def new-state
  {:had-error false})

(defmacro member [name]
  `(fn [x#] (~name x#)))

(defn run [source state]
  (let [scanner (->Scanner source)
        [tokens state] (scan-tokens scanner state)]
    (dorun (map (comp println (member .toString)) tokens))
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
