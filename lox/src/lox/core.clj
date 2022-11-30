(ns lox.core
  (:gen-class))

(defn run-prompt []
  (println "lox> "))

(defn run-file [file]
  (let [source (slurp file)]
    (println source)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (case (count args)
    0 (run-prompt)
    1 (run-file (first args))
    (println "Usage: lox [script]")))
