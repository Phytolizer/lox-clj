(ns lox.core
  (:gen-class))

(defn run [source])

(defn run-prompt []
  (print "lox> ")
  (flush)
  (let [source (read-line)]
    (if (= source nil)
      (println ".q")
      (do
        (run source)
        (recur)))))

(defn run-file [file]
  (-> file slurp run))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (case (count args)
    0 (run-prompt)
    1 (run-file (first args))
    (println "Usage: lox [script]")))
