(ns lox.helpers)

(defmacro member [name]
  `(fn [x#] (~name x#)))

(defmacro accept [obj fns visitor]
  `(((:type ~obj) ~fns) ~visitor (:value ~obj)))

(defn try-any [f values]
  (if (empty? values)
    false
    (if (f (first values))
      true
      (try-any f (rest values)))))
