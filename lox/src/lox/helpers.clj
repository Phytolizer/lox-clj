(ns lox.helpers)

(defmacro member [name]
  `(fn [x#] (~name x#)))

(defmacro accept [obj fns visitor]
  `(((:type ~obj) ~fns) ~visitor (:value ~obj)))
