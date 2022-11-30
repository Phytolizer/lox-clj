(ns lox.helpers)

(defmacro member [name]
  `(fn [x#] (~name x#)))
