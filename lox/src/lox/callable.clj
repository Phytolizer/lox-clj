(ns lox.callable)

(defn ->Callable [arity call]
  {:arity arity
   :call call
   :callable true
   :to-string (fn [_self] "<native fn>")})
