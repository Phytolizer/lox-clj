(ns lox.token)

(deftype Token [type lexeme literal line]
  Object
  (toString [self]
    (str (.type self) " " (.lexeme self) " " (.literal self))))
