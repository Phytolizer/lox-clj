(ns lox.scanner
  (:refer-clojure :exclude [peek])
  (:require [lox.errors :refer [error]]
            [lox.helpers :refer [member]]
            [lox.token :refer [->Token]]))

(defn ->Scanner [source]
  {:source source
   :tokens []
   :start 0
   :current 0
   :line 1})

(defn- is-at-end [scanner]
  (>= (:current scanner) (count (:source scanner))))

(defn- peek
  ([scanner] (peek scanner 0))
  ([scanner n]
   (let [i (+ (:current scanner) n)]
     (if (>= i (count (:source scanner)))
       \u0000
       (nth (:source scanner) i)))))

(defn- peek-next [scanner] (peek scanner 1))

(defn- advance [scanner]
  (let [c (peek scanner)]
    (list (assoc scanner
                 :current (inc (:current scanner)))
          c)))

(defn- match [scanner expected]
  (if (and
       (not (is-at-end scanner))
       (= (peek scanner) expected))
    (list (first (advance scanner)) true)
    (list scanner false)))

(defn- token-text [scanner]
  (subs (:source scanner) (:start scanner) (:current scanner)))

(defn- add-token
  ([scanner type] (add-token scanner type nil))
  ([scanner type literal]
   (let [text (token-text scanner)
         token (->Token type text literal (:line scanner))]
     (assoc scanner
            :tokens (conj (:tokens scanner) token)))))

(defn- string [scanner state]
  (letfn [(skip [scanner]
            (if (or (= (peek scanner) \") (is-at-end scanner))
              scanner
              (let [scanner (if (= (peek scanner) \newline)
                              (assoc scanner :line (inc (:line scanner)))
                              scanner)
                    scanner (first (advance scanner))]
                (recur scanner))))]
    (let [scanner (skip scanner)]
      (if (is-at-end scanner)
        (list scanner
              (error (:line scanner) "Unterminated string." state))
        (let [scanner (first (advance scanner))
              value (subs (:source scanner)
                          (inc (:start scanner))
                          (dec (:current scanner)))
              scanner (add-token scanner :string value)]
          (list scanner state))))))

(defn- number [scanner state]
  (letfn [(skip [scanner]
            (if (-> scanner peek ((member Character/isDigit)))
              (-> scanner advance first recur)
              scanner))]
    (let [scanner (skip scanner)
          ; Look for a fractional part.
          scanner (if (and
                       (-> scanner peek (= \.))
                       (-> scanner peek-next ((member Character/isDigit))))
                    (-> scanner advance first skip)
                    scanner)]
      (list
       (add-token scanner :number (Double/parseDouble (token-text scanner)))
       state))))

(defn- check-token [c scanner state]
  (letfn [(ok [scanner]
            (list scanner state))
          (err [message scanner state]
            (list scanner (error (:line scanner) message state)))]
    (case c
      \( (ok (add-token scanner :left-paren))
      \) (ok (add-token scanner :right-paren))
      \{ (ok (add-token scanner :left-brace))
      \} (ok (add-token scanner :right-brace))
      \, (ok (add-token scanner :comma))
      \. (ok (add-token scanner :dot))
      \- (ok (add-token scanner :minus))
      \+ (ok (add-token scanner :plus))
      \; (ok (add-token scanner :semicolon))
      \* (ok (add-token scanner :star))
      \! (let [[scanner matched] (match scanner \=)]
           (ok (add-token scanner (if matched :bang-equal :bang))))
      \= (let [[scanner matched] (match scanner \=)]
           (ok (add-token scanner (if matched :equal-equal :equal))))
      \< (let [[scanner matched] (match scanner \=)]
           (ok (add-token scanner (if matched :less-equal :less))))
      \> (let [[scanner matched] (match scanner \=)]
           (ok (add-token scanner (if matched :greater-equal :greater))))
      \/ (let [[scanner matched] (match scanner \/)]
           (if matched
             (letfn [(next [scanner]
                       (if (or
                            (= (peek scanner) \newline)
                            (is-at-end scanner))
                         scanner
                         (recur (first (advance scanner)))))]
               (ok (next scanner)))
             (ok (add-token scanner :slash))))
      (\  \return \tab) (ok scanner)
      \newline (ok (assoc scanner :line (inc (:line scanner))))
      \" (string scanner state)
      (if (Character/isDigit c)
        (number scanner state)
        (err "Unexpected character." scanner state)))))

(defn- scan-token [scanner state]
  (let [[scanner c] (advance scanner)
        [scanner state] (check-token c scanner state)]
    (list scanner state)))

(defn scan-tokens [scanner state]
  (if (is-at-end scanner)
    (let [tokens (conj (:tokens scanner)
                       (->Token :eof "" nil (:line scanner)))]
      (list tokens state))
    (let [scanner (assoc scanner :start (:current scanner))
          [scanner state] (scan-token scanner state)]
      (recur scanner state))))
