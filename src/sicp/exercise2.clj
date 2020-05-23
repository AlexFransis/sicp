(ns sicp.exercise2)


(defn my-cons
  [x y]
  (let [dispatch (fn [m]
                   (condp = m
                     0 x
                     1 y
                     (str "Argument error not 0 or 1: my-cons: " m)))]
    dispatch))

(defn car
  [z]
  (z 0))

(defn cdr
  [z]
  (z 1))


;; Exercise 2.4
(defn my-cons2
  "Returns a function that takes a selector function
  to access either the first value or second value"
  [x y]
  (fn [selector-fn] (selector-fn x y)))

(defn car
  "Returns the first value of a cons cell"
  [cons-cell]
  (cons-cell (fn [p _] p)))

(defn cdr
  "Returns the second value of a cons cell"
  [cons-cell]
  (cons-cell (fn [_ q] q)))


(def ninenine (my-cons2 9 8))

(car ninenine)

(cdr ninenine)


;; Exercise 2.7
(defn make-interval
  [a b]
  (my-cons2 a b))

(defn upper-bound
  [interval]
  (max (car interval) (cdr interval)))

(defn lower-bound
  [interval]
  (min (car interval) (cdr interval)))

(def i (make-interval 3 2))

(defn list-ref
  [items i]
  (if (= i 0)
    (first items)
    (recur (rest items) (dec i))))

(defn length
  [items]
  (let [length-iter (fn [coll r]
                   (if (not (seq coll))
                     r
                     (recur (rest coll) (inc r))))]
    (length-iter items 0)))

(defn append
  [coll1 coll2]
  (letfn [(append-iter [c1 c2 result]
            (if (not (seq c1))
              (concat result c2)
              (recur (rest c1)
                     c2
                     (cons (first c1) result))))]
    (append-iter coll1 coll2 '())))

;; Exercise 2.17
(defn last-pair
  [coll]
  (if (= (length coll) 1)
    coll
    (recur (rest coll))))

(defn my-reverse
  [coll]
  (let [coll-iter (fn [c result]
                    (if (not (seq c))
                      result
                      (recur (rest c) (conj result (first c)))))]
    (coll-iter coll '())))

;; Exercise 2.20
(defn same-parity
  [& args]
  (let [parity-fn (fn [n] (mod n 2))
        parity (parity-fn (first args))
        iter (fn [coll r]
               (if (empty? coll)
                 r
                 (recur (rest coll) (if (= (parity-fn (first coll))
                                           parity)
                                      (conj r (first coll))
                                      r))))]
    (my-reverse (iter args '()))))
